library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use IEEE.std_logic_misc.all;

library UNISIM;
use UNISIM.VComponents.all;

library mylib;
use mylib.defToplevel.all;
use mylib.defBusAddressMap.all;
use mylib.defBCT.all;
use mylib.defRBCP.all;
use mylib.defSiTCP.all;
use mylib.defMIG.all;
use mylib.defMiiRstTimer.all;

entity toplevel is
  Port (
-- System ---------------------------------------------------------------
    PROGB_ON            : out std_logic;
    BASE_CLKP           : in std_logic;
    BASE_CLKN           : in std_logic;
    USR_RSTB            : in std_logic;
    DIP                 : in std_logic_vector(4 downto 1);
    VP                  : in std_logic;
    VN                  : in std_logic;

-- GTX ------------------------------------------------------------------
    GTX_REFCLK_P        : in std_logic;
    GTX_REFCLK_N        : in std_logic;
    GTX_TX_P            : out std_logic;
    GTX_RX_P            : in std_logic;
    GTX_TX_N            : out std_logic;
    GTX_RX_N            : in std_logic;

-- SPI flash ------------------------------------------------------------
    MOSI                : out std_logic;
    DIN                 : in std_logic;
    FCSB                : out std_logic;

-- EEPROM ---------------------------------------------------------------
    EEP_CS              : out std_logic;
    EEP_SK              : out std_logic;
    EEP_DI              : out std_logic;
    EEP_DO              : in std_logic;

-- MPPC-BIAS ------------------------------------------------------------
    BIAS_CSB            : out std_logic;
    BIAS_SCLK           : out std_logic;
    BIAS_DIN            : out std_logic;
    BIAS_CLB            : in std_logic;

-- NIM-IO ---------------------------------------------------------------
    NIM_IN              : in std_logic_vector(2 downto 1);
    NIM_OUT             : out std_logic_vector(2 downto 1);

-- JItter cleaner -------------------------------------------------------
    CDCE_PDB            : out std_logic;
    CDCE_LOCK           : in std_logic;
    CDCE_SCLK           : out std_logic;
    CDCE_SO             : in std_logic;
    CDCE_SI             : out std_logic;
    CDCE_LE             : out std_logic;
    CDCE_REFP           : out std_logic;
    CDCE_REFN           : out std_logic;
    MIGS_CLKP           : in std_logic;
    MIGS_CLKN           : in std_logic;
--    TDC_CLKP            : in std_logic;
--    TDC_CLKN            : in std_logic;

-- Test pins ------------------------------------------------------------
    TESTPIN             : out std_logic_vector(4 downto 1);

-- CITIROC pins ---------------------------------------------------------
    CI_PWRON            : out std_logic;
    CI_RSTB_PA          : out std_logic;
    CI_RSTB_SR          : out std_logic;
    CI_RSTB_READ        : out std_logic;
    CI_RSTB_PSC         : out std_logic;

    CI_SRIN             : out std_logic;
    CI_READIN           : out std_logic;
    CI_CLK_SR           : out std_logic;
    CI_CLK_READ         : out std_logic;
    CI_LOAD_SC          : out std_logic;
    CI_SELECT           : out std_logic;
    CI_SROUT            : in std_logic;
    CI_READOUT          : in std_logic;

    CI_HOLD_HG          : out std_logic;
    CI_HOLD_LG          : out std_logic;
    CI_MODEB_EXT        : out std_logic;
    CI_PS_GTRIG         : out std_logic;
    CI_VEVT             : out std_logic;
    CI_RAZCHN           : out std_logic;

    PMUX_A              : out std_logic_vector(1 downto 0);

-- CITIROC trigger ------------------------------------------------------
    CI_TRIGB            : in std_logic_vector(128 downto 1);

-- DDR3 SDRAM -----------------------------------------------------------
    DDR3_CK_P           : out std_logic_vector(0 downto 0);
    DDR3_CK_N           : out std_logic_vector(0 downto 0);
    DDR3_RESET_N        : out std_logic;
    DDR3_CKE            : out std_logic_vector(0 downto 0);
    DDR3_CS_N           : out std_logic_vector(0 downto 0);
    DDR3_RAS_N          : out std_logic;
    DDR3_CAS_N          : out std_logic;
    DDR3_WE_N           : out std_logic;
    DDR3_ODT            : out std_logic_vector(0 downto 0);
    DDR3_BA             : out std_logic_vector(2 downto 0);
    DDR3_ADDR           : out std_logic_vector(13 downto 0);
    DDR3_DQ             : inout std_logic_vector(15 downto 0);
    DDR3_DM             : out std_logic_vector(1 downto 0);
    DDR3_DQS_P          : inout std_logic_vector(1 downto 0);
    DDR3_DQS_N          : inout std_logic_vector(1 downto 0);

    -- AD9220 ---------------------------------------------------------------
    ADC_OTR             : in std_logic;
    ADC_BIT            : in std_logic_vector(12 downto 1);
    ADC_CLK             : out std_logic
    );
end toplevel;

architecture Behavioral of toplevel is
  attribute mark_debug : string;

  -- System --------------------------------------------------------------------------------
  signal sitcp_reset  : std_logic;
  signal system_reset : std_logic;
  signal user_reset   : std_logic;

  signal mii_reset    : std_logic;
  signal emergency_reset  : std_logic_vector(kNumGtx-1 downto 0);

  signal bct_reset    : std_logic;
  signal rst_from_bus : std_logic;

  signal delayed_usr_rstb : std_logic;


  -- USER ----------------------------------------------------------------------------------

  -- DIP -----------------------------------------------------------------------------------
  signal dip_sw       : std_logic_vector(DIP'range);
  subtype DipID is integer range 0 to 4;
  type regLeaf is record
    Index : DipID;
  end record;
  constant kSiTCP     : regLeaf := (Index => 1);
  constant kLocalClk  : regLeaf := (Index => 2);
  constant kNC2       : regLeaf := (Index => 3);
  constant kNC3       : regLeaf := (Index => 4);
  constant kDummy     : regLeaf := (Index => 0);

  -- C6C ----------------------------------------------------------------------------------
  signal c6c_reset    : std_logic;

  -- MIG ----------------------------------------------------------------------------------
  signal dout_test_data   : std_logic_vector(kWidthDaqData-1 downto 0);
  signal din_test_data    : std_logic_vector(kWidthDaqData-1 downto 0);
  signal dvalid_test_data, rvalid_test_data   : std_logic;
  signal ren_read_buf, empty_read_buf   : std_logic;

  -- SDS ---------------------------------------------------------------------
  signal shutdown_over_temp     : std_logic;
  signal reg_temp               : std_logic_vector(11 downto 0);

  -- FMP ---------------------------------------------------------------------

  -- ASIC ----------------------------------------------------------------------------------
  signal reg_direct_control1     : std_logic_vector(7 downto 0);
  signal reg_direct_control2     : std_logic_vector(7 downto 0);

  signal reg_srout               : std_logic;
  signal reg_readout             : std_logic;

  signal read_clk_sr             : std_logic;
  signal read_in_sr              : std_logic;

  -- attribute mark_debug of reg_srout : signal is "true";
  -- attribute mark_debug of reg_readout : signal is "true";
  -- attribute mark_debug of reg_direct_control2 : signal is "true";

  -- AD9220 --------------------------------------------------------------------------------
  signal hold_out   : std_logic;
  signal start_adc  : std_logic;
  signal busy_adc   : std_logic;

  signal read_clk_adc : std_logic;
  signal read_in_adc  : std_logic;

  signal adc_clock_out  : std_logic;

  -- BCT -----------------------------------------------------------------------------------
  signal addr_LocalBus          : LocalAddressType;
  signal data_LocalBusIn        : LocalBusInType;
  signal data_LocalBusOut       : DataArray;
  signal re_LocalBus            : ControlRegArray;
  signal we_LocalBus            : ControlRegArray;
  signal ready_LocalBus         : ControlRegArray;

  -- TSD -----------------------------------------------------------------------------------
  signal wd_to_tsd                              : std_logic_vector(kWidthDataTCP-1 downto 0);
  signal we_to_tsd, empty_to_tsd, re_from_tsd   : std_logic;

  type typeTcpData is array(kNumGtx-1 downto 0) of std_logic_vector(kWidthDataTCP-1 downto 0);
  signal daq_data                          : typeTcpData;
  signal valid_data, empty_data, req_data  : std_logic_vector(kNumGtx-1 downto 0);

  -- SiTCP ---------------------------------------------------------------------------------
  type typeUdpAddr is array(kNumGtx-1 downto 0) of std_logic_vector(kWidthAddrRBCP-1 downto 0);
  type typeUdpData is array(kNumGtx-1 downto 0) of std_logic_vector(kWidthDataRBCP-1 downto 0);

  signal tcp_isActive, close_req, close_act    : std_logic_vector(kNumGtx-1 downto 0);
  -- signal reg_dummy0    : std_logic_vector(7 downto 0);
  -- signal reg_dummy1    : std_logic_vector(7 downto 0);
  -- signal reg_dummy2    : std_logic_vector(7 downto 0);
  -- signal reg_dummy3    : std_logic_vector(7 downto 0);

  signal tcp_tx_clk   : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_rx_wr    : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_rx_data  : typeTcpData;
  signal tcp_tx_full  : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_tx_wr    : std_logic_vector(kNumGtx-1 downto 0);
  signal tcp_tx_data  : typeTcpData;

  signal rbcp_act     : std_logic_vector(kNumGtx-1 downto 0);
  signal rbcp_addr    : typeUdpAddr;
  signal rbcp_wd      : typeUdpData;
  signal rbcp_we      : std_logic_vector(kNumGtx-1 downto 0); --: Write enable
  signal rbcp_re      : std_logic_vector(kNumGtx-1 downto 0); --: Read enable
  signal rbcp_ack     : std_logic_vector(kNumGtx-1 downto 0); -- : Access acknowledge
  signal rbcp_rd      : typeUdpData;

  signal rbcp_gmii_addr    : typeUdpAddr;
  signal rbcp_gmii_wd      : typeUdpData;
  signal rbcp_gmii_we      : std_logic_vector(kNumGtx-1 downto 0); --: Write enable
  signal rbcp_gmii_re      : std_logic_vector(kNumGtx-1 downto 0); --: Read enable
  signal rbcp_gmii_ack     : std_logic_vector(kNumGtx-1 downto 0); -- : Access acknowledge
  signal rbcp_gmii_rd      : typeUdpData;

  component WRAP_SiTCP_GMII_XC7K_32K
    port
      (
        CLK                   : in std_logic; --: System Clock >129MHz
        RST                   : in std_logic; --: System reset
        -- Configuration parameters
        FORCE_DEFAULTn        : in std_logic; --: Load default parameters
        EXT_IP_ADDR           : in std_logic_vector(31 downto 0); --: IP address[31:0]
        EXT_TCP_PORT          : in std_logic_vector(15 downto 0); --: TCP port #[15:0]
        EXT_RBCP_PORT         : in std_logic_vector(15 downto 0); --: RBCP port #[15:0]
        PHY_ADDR              : in std_logic_vector(4 downto 0);  --: PHY-device MIF address[4:0]

        -- EEPROM
        EEPROM_CS             : out std_logic; --: Chip select
        EEPROM_SK             : out std_logic; --: Serial data clock
        EEPROM_DI             : out    std_logic; --: Serial write data
        EEPROM_DO             : in std_logic; --: Serial read data
        --    user data, intialial values are stored in the EEPROM, 0xFFFF_FC3C-3F
        USR_REG_X3C           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3C
        USR_REG_X3D           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3D
        USR_REG_X3E           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3E
        USR_REG_X3F           : out    std_logic_vector(7 downto 0); --: Stored at 0xFFFF_FF3F
        -- MII interface
        GMII_RSTn             : out    std_logic; --: PHY reset
        GMII_1000M            : in std_logic;  --: GMII mode (0:MII, 1:GMII)
        -- TX
        GMII_TX_CLK           : in std_logic; -- : Tx clock
        GMII_TX_EN            : out    std_logic; --: Tx enable
        GMII_TXD              : out    std_logic_vector(7 downto 0); --: Tx data[7:0]
        GMII_TX_ER            : out    std_logic; --: TX error
        -- RX
        GMII_RX_CLK           : in std_logic; -- : Rx clock
        GMII_RX_DV            : in std_logic; -- : Rx data valid
        GMII_RXD              : in std_logic_vector(7 downto 0); -- : Rx data[7:0]
        GMII_RX_ER            : in std_logic; --: Rx error
        GMII_CRS              : in std_logic; --: Carrier sense
        GMII_COL              : in std_logic; --: Collision detected
        -- Management IF
        GMII_MDC              : out std_logic; --: Clock for MDIO
        GMII_MDIO_IN          : in std_logic; -- : Data
        GMII_MDIO_OUT         : out    std_logic; --: Data
        GMII_MDIO_OE          : out    std_logic; --: MDIO output enable
        -- User I/F
        SiTCP_RST             : out    std_logic; --: Reset for SiTCP and related circuits
        -- TCP connection control
        TCP_OPEN_REQ          : in std_logic; -- : Reserved input, shoud be 0
        TCP_OPEN_ACK          : out    std_logic; --: Acknowledge for open (=Socket busy)
        TCP_ERROR             : out    std_logic; --: TCP error, its active period is equal to MSL
        TCP_CLOSE_REQ         : out    std_logic; --: Connection close request
        TCP_CLOSE_ACK         : in std_logic ;-- : Acknowledge for closing
        -- FIFO I/F
        TCP_RX_WC             : in std_logic_vector(15 downto 0); --: Rx FIFO write count[15:0] (Unused bits should be set 1)
        TCP_RX_WR             : out    std_logic; --: Write enable
        TCP_RX_DATA           : out    std_logic_vector(7 downto 0); --: Write data[7:0]
        TCP_TX_FULL           : out    std_logic; --: Almost full flag
        TCP_TX_WR             : in std_logic; -- : Write enable
        TCP_TX_DATA           : in std_logic_vector(7 downto 0); -- : Write data[7:0]
        -- RBCP
        RBCP_ACT              : out std_logic; -- RBCP active
        RBCP_ADDR             : out    std_logic_vector(31 downto 0); --: Address[31:0]
        RBCP_WD               : out    std_logic_vector(7 downto 0); --: Data[7:0]
        RBCP_WE               : out    std_logic; --: Write enable
        RBCP_RE               : out    std_logic; --: Read enable
        RBCP_ACK              : in std_logic; -- : Access acknowledge
        RBCP_RD               : in std_logic_vector(7 downto 0 ) -- : Read data[7:0]
        );
  end component;

  -- SFP transceiver -----------------------------------------------------------------------
  constant kMiiPhyad      : std_logic_vector(kWidthPhyAddr-1 downto 0):= "00000";
  signal mii_init_mdc, mii_init_mdio : std_logic;

  component mii_initializer is
    port(
      -- System
      CLK         : in std_logic;
      --RST         => system_reset,
      RST         : in std_logic;
      -- PHY
      PHYAD       : in std_logic_vector(kWidthPhyAddr-1 downto 0);
      -- MII
      MDC         : out std_logic;
      MDIO_OUT    : out std_logic;
      -- status
      COMPLETE    : out std_logic
      );
  end component;

  signal mmcm_reset_all   : std_logic;
  signal mmcm_reset       : std_logic_vector(kNumGtx-1 downto 0);
  signal mmcm_locked      : std_logic;

  signal gt0_qplloutclk, gt0_qplloutrefclk  : std_logic;
  signal gtrefclk_i, gtrefclk_bufg  : std_logic;
  signal txout_clk, rxout_clk       : std_logic_vector(kNumGtx-1 downto 0);
  signal user_clk, user_clk2, rxuser_clk, rxuser_clk2   : std_logic;

  signal eth_tx_clk       : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_tx_en        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_tx_er        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_tx_d         : typeTcpData;

  signal eth_rx_clk       : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_rx_dv        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_rx_er        : std_logic_vector(kNumGtx-1 downto 0);
  signal eth_rx_d         : typeTcpData;


  -- Clock ---------------------------------------------------------------------------
  signal clk_gbe, clk_sys   : std_logic;
  signal clk_locked         : std_logic;
  signal clk_sys_locked     : std_logic;
  signal clk_spi            : std_logic;
  signal clk_machine        : std_logic;
  signal clk_machine_div2   : std_logic;
  signal gclk_mac_div2      : std_logic;

  signal clk_tdc            : std_logic;
  signal clk_slow           : std_logic;

  component clk_wiz_sys
    port
      (-- Clock in ports
        -- Clock out ports
        clk_sys          : out    std_logic;
        clk_indep_gtx    : out    std_logic;
        clk_spi          : out    std_logic;
        clk_machine      : out    std_logic;
--        clk_buf          : out    std_logic;
        -- Status and control signals
        reset            : in     std_logic;
        locked           : out    std_logic;
        clk_in1_p        : in     std_logic;
        clk_in1_n        : in     std_logic
        );
  end component;

  signal mmcm_cdcm_locked     : std_logic;
  signal mmcm_cdcm_reset      : std_logic;

  component clk_wiz_tdc
    port
     (-- Clock in ports
      -- Clock out ports
      clk_tdc           : out    std_logic;
      clk_slow          : out    std_logic;
      -- Status and control signals
      reset             : in     std_logic;
      locked            : out    std_logic;
      clk_in1           : in     std_logic
     );
    end component;

  -- debug -----------------------------------------------------------------------------

begin
  -- ===================================================================================
  -- body
  -- ===================================================================================
--  SPID2   <= 'Z';
--  SPID3   <= 'Z';

  -- Global ----------------------------------------------------------------------------
  u_DelayUsrRstb : entity mylib.DelayGen
    generic map(kNumDelay => 128)
    port map(clk_sys, USR_RSTB, delayed_usr_rstb);

  c6c_reset       <= (not clk_sys_locked) or (not delayed_usr_rstb);
  mmcm_cdcm_reset <= (not delayed_usr_rstb);

  clk_locked      <= clk_sys_locked and mmcm_cdcm_locked;
  system_reset    <= (not clk_locked) or (not USR_RSTB);

  user_reset      <= system_reset or rst_from_bus or emergency_reset(0);
  bct_reset       <= system_reset or emergency_reset(0);

  -- Input --
  --NIM_OUT(1) <= CI_TRIGB(1) when(dip_sw(kLocalClk.Index) = '0') else CI_TRIGB(33);
  NIM_OUT(2) <= hold_out;
  -- NIM_OUT(1) <= BIAS_CLB;
  -- NIM_OUT(1) <= reg_srout and reg_readout;
  -- NIM_OUT(1) <= '0';
  -- NIM_OUT(2) <=

  dip_sw(1)   <= DIP(1); -- SiTCP
  dip_sw(2)   <= DIP(2);
  dip_sw(3)   <= DIP(3);
  dip_sw(4)   <= DIP(4);


  -- TESTPIN(1)  <= dip_sw(1);
  -- TESTPIN(2)  <= dip_sw(2);
  -- TESTPIN(3)  <= dip_sw(3);
  -- TESTPIN(4)  <= dip_sw(4);

  TESTPIN(1) <= hold_out;
  TESTPIN(2) <= adc_clock_out;
  TESTPIN(3) <= reg_readout;
  TESTPIN(4) <= reg_srout;

  CI_RSTB_READ <= reg_direct_control1(0);
  CI_RSTB_SR   <= reg_direct_control1(1);
  CI_LOAD_SC   <= reg_direct_control1(2);
  CI_SELECT    <= reg_direct_control1(3);
  CI_PWRON     <= reg_direct_control1(4);
  CI_RSTB_PA   <= reg_direct_control1(5);
  CI_VEVT      <= reg_direct_control1(6);
  CI_RAZCHN    <= reg_direct_control1(7);

  CI_RSTB_PSC  <= reg_direct_control2(0);
  CI_MODEB_EXT <= reg_direct_control2(1);

  PMUX_A(1 downto 0) <= reg_direct_control2(4 downto 3);

  -- AMUX_A       <= "0";
  -- PMUX_A       <= "00";

  reg_srout    <= CI_SROUT;
  reg_readout  <= CI_READOUT;

  -- IBUFDS_inst1 : IBUFDS
  --  generic map (
  --     DIFF_TERM => FALSE, Differential Termination
  --     IBUF_LOW_PWR => TRUE, Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
  --     IOSTANDARD => "LVDS")
  --  port map (
  --     O => NIM_OUT(1),  Buffer output
  --     I  => MIGS_CLKP,  Diff_p buffer input (connect directly to top-level port)
  --     IB => MIGS_CLKN Diff_n buffer input (connect directly to top-level port)
  --     );

--  IBUFDS_inst2 : IBUFDS
--   generic map (
--      DIFF_TERM => FALSE, -- Differential Termination
--      IBUF_LOW_PWR => TRUE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
--      IOSTANDARD => "LVDS")
--   port map (
--      O => NIM_OUT(2),  -- Buffer output
--      I  => TDC_CLKP,  -- Diff_p buffer input (connect directly to top-level port)
--      IB => TDC_CLKN -- Diff_n buffer input (connect directly to top-level port)
--   );

  -- LED(3 downto 1)     <= out_led(3 downto 1);
  -- LED(4)     <= CDCE_LOCK;


  -- CITIROC ---------------------------------------------------------------------------
  CI_CLK_READ   <= read_clk_sr or read_clk_adc;
  CI_READIN     <= read_in_sr or read_in_adc;

  CI_HOLD_HG   <= hold_out;
  CI_HOLD_LG   <= hold_out;
  CI_PS_GTRIG  <= '0';

  u_CITIROC_Inst : entity mylib.CitirocController
    port map(
      rst               => user_reset,
      clk               => clk_slow,
      clk_machine       => gclk_mac_div2,
      -- Local bus --
      addrLocalBus      => addr_LocalBus,
      dataLocalBusIn    => data_LocalBusIn,
      dataLocalBusOut   => data_LocalBusOut(kASIC.ID),
      reLocalBus        => re_LocalBus(kASIC.ID),
      weLocalBus        => we_LocalBus(kASIC.ID),
      readyLocalBus     => ready_LocalBus(kASIC.ID),
      -- Module output --
      clk_sr            => CI_CLK_SR,
      srin_sr           => CI_SRIN,
      clk_read          => read_clk_sr,
      srin_read         => read_in_sr,
      direct_control1   => reg_direct_control1,
      direct_control2   => reg_direct_control2
    );

  -- MPPC-BIAS -------------------------------------------------------------------------
  u_BIAS_Inst : entity mylib.MAX1932Controller
    port map(
      rst	          => user_reset,
      clk	          => clk_slow,

      -- Module output --
      CSB_SPI           => BIAS_CSB,
      SCLK_SPI          => BIAS_SCLK,
      MOSI_SPI          => BIAS_DIN,

      -- Local bus --
      addrLocalBus	    => addr_LocalBus,
      dataLocalBusIn	  => data_LocalBusIn,
      dataLocalBusOut	  => data_LocalBusOut(kBIAS.ID),
      reLocalBus	      => re_LocalBus(kBIAS.ID),
      weLocalBus	      => we_LocalBus(kBIAS.ID),
      readyLocalBus	    => ready_LocalBus(kBIAS.ID)
    );

  -- AD9220 ---------------------------------------------------------------------------
  u_HoldGen : entity mylib.HoldGeneratorDigital
    port map(
      -- System --
      reset     => user_reset,
      clk       => clk_slow,
      clkFast   => clk_tdc,

      holdIn    => NIM_IN(2),
      holdOut   => hold_out,
      startAdc  => start_adc,
      busyAdc   => busy_adc,

      -- Local bus --
      addrLocalBus        => addr_LocalBus,
      dataLocalBusIn      => data_LocalBusIn,
      dataLocalBusOut     => data_LocalBusOut(kHGD.ID),
      reLocalBus          => re_LocalBus(kHGD.ID),
      weLocalBus          => we_LocalBus(kHGD.ID),
      readyLocalBus       => ready_LocalBus(kHGD.ID)
      );

  ADC_CLK   <= adc_clock_out;
  u_AD9220 : entity mylib.AD9220
    generic map(
      freqSysClk  => 125_000_000,
      freqBusClk  => 5_000_000,
      enDebug     => false
      )
    port map(
      -- System --
      clk       => clk_slow,
      reset     => user_reset,
      busyOut   => busy_adc,
      startIn   => start_adc,

      -- CITIROC --
      readClk   => read_clk_adc,
      readIn    => read_in_adc,

      -- AD9220 --
      adcClk    => adc_clock_out,
      adcBit    => ADC_BIT,
      adcOTR    => ADC_OTR,

      -- Local bus --
      addrLocalBus        => addr_LocalBus,
      dataLocalBusIn      => data_LocalBusIn,
      dataLocalBusOut     => data_LocalBusOut(kADC.ID),
      reLocalBus          => re_LocalBus(kADC.ID),
      weLocalBus          => we_LocalBus(kADC.ID),
      readyLocalBus       => ready_LocalBus(kADC.ID)
      );


  -- IOM -------------------------------------------------------------------------------
  u_IOM_Inst : entity mylib.IOManager
    generic map(
      kNumInput           => kNumInput
    )
    port map(
      rst	                => user_reset,
      clk	                => clk_slow,

      -- Module Input --
      discriIn            => CI_TRIGB,

      -- Module output --
      discriMuxOut        => NIM_OUT(1),

      -- Local bus --
      addrLocalBus        => addr_LocalBus,
      dataLocalBusIn      => data_LocalBusIn,
      dataLocalBusOut     => data_LocalBusOut(kIOM.ID),
      reLocalBus          => re_LocalBus(kIOM.ID),
      weLocalBus          => we_LocalBus(kIOM.ID),
      readyLocalBus       => ready_LocalBus(kIOM.ID)
      );


  -- C6C -------------------------------------------------------------------------------
  u_C6C_Inst : entity mylib.CDCE62002Controller
    generic map(
      kSysClkFreq         => 125_000_000
    )
    port map(
      rst	                => system_reset,
      clk	                => clk_slow,
      refClkIn            => clk_sys,

      chipReset           => c6c_reset,
      clkIndep            => clk_gbe,
      chipLock            => CDCE_LOCK,

      -- Module output --
      PDB                 => CDCE_PDB,
      REF_CLKP            => CDCE_REFP,
      REF_CLKN            => CDCE_REFN,
      CSB_SPI             => CDCE_LE,
      SCLK_SPI            => CDCE_SCLK,
      MOSI_SPI            => CDCE_SI,
      MISO_SPI            => CDCE_SO,

      -- Local bus --
      addrLocalBus	      => addr_LocalBus,
      dataLocalBusIn	    => data_LocalBusIn,
      dataLocalBusOut	    => data_LocalBusOut(kC6C.ID),
      reLocalBus		      => re_LocalBus(kC6C.ID),
      weLocalBus		      => we_LocalBus(kC6C.ID),
      readyLocalBus	      => ready_LocalBus(kC6C.ID)
    );

  -- MIG -------------------------------------------------------------------------------
  u_MIG : entity mylib.MigDD3SDRAM
    port map(
    rst	                => user_reset,
    clk	                => clk_slow,
    refClkIn            => clk_gbe,
    xadcTempIn          => reg_temp,

    -- DDR3-SDRAM toplevel ports--
    DDR3_CK_P           => DDR3_CK_P,
    DDR3_CK_N           => DDR3_CK_N,
    DDR3_RESET_N        => DDR3_RESET_N,
    DDR3_CKE            => DDR3_CKE,
    DDR3_CS_N           => DDR3_CS_N,
    DDR3_RAS_N          => DDR3_RAS_N,
    DDR3_CAS_N          => DDR3_CAS_N ,
    DDR3_WE_N           => DDR3_WE_N,
    DDR3_ODT            => DDR3_ODT,
    DDR3_BA             => DDR3_BA,
    DDR3_ADDR           => DDR3_ADDR,
    DDR3_DQ             => DDR3_DQ,
    DDR3_DM             => DDR3_DM,
    DDR3_DQS_P          => DDR3_DQS_P,
    DDR3_DQS_N          => DDR3_DQS_N,
    MIGS_CLKP           => MIGS_CLKP,
    MIGS_CLKN           => MIGS_CLKN,

    -- User ports --
    dIn                 => dout_test_data,
    writeEnable         => dvalid_test_data,
    progFull            => open,
    dOut                => din_test_data,
    readEnable          => ren_read_buf,
    readValid           => rvalid_test_data,
    emptyReadBuf        => empty_read_buf,

    -- Local bus --
    addrLocalBus	      => addr_LocalBus,
    dataLocalBusIn	    => data_LocalBusIn,
    dataLocalBusOut	    => data_LocalBusOut(kMIG.ID),
    reLocalBus		      => re_LocalBus(kMIG.ID),
    weLocalBus		      => we_LocalBus(kMIG.ID),
    readyLocalBus	      => ready_LocalBus(kMIG.ID)
    );

  ren_read_buf  <= not empty_read_buf;
  u_MIGtest : entity mylib.MigTestModule
    port map(
      rst         => user_reset,
      clk         => clk_slow,
      dOut        => dout_test_data,
      dValid      => dvalid_test_data,
      dIn         => din_test_data,
      rValid      => rvalid_test_data
      );


  -- TSD -------------------------------------------------------------------------------
  -- daq_data(0)   <= wd_to_tsd;
  -- daq_data(1)   <= "11011010";

  -- valid_data(0) <= we_to_tsd;
  -- valid_data(1) <= nim_in(4);

  -- empty_data(0) <= empty_to_tsd;
  -- empty_data(1) <= '1';

  -- re_from_tsd   <= req_data(0);

  gen_sd: for i in 0 to kNumGtx-1 generate
    u_TSD_Inst : entity mylib.TCP_sender
      port map(
        RST                     => user_reset,
        CLK                     => clk_sys,

        -- data from EVB --
        -- rdFromEVB               => daq_data(i),
        -- rvFromEVB               => valid_data(i),
        -- emptyFromEVB            => empty_data(i),
        -- reToEVB                 => req_data(i),

        -- -- data to SiTCP
        -- isActive                => tcp_isActive(i),
        -- afullTx                 => tcp_tx_full(i),
        -- weTx                    => tcp_tx_wr(i),
        -- wdTx                    => tcp_tx_data(i)

        -- data from EVB --
        rdFromEVB               => X"00",
        rvFromEVB               => '0',
        emptyFromEVB            => '1',
        reToEVB                 => open,

        -- data to SiTCP
        isActive                => '0',
        afullTx                 => '0',
        weTx                    => open,
        wdTx                    => open
        );
  end generate;

  -- SDS --------------------------------------------------------------------
  u_SDS_Inst : entity mylib.SelfDiagnosisSystem
    port map(
      rst               => user_reset,
      clk               => clk_slow,
      clkIcap           => clk_spi,

      -- Module input  --
      VP                => VP,
      VN                => VN,

      -- Module output --
      shutdownOverTemp  => shutdown_over_temp,
      xadcTempOut       => reg_temp,
      uncorrectableAlarm  => open,

      -- Local bus --
      addrLocalBus      => addr_LocalBus,
      dataLocalBusIn    => data_LocalBusIn,
      dataLocalBusOut   => data_LocalBusOut(kSDS.ID),
      reLocalBus        => re_LocalBus(kSDS.ID),
      weLocalBus        => we_LocalBus(kSDS.ID),
      readyLocalBus     => ready_LocalBus(kSDS.ID)
      );


  -- FMP --------------------------------------------------------------------
  u_FMP_Inst : entity mylib.FlashMemoryProgrammer
    port map(
      rst	              => user_reset,
      clk	              => clk_slow,
      clkSpi            => clk_spi,

      -- Module output --
      CS_SPI            => FCSB,
--      SCLK_SPI          => USR_CLK,
      MOSI_SPI          => MOSI,
      MISO_SPI          => DIN,

      -- Local bus --
      addrLocalBus      => addr_LocalBus,
      dataLocalBusIn    => data_LocalBusIn,
      dataLocalBusOut   => data_LocalBusOut(kFMP.ID),
      reLocalBus        => re_LocalBus(kFMP.ID),
      weLocalBus        => we_LocalBus(kFMP.ID),
      readyLocalBus     => ready_LocalBus(kFMP.ID)
      );

  -- BCT -------------------------------------------------------------------------------
  -- Actual local bus
  u_BCT_Inst : entity mylib.BusController
    port map(
      rstSys                    => bct_reset,
      rstFromBus                => rst_from_bus,
      reConfig                  => PROGB_ON,
      clk                       => clk_slow,
      -- Local Bus --
      addrLocalBus              => addr_LocalBus,
      dataFromUserModules       => data_LocalBusOut,
      dataToUserModules         => data_LocalBusIn,
      reLocalBus                => re_LocalBus,
      weLocalBus                => we_LocalBus,
      readyLocalBus             => ready_LocalBus,
      -- RBCP --
      addrRBCP                  => rbcp_addr(0),
      wdRBCP                    => rbcp_wd(0),
      weRBCP                    => rbcp_we(0),
      reRBCP                    => rbcp_re(0),
      ackRBCP                   => rbcp_ack(0),
      rdRBCP                    => rbcp_rd(0)
      );


  -- SiTCP Inst ------------------------------------------------------------------------
  sitcp_reset     <= system_reset OR (NOT USR_RSTB);

  gen_SiTCP : for i in 0 to kNumGtx-1 generate

    eth_tx_clk(i)      <= eth_rx_clk(0);

    u_SiTCP_Inst : WRAP_SiTCP_GMII_XC7K_32K
      port map
      (
        CLK               => clk_sys, --: System Clock >129MHz
        RST               => sitcp_reset, --: System reset
        -- Configuration parameters
--        FORCE_DEFAULTn    => DIP(kSiTCP.Index), --: Load default parameters
        FORCE_DEFAULTn         => '0', --: Load default parameters
        EXT_IP_ADDR       => X"00000000", --: IP address[31:0]
        EXT_TCP_PORT      => X"0000", --: TCP port #[15:0]
        EXT_RBCP_PORT     => X"0000", --: RBCP port #[15:0]
        PHY_ADDR          => "00000", --: PHY-device MIF address[4:0]
        -- EEPROM
        EEPROM_CS         => EEP_CS, --: Chip select
        EEPROM_SK         => EEP_SK, --: Serial data clock
        EEPROM_DI         => EEP_DI, --: Serial write data
        EEPROM_DO         => EEP_DO, --: Serial read data
        --    user data, intialial values are stored in the EEPROM, 0xFFFF_FC3C-3F
        USR_REG_X3C       => open, --: Stored at 0xFFFF_FF3C
        USR_REG_X3D       => open, --: Stored at 0xFFFF_FF3D
        USR_REG_X3E       => open, --: Stored at 0xFFFF_FF3E
        USR_REG_X3F       => open, --: Stored at 0xFFFF_FF3F
        -- MII interface
        GMII_RSTn         => open, --: PHY reset
        GMII_1000M        => '1',  --: GMII mode (0:MII, 1:GMII)
        -- TX
        GMII_TX_CLK       => eth_tx_clk(i), --: Tx clock
        GMII_TX_EN        => eth_tx_en(i),  --: Tx enable
        GMII_TXD          => eth_tx_d(i),   --: Tx data[7:0]
        GMII_TX_ER        => eth_tx_er(i),  --: TX error
        -- RX
        GMII_RX_CLK       => eth_rx_clk(0), --: Rx clock
        GMII_RX_DV        => eth_rx_dv(i),  --: Rx data valid
        GMII_RXD          => eth_rx_d(i),   --: Rx data[7:0]
        GMII_RX_ER        => eth_rx_er(i),  --: Rx error
        GMII_CRS          => '0', --: Carrier sense
        GMII_COL          => '0', --: Collision detected
        -- Management IF
        GMII_MDC          => open, --: Clock for MDIO
        GMII_MDIO_IN      => '1', -- : Data
        GMII_MDIO_OUT     => open, --: Data
        GMII_MDIO_OE      => open, --: MDIO output enable
        -- User I/F
        SiTCP_RST         => emergency_reset(i), --: Reset for SiTCP and related circuits
        -- TCP connection control
        TCP_OPEN_REQ      => '0', -- : Reserved input, shoud be 0
        TCP_OPEN_ACK      => tcp_isActive(i), --: Acknowledge for open (=Socket busy)
        --    TCP_ERROR           : out    std_logic; --: TCP error, its active period is equal to MSL
        TCP_CLOSE_REQ     => close_req(i), --: Connection close request
        TCP_CLOSE_ACK     => close_act(i), -- : Acknowledge for closing
        -- FIFO I/F
        TCP_RX_WC         => X"0000",    --: Rx FIFO write count[15:0] (Unused bits should be set 1)
        TCP_RX_WR         => open, --: Read enable
        TCP_RX_DATA       => open, --: Read data[7:0]
        TCP_TX_FULL       => tcp_tx_full(i), --: Almost full flag
        TCP_TX_WR         => tcp_tx_wr(i),   -- : Write enable
        TCP_TX_DATA       => tcp_tx_data(i), -- : Write data[7:0]
        -- RBCP
        RBCP_ACT          => open, --: RBCP active
        RBCP_ADDR         => rbcp_gmii_addr(i), --: Address[31:0]
        RBCP_WD           => rbcp_gmii_wd(i),   --: Data[7:0]
        RBCP_WE           => rbcp_gmii_we(i),   --: Write enable
        RBCP_RE           => rbcp_gmii_re(i),   --: Read enable
        RBCP_ACK          => rbcp_gmii_ack(i),  --: Access acknowledge
        RBCP_RD           => rbcp_gmii_rd(i)    --: Read data[7:0]
        );

    --aaa
    u_RbcpCdc : entity mylib.RbcpCdc
      port map(
        -- Mikumari clock domain --
        rstSys      => system_reset,
        clkSys      => clk_slow,
        rbcpAddr    => rbcp_addr(i),
        rbcpWd      => rbcp_wd(i),
        rbcpWe      => rbcp_we(i),
        rbcpRe      => rbcp_re(i),
        rbcpAck     => rbcp_ack(i),
        rbcpRd      => rbcp_rd(i),

        -- GMII clock domain --
        rstXgmii    => system_reset,
        clkXgmii    => clk_sys,
        rbcpXgAddr  => rbcp_gmii_addr(i),
        rbcpXgWd    => rbcp_gmii_wd(i),
        rbcpXgWe    => rbcp_gmii_we(i),
        rbcpXgRe    => rbcp_gmii_re(i),
        rbcpXgAck   => rbcp_gmii_ack(i),
        rbcpXgRd    => rbcp_gmii_rd(i)
        );

    u_gTCP_inst : entity mylib.global_sitcp_manager
      port map(
        RST           => system_reset,
        CLK           => clk_sys,
        ACTIVE        => tcp_isActive(i),
        REQ           => close_req(i),
        ACT           => close_act(i),
        rstFromTCP    => open
        );
  end generate;

  -- SFP transceiver -------------------------------------------------------------------
  u_MiiRstTimer_Inst : entity mylib.MiiRstTimer
    port map(
      rst         => system_reset,
      clk         => clk_sys,
      rstMiiOut   => mii_reset
    );

  u_MiiInit_Inst : mii_initializer
    port map(
      -- System
      CLK         => clk_sys,
      --RST         => system_reset,
      RST         => mii_reset,
      -- PHY
      PHYAD       => kMiiPhyad,
      -- MII
      MDC         => mii_init_mdc,
      MDIO_OUT    => mii_init_mdio,
      -- status
      COMPLETE    => open
      );

  mmcm_reset_all  <= or_reduce(mmcm_reset);

  u_GtClockDist_Inst : entity mylib.GtClockDistributer2
    port map(
      -- GTX refclk --
      GT_REFCLK_P   => GTX_REFCLK_P,
      GT_REFCLK_N   => GTX_REFCLK_N,

      gtRefClk      => gtrefclk_i,
      gtRefClkBufg  => gtrefclk_bufg,

      -- USERCLK2 --
      mmcmReset     => mmcm_reset_all,
      mmcmLocked    => mmcm_locked,
      txOutClk      => txout_clk(0),
      rxOutClk      => rxout_clk(0),

      userClk       => user_clk,
      userClk2      => user_clk2,
      rxuserClk     => rxuser_clk,
      rxuserClk2    => rxuser_clk2,

      -- GTXE_COMMON --
      reset         => system_reset,
      clkIndep      => clk_gbe,
      clkQPLL       => gt0_qplloutclk,
      refclkQPLL    => gt0_qplloutrefclk
      );

  gen_pcspma : for i in 0 to kNumGtx-1 generate
    u_pcspma_Inst : entity mylib.GbEPcsPma
      port map(

        --An independent clock source used as the reference clock for an
        --IDELAYCTRL (if present) and for the main GT transceiver reset logic.
        --This example design assumes that this is of frequency 200MHz.
        independent_clock    => clk_gbe,

        -- Tranceiver Interface
        -----------------------
        gtrefclk             => gtrefclk_i,
        gtrefclk_bufg        => gtrefclk_bufg,

        gt0_qplloutclk       => gt0_qplloutclk,
        gt0_qplloutrefclk    => gt0_qplloutrefclk,

        userclk              => user_clk,
        userclk2             => user_clk2,
        rxuserclk            => rxuser_clk,
        rxuserclk2           => rxuser_clk2,

        mmcm_locked          => mmcm_locked,
        mmcm_reset           => mmcm_reset(i),

        -- clockout --
        txoutclk             => txout_clk(i),
        rxoutclk             => rxout_clk(i),

        -- Tranceiver Interface
        -----------------------
        txp                  => GTX_TX_P,
        txn                  => GTX_TX_N,
        rxp                  => GTX_RX_P,
        rxn                  => GTX_RX_N,

        -- GMII Interface (client MAC <=> PCS)
        --------------------------------------
        gmii_tx_clk          => eth_tx_clk(i),
        gmii_rx_clk          => eth_rx_clk(i),
        gmii_txd             => eth_tx_d(i),
        gmii_tx_en           => eth_tx_en(i),
        gmii_tx_er           => eth_tx_er(i),
        gmii_rxd             => eth_rx_d(i),
        gmii_rx_dv           => eth_rx_dv(i),
        gmii_rx_er           => eth_rx_er(i),
        -- Management: MDIO Interface
        -----------------------------

        mdc                  => mii_init_mdc,
        mdio_i               => mii_init_mdio,
        mdio_o               => open,
        mdio_t               => open,
        phyaddr              => "00000",
        configuration_vector => "00000",
        configuration_valid  => '0',

        -- General IO's
        ---------------
        status_vector        => open,
        reset                => system_reset
        );
  end generate;

  -- Clock inst ------------------------------------------------------------------------
  u_ClkMan_Inst   : clk_wiz_sys
    port map (
      -- Clock out ports
      clk_sys         => clk_sys,
      clk_indep_gtx   => clk_gbe,
      clk_spi         => clk_spi,
      clk_machine     => clk_machine,
      -- Status and control signals
      reset           => '0',
      locked          => clk_sys_locked,
      -- Clock in ports
      clk_in1_p       => BASE_CLKP,
      clk_in1_n       => BASE_CLKN
      );


  u_div2 : process(clk_machine)
  begin
    if(clk_machine'event and clk_machine = '1') then
      clk_machine_div2  <= not clk_machine_div2;
    end if;
  end process;

  u_bufg_div2 : BUFG
  port map (
     O => gclk_mac_div2, -- 1-bit output: Clock output
     I => clk_machine_div2  -- 1-bit input: Clock input
  );

  u_ClkTdc_Inst :  clk_wiz_tdc
    port map
     (-- Clock in ports
      -- Clock out ports
      clk_tdc           => clk_tdc,
      clk_slow          => clk_slow,
      -- Status and control signals
      reset             => mmcm_cdcm_reset,
      locked            => mmcm_cdcm_locked,
      clk_in1           => clk_sys
     );


end Behavioral;
