library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

library mylib;
use mylib.defMIF.all;

library UNISIM;
use UNISIM.VComponents.all;


entity MznInterface is
  generic
    (
      invStatusMzn  : std_logic_vector(kWidthStatusMzn-1 downto 0);
      invStatusBase : std_logic_vector(kWidthStatusBase-1 downto 0);
      invFrst       : std_logic;
      invClk        : std_logic_vector(1 downto 0);
      invSlotPos    : std_logic;
      kExIoStd      : string
  );
  port
    (
      -- Toplevel MZN ports ----------------------------------------------------
      -- System
      FRST_P            : out std_logic;
      FRST_N            : out std_logic;
      CLKHUL_P          : out std_logic;
      CLKHUL_N          : out std_logic;
      SLOT_POS_P        : out std_logic;
      SLOT_POS_N        : out std_logic;

      -- Bct Bus Bridge --
      BBP_PRI_ACTIVE_P  : out std_logic;
      BBP_PRI_ACTIVE_N  : out std_logic;
      BBP_SCN_REQ_P     : in std_logic;
      BBP_SCN_REQ_N     : in std_logic;
      BBP_CLK_P         : out std_logic;
      BBP_CLK_N         : out std_logic;
      BBP_PISO_P        : in std_logic;
      BBP_PISO_N        : in std_logic;
      BBP_POSI_P       : out std_logic;
      BBP_POSI_N       : out std_logic;

      -- DAQ functions --
--      TRIGGER_P         : out std_logic;
--      TRIGGER_N         : out std_logic;
--      BUSY_P            : in std_logic;
--      BUSY_N            : in std_logic;

      -- Status ports --
      STATUS_IN_P       : in std_logic_vector(kWidthStatusMzn-1 downto 0);
      STATUS_IN_N       : in std_logic_vector(kWidthStatusMzn-1 downto 0);
      STATUS_OUT_P      : out std_logic_vector(kWidthStatusBase-1 downto 0);
      STATUS_OUT_N      : out std_logic_vector(kWidthStatusBase-1 downto 0);

      -- FW internal signals ---------------------------------------------------
      -- System ports --
      rstForce          : in std_logic;
      clkHul            : in std_logic;
      slotPosition      : in std_logic;

      -- Bct Bus Bridge --
      bbpPrimActive     : in std_logic;
      bbpScndReq        : out std_logic;
      bbpClk            : in std_logic;
      bbpPiso           : out std_logic;
      bbpPosi          : in std_logic;

      -- MIF status ports --
      statusMzn         : out std_logic_vector(kWidthStatusMzn-1 downto 0);
      statusBase        : in std_logic_vector(kWidthStatusBase-1 downto 0)
);
end MznInterface;

architecture RTL of MznInterface is
  -- signal decralation ---------------------------------------------------
  signal masked_trigger, raw_busy  : std_logic;

  signal clk_hul        : std_logic;
  signal rst_force      : std_logic;
  signal slot_pos       : std_logic;
  signal status_mzn     : std_logic_vector(kWidthStatusMzn-1 downto 0);
  signal status_base    : std_logic_vector(kWidthStatusBase-1 downto 0);

begin
  -- ================================ body ================================
  -- Force Reset -------------------------------------------
  rst_force <= rstForce xor invFrst;

  u_MZN_FRST_inst : OBUFDS
    generic map ( IOSTANDARD => "LVDS_25", SLEW => "SLOW")
    port map ( O => FRST_P, OB => FRST_N, I => rst_force );

  -- Slot position -----------------------------------------
  slot_pos  <= slotPosition xor invSlotPos;
  u_MZN_SLOTPOS_inst : OBUFDS
    generic map ( IOSTANDARD => kExIoStd, SLEW => "SLOW")
    port map ( O => SLOT_POS_P, OB => SLOT_POS_N, I =>  slot_pos);

  -- Clock Output for MZN ----------------------------------
  u_MZN_CLKHUL_inst : OBUFDS
    generic map ( IOSTANDARD => "LVDS_25", SLEW => "FAST")
    port map ( O => CLKHUL_P, OB => CLKHUL_N, I => clk_hul );

  u_wdclk_oddr : ODDR
    generic map(
      DDR_CLK_EDGE => "SAME_EDGE", -- "OPPOSITE_EDGE" or "SAME_EDGE"
      INIT         => '0',   -- Initial value for Q port ('1' or '0')
      SRTYPE       => "SYNC") -- Reset Type ("ASYNC" or "SYNC")
    port map (
      Q  => clk_hul,   -- 1-bit DDR output
      C  => clkHul,    -- 1-bit clock input
      CE => '1',       -- 1-bit clock enable input
      D1 => invClk(0), -- 1-bit data input (positive edge)
      D2 => invClk(1), -- 1-bit data input (negative edge)
      R  => '0',       -- 1-bit reset input
      S  => '0'        -- 1-bit set input
      );

  u_PRI_ACTIVE : OBUFDS
    generic map ( IOSTANDARD => "LVDS_25", SLEW => "SLOW")
    port map ( O => BBP_PRI_ACTIVE_P, OB => BBP_PRI_ACTIVE_N, I => bbpPrimActive );

  u_SNCD_REQ : IBUFDS
    generic map ( DIFF_TERM => TRUE, IBUF_LOW_PWR => TRUE, IOSTANDARD => "LVDS_25")
    port map ( O => bbpScndReq, I => BBP_SCN_REQ_P, IB => BBP_SCN_REQ_N );

  u_piso : IBUFDS
    generic map ( DIFF_TERM => TRUE, IBUF_LOW_PWR => TRUE, IOSTANDARD => "LVDS_25")
    port map ( O => bbpPiso, I => BBP_PISO_P, IB => BBP_PISO_N );

  u_posi : OBUFDS
    generic map ( IOSTANDARD => "LVDS_25", SLEW => "SLOW")
    port map ( O => BBP_POSI_P, OB => BBP_POSI_N, I => bbpPosi );

  u_BBPCLK : OBUFDS
    generic map ( IOSTANDARD => kExIoStd, SLEW => "SLOW")
    port map ( O => BBP_CLK_P, OB => BBP_CLK_N, I => bbpClk );

  -- STATUS -----------------------------------------------------
  gen_status_mzn : for i in 0 to kWidthStatusMzn-1 generate
    statusMzn(i) <= status_mzn(i) xor invStatusMzn(i);
    u_MZN_STATUS_inst : IBUFDS
      generic map ( DIFF_TERM => TRUE, IBUF_LOW_PWR => TRUE, IOSTANDARD => "LVDS_25")
      port map ( O => status_mzn(i), I => STATUS_IN_P(i), IB => STATUS_IN_N(i) );
  end generate;

  gen_status_base : for i in 0 to kWidthStatusBase-1 generate
    status_base(i)  <= statusBase(i) xor invStatusBase(i);
    u_BASE_STATUS_inst : OBUFDS
      generic map ( IOSTANDARD => "LVDS_25", SLEW => "SLOW")
      port map ( O => STATUS_OUT_P(i), OB => STATUS_OUT_N(i), I => status_base(i) );
  end generate;

end RTL;
