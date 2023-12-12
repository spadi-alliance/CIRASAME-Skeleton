library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

Library UNISIM;
use UNISIM.vcomponents.all;
--

entity Iserdes8Impl is
  generic
  (
    kRxPolarity     : boolean:= false;       -- Invert output polarity
    kIoStandard     : string:= "LVCMOS18"    -- IOSTANDARD of OBUFDS
  );
  port
  (
    -- From the system to the device
    dInFromPin      : in std_logic;
    ibufOut         : out std_logic;

    -- ISERDES
    dOutToDevice    : out std_logic_vector(7 downto 0);
    bitslip         : in std_logic;

    -- Clock and reset
    clkIn           : in std_logic;
    clkDivIn        : in std_logic;
    ioReset         : in std_logic
  );
end Iserdes8Impl;

architecture RTL of Iserdes8Impl is
  constant kSysW           : integer:= 1;  -- width of the ata for the system
  constant kDevW           : integer:= 8; -- width of the ata for the device


  signal clk_in, clk_in_inv     : std_logic;
  signal data_in_from_pin       : std_logic;
  signal iserdes_q  : std_logic_vector(13 downto 0);
  signal rx_output  : std_logic_vector(dOutToDevice'range);

begin

  ibufOut <= data_in_from_pin;

  u_IBUF_inst : IBUF
    generic map
    (
      IBUF_LOW_PWR => TRUE,     -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
      IOSTANDARD   => kIoStandard
    )
    port map (
      O => data_in_from_pin,  -- Buffer output
      I => dInFromPin         -- Buffer input (connect directly to top-level port)
    );

  clk_in      <= clkIn;
  clk_in_inv  <= not clkIn;

  u_ISERDESE2_master : ISERDESE2
    generic map (
       DATA_RATE          => "DDR",         -- DDR, SDR
       DATA_WIDTH         => kDevW,         -- Parallel data width (2-8,10,14)
       DYN_CLKDIV_INV_EN  => "FALSE",       -- Enable DYNCLKDIVINVSEL inversion (FALSE, TRUE)
       DYN_CLK_INV_EN     => "FALSE",       -- Enable DYNCLKINVSEL inversion (FALSE, TRUE)
       INTERFACE_TYPE     => "NETWORKING",  -- MEMORY, MEMORY_DDR3, MEMORY_QDR, NETWORKING, OVERSAMPLE
       IOBDELAY           => "NONE",         -- NONE, BOTH, IBUF, IFD
       NUM_CE             => 2,             -- Number of clock enables (1,2)
       OFB_USED           => "FALSE",       -- Select OFB path (FALSE, TRUE)
       SERDES_MODE        => "MASTER"       -- MASTER, SLAVE
    )
    port map (
       O => open,                       -- 1-bit output: Combinatorial output
       -- Q1 - Q8: 1-bit (each) output: Registered data outputs
       Q1 => iserdes_q(0),
       Q2 => iserdes_q(1),
       Q3 => iserdes_q(2),
       Q4 => iserdes_q(3),
       Q5 => iserdes_q(4),
       Q6 => iserdes_q(5),
       Q7 => iserdes_q(6),
       Q8 => iserdes_q(7),
       -- SHIFTOUT1, SHIFTOUT2: 1-bit (each) output: Data width expansion output ports
       SHIFTOUT1 => open,
       SHIFTOUT2 => open,
       BITSLIP => bitslip,           -- 1-bit input: The BITSLIP pin performs a Bitslip operation synchronous to
                                     -- CLKDIV when asserted (active High). Subsequently, the data seen on the
                                     -- Q1 to Q8 output ports will shift, as in a barrel-shifter operation, one
                                     -- position every time Bitslip is invoked (DDR operation is different from
                                     -- SDR).

       -- CE1, CE2: 1-bit (each) input: Data register clock enable inputs
       CE1 => '1',
       CE2 => '1',
       CLKDIVP => '0',           -- 1-bit input: TBD
       -- Clocks: 1-bit (each) input: ISERDESE2 clock input ports
       CLK => clk_in,                   -- 1-bit input: High-speed clock
       CLKB => clk_in_inv,                 -- 1-bit input: High-speed secondary clock
       CLKDIV => clkDivIn,             -- 1-bit input: Divided clock
       OCLK => '0',                 -- 1-bit input: High speed output clock used when INTERFACE_TYPE="MEMORY"
       -- Dynamic Clock Inversions: 1-bit (each) input: Dynamic clock inversion pins to switch clock polarity
       DYNCLKDIVSEL => '0', -- 1-bit input: Dynamic CLKDIV inversion
       DYNCLKSEL => '0',       -- 1-bit input: Dynamic CLK/CLKB inversion
       -- Input Data: 1-bit (each) input: ISERDESE2 data input ports
       D => data_in_from_pin,                       -- 1-bit input: Data input
       DDLY => '0',                 -- 1-bit input: Serial data from IDELAYE2
       OFB => '0',                   -- 1-bit input: Data feedback from OSERDESE2
       OCLKB => '0',               -- 1-bit input: High speed negative edge output clock
       RST => ioReset,                   -- 1-bit input: Active high asynchronous reset
       -- SHIFTIN1, SHIFTIN2: 1-bit (each) input: Data width expansion input ports
       SHIFTIN1 => '0',
       SHIFTIN2 => '0'
    );


  u_swap : for i in 0 to kDevW-1 generate
    begin
      rx_output(i)   <= iserdes_q(kDevW-i-1);
  end generate;

  gen_invout : if kRxPolarity = TRUE generate
  begin
      dOutToDevice  <= not rx_output;
  end generate;

  gen_out : if kRxPolarity = FALSE generate
  begin
      dOutToDevice  <= rx_output;
  end generate;

end RTL;
