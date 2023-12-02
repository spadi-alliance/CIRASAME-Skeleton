library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;

library mylib;
use mylib.defBCT.all;
use mylib.defRBCP.all;

library UNISIM;
use UNISIM.VComponents.all;

entity RbcpCdc is
  port(
    -- System clock domain --
    rstSys      : in std_logic;
    clkSys      : in std_logic;
    rbcpAddr    : out std_logic_vector(kWidthAddrRBCP-1 downto 0);
    rbcpWd      : out std_logic_vector(kWidthDataRBCP-1 downto 0);
    rbcpWe      : out std_logic;
    rbcpRe      : out std_logic;
    rbcpAck     : in std_logic;
    rbcpRd      : in std_logic_vector(kWidthDataRBCP-1 downto 0);

    -- XGMII clock domain --
    rstXgmii    : in std_logic;
    clkXgmii    : in std_logic;
    rbcpXgAddr  : in std_logic_vector(kWidthAddrRBCP-1 downto 0);
    rbcpXgWd    : in std_logic_vector(kWidthDataRBCP-1 downto 0);
    rbcpXgWe    : in std_logic;
    rbcpXgRe    : in std_logic;
    rbcpXgAck   : out std_logic;
    rbcpXgRd    : out std_logic_vector(kWidthDataRBCP-1 downto 0)
    );
end RbcpCdc;

architecture RTL of RbcpCdc is
  -- signal declaration ---------------------------------------------------
  constant kWidthXtoS : integer:= kWidthAddrRBCP + kWidthDataRBCP + 2;
  signal reg_sys_out, reg_xgmii_in  : std_logic_vector(kWidthXtoS-1 downto 0);
  signal we_xgmii_in, re_sys_out    : std_logic;
  signal empty_linktosys            : std_logic;
  signal valid_sys_out              : std_logic;

  constant kWidthStoX : integer:= kWidthDataRBCP + 1;
  signal reg_sys_in, reg_xgmii_out  : std_logic_vector(kWidthStoX-1 downto 0);
  signal we_sys_in, re_xgmii_out    : std_logic;
  signal empty_systolink            : std_logic;
  signal valid_xgmii_out            : std_logic;


  COMPONENT RbcpCdc_LinkToSys
    PORT (
      rst : IN STD_LOGIC;
      wr_clk : IN STD_LOGIC;
      rd_clk : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(41 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(41 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      valid : OUT STD_LOGIC
    );
  END COMPONENT;

  COMPONENT RbcpCdc_SysToLink
    PORT (
      rst : IN STD_LOGIC;
      wr_clk : IN STD_LOGIC;
      rd_clk : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(8 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(8 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      valid : OUT STD_LOGIC
    );
  END COMPONENT;

-- ================================ body ==================================
begin
  -- signal connection ----------------------------------------------------


  -- XGMII to SYSTEM --
  rbcpAddr  <= reg_sys_out(kWidthXtoS-1 downto kWidthXtoS-kWidthAddrRBCP);
  rbcpWd    <= reg_sys_out(kWidthXtoS-kWidthAddrRBCP-1 downto kWidthXtoS-kWidthAddrRBCP-kWidthDataRBCP);
  rbcpWe    <= reg_sys_out(1) and valid_sys_out;
  rbcpRe    <= reg_sys_out(0) and valid_sys_out;

  reg_xgmii_in(kWidthXtoS-1 downto kWidthXtoS-kWidthAddrRBCP)                                 <= rbcpXgAddr;
  reg_xgmii_in(kWidthXtoS-kWidthAddrRBCP-1 downto kWidthXtoS-kWidthAddrRBCP-kWidthDataRBCP)   <= rbcpXgWd;
  reg_xgmii_in(1)   <= rbcpXgWe;
  reg_xgmii_in(0)   <= rbcpXgRe;

  we_xgmii_in       <= rbcpXgWe or rbcpXgRe;
  re_sys_out        <= not empty_linktosys;

  u_LinkToSys : RbcpCdc_LinkToSys
    port map(
      rst       => rstXgmii,
      wr_clk    => clkXgmii,
      rd_clk    => clkSys,
      din       => reg_xgmii_in,
      wr_en     => we_xgmii_in,
      rd_en     => re_sys_out,
      dout      => reg_sys_out,
      full      => open,
      empty     => empty_linktosys,
      valid     => valid_sys_out
    );


  -- SYSTEM to XGMII --
  rbcpXgRd  <= reg_xgmii_out(kWidthStoX-1 downto kWidthStoX-kWidthDataRBCP);
  rbcpXgAck <= reg_xgmii_out(0) and valid_xgmii_out;

  reg_sys_in(kWidthStoX-1 downto kWidthStoX-kWidthDataRBCP)   <= rbcpRd;
  reg_sys_in(0)   <= rbcpAck;

  we_sys_in       <= rbcpAck;
  re_xgmii_out    <= not empty_systolink;

  u_SysToLink : RbcpCdc_SysToLink
    port map(
      rst       => rstSys,
      wr_clk    => clkSys,
      rd_clk    => clkXgmii,
      din       => reg_sys_in,
      wr_en     => we_sys_in,
      rd_en     => re_xgmii_out,
      dout      => reg_xgmii_out,
      full      => open,
      empty     => empty_systolink,
      valid     => valid_xgmii_out
    );



end RTL;

