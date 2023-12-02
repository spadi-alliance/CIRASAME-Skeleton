library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

library mylib;
use mylib.defGlobal.all;

library UNISIM;
use UNISIM.VComponents.all;

entity PhaseDetector is
  port (
    rst         : in std_logic;
    clkSlow     : in std_logic;
    clkFast     : in std_logic;
    clkCdce     : in std_logic;
    cdceReset   : out std_logic
    );
  end PhaseDetector;

architecture RTL of PhaseDetector is
  attribute mark_debug  : string;
  attribute RLOC        : string;
  attribute BEL         : string;

  -- System --
  signal reset_shiftreg       : std_logic_vector(kWidthResetSync-1 downto 0);
  signal sync_reset           : std_logic;

  -- Phase detector --
  signal clear_q      : std_logic;
  signal d_ff         : std_logic_vector(1 downto 0);
  signal q_ff         : std_logic_vector(1 downto 0);

  type SyncPhaseType is array (integer range 0 to 1) of
    std_logic_vector(1 downto 0);

  signal sync_phase_diff  : SyncPhaseType;

  attribute RLOC of u_phase_a : label is "X0Y0";
  attribute RLOC of u_phase_b : label is "X1Y0";
  attribute RLOC of u_sync1_a : label is "X0Y1";
  attribute RLOC of u_sync1_b : label is "X1Y1";
  attribute RLOC of u_sync2_a : label is "X0Y1";
  attribute RLOC of u_sync2_b : label is "X1Y1";

  attribute BEL of u_phase_a : label is "DFF";
  attribute BEL of u_phase_b : label is "DFF";

  attribute BEL of u_sync1_a : label is "AFF";
  attribute BEL of u_sync1_b : label is "AFF";
  attribute BEL of u_sync2_a : label is "BFF";
  attribute BEL of u_sync2_b : label is "BFF";

  -- Phase Gacha --
  constant kWidthTime     : positive:= 12;
  signal elapsed_time     : std_logic_vector(kWidthTime-1 downto 0);
  signal num_phase_diff   : std_logic_vector(elapsed_time'range);
  signal cdce_reset       : std_logic;

  constant kTimeThreshold : integer:= 4000;
  constant k0degThreshold : integer:= 100;

begin
  -- ====================== body ============================= --
  cdceReset <= cdce_reset;

  -- Phase detector --
  clear_q   <= q_ff(0) and q_ff(1);
  d_ff(0)   <= not q_ff(0);
  d_ff(1)   <= not q_ff(1);
  u_phase_a : FDCE port map(q_ff(0), clkSlow, '1', clear_q, d_ff(0));
  u_phase_b : FDCE port map(q_ff(1), clkCdce, '1', clear_q, d_ff(1));

  u_sync1_a : FDCE port map(sync_phase_diff(0)(0), clkFast, '1', rst, q_ff(0));
  u_sync1_b : FDCE port map(sync_phase_diff(1)(0), clkFast, '1', rst, q_ff(1));

  u_sync2_a : FDCE port map(sync_phase_diff(0)(1), clkFast, '1', rst, sync_phase_diff(0)(0));
  u_sync2_b : FDCE port map(sync_phase_diff(1)(1), clkFast, '1', rst, sync_phase_diff(1)(0));

  -- Phase Gacha --
  u_phase_diff: process(sync_reset, clkFast)
  begin
    if(sync_reset = '1') then
      elapsed_time    <= (others => '0');
      num_phase_diff  <= (others => '0');
      cdce_reset      <= '0';
    elsif(clkFast'event and clkFast = '1') then
      if(to_integer(unsigned(elapsed_time)) < kTimeThreshold) then
        elapsed_time  <= std_logic_vector(unsigned(elapsed_time) +1);
        if(sync_phase_diff(0)(1) = '1' or sync_phase_diff(1)(1) = '1') then
          num_phase_diff  <= std_logic_vector(unsigned(num_phase_diff) +1);
        end if;
      else
        if(to_integer(unsigned(num_phase_diff)) > k0degThreshold) then
          cdce_reset  <= '1';
        else
          cdce_reset  <= '0';
        end if;
      end if;
    end if;
  end process;

  -- Reset sequence --
  sync_reset  <= reset_shiftreg(kWidthResetSync-1);
  u_sync_reset : process(rst, clkFast)
  begin
    if(rst = '1') then
      reset_shiftreg  <= (others => '1');
    elsif(clkFast'event and clkFast = '1') then
      reset_shiftreg  <= reset_shiftreg(kWidthResetSync-2 downto 0) & '0';
    end if;
  end process;

end RTL;
