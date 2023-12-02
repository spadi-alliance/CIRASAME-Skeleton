LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

library mylib;

entity HoldGenerator is
  port(
    -- System --
    clk       : in std_logic;
    reset     : in std_logic;
    adcMode   : in std_logic;
    holdIn    : in std_logic; -- Async sig

    holdOut   : out std_logic; -- Async sig to CITIROC
    startAdc  : out std_logic;
    busyAdc   : in std_logic
    );
end HoldGenerator;

architecture RTL of HoldGenerator is
  attribute mark_debug  : boolean;

  -- System --
  signal sync_reset           : std_logic;

  -- Signal latch --
  signal reset_hold     : std_logic;
  signal en_dlatch_0    : std_logic;
  signal latch_hold_0, latch_hold   : std_logic;

  constant kWidthSr     : integer:= 256;
  signal sync_hold      : std_logic;
  signal oneshot_hold   : std_logic;
  signal delay_sr       : std_logic_vector(kWidthSr-1 downto 0);

  -- debug --
  --attribute mark_debug of oneshot_hold       : signal is true;

begin

  -- Port connection --
  holdOut   <= latch_hold or (busyAdc and adcMode);
  startAdc  <= oneshot_hold and adcMode and (not busyAdc);

  -- Asynchronous signal latcher ---------------------------------------------------------------
  u_dlatch_0 : entity mylib.DLatch port map(reset_hold, holdIn,       en_dlatch_0, latch_hold_0);
  u_dlatch_1 : entity mylib.DLatch port map(reset_hold, latch_hold_0, '1',         latch_hold);
  en_dlatch_0   <= not latch_hold;

  u_sync : entity mylib.synchronizer port map(clk, holdIn, sync_hold);
  u_edge : entity mylib.EdgeDetector port map('0', clk, sync_hold, oneshot_hold);
  u_delay_sr : process(clk)
  begin
    if(clk'event and clk = '1') then
      delay_sr  <= delay_sr(kWidthSr-2 downto 0) & oneshot_hold;
    end if;
  end process;
  reset_hold  <= delay_sr(kWidthSr-1) or reset;

  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(reset, clk, sync_reset);




end RTL;
