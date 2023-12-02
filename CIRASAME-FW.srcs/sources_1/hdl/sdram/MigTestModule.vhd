library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_misc.all;
use ieee.std_logic_unsigned.all;

library mylib;
use mylib.defMIG.all;

Library xpm;
use xpm.vcomponents.all;


entity MigTestModule is
  port (
    rst         : in std_logic;
    clk         : in std_logic;
    dOut        : out std_logic_vector(kWidthDaqData-1 downto 0);
    dValid      : out std_logic;
    dIn         : in  std_logic_vector(kWidthDaqData-1 downto 0);
    rValid      : in std_logic
    );
end MigTestModule;

architecture RTL of MigTestModule is
  attribute mark_debug  : string;

  -- System --
    signal sync_reset           : std_logic;

  -- signal decralation -----------------------------------------------------
  constant kWidthCount        : positive:= 16;
  signal reg_test_data_cout   : std_logic_vector(kWidthCount-1 downto 0);
  signal start_test : std_logic_vector(0 downto 0);
  COMPONENT vio_mig
    PORT (

      clk : IN STD_LOGIC;
      probe_out0 : OUT STD_LOGIC_VECTOR(kWidthCount-1 DOWNTO 0);
      probe_out1 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
    );
  END COMPONENT;

  signal prev_data, current_data  : std_logic_vector(kWidthDaqData-1 downto 0);
  signal test_data_is_ok          : std_logic;

  -- debug --
  -- attribute mark_debug of current_data : signal is "true";
  -- attribute mark_debug of test_data_is_ok : signal is "true";

begin
  -- ====================== body ============================= --

  u_vio : vio_mig
    port map(
      clk         => clk,
      probe_out0  => reg_test_data_cout,
      probe_out1  => start_test
    );

  u_count : process(sync_reset, clk)
    variable count : std_logic_vector(kWidthCount-1 downto 0);
  begin
    if(sync_reset = '1') then
      count   := (others => '0');
    elsif(clk'event and clk = '1') then
      if(start_test = "1") then
        if(count /= conv_integer(reg_test_data_cout)) then
          count           := count +1;
          dOut   <= conv_std_logic_vector(0, kWidthDaqData-kWidthCount) & count;
          dValid <= '1';
        else
          dValid <= '0';
        end if;
      else
        count     := (others => '0');
      end if;
    end if;
  end process;

  u_check : process(sync_reset, clk)
  begin
    if(sync_reset = '1') then
      prev_data     <= (others => '0');
      current_data  <= (others => '0');
      test_data_is_ok  <= '0';
    elsif(clk'event and clk = '1') then
      if(rValid = '1') then
        current_data  <= dIn;
        prev_data     <= current_data;
      end if;

      if(current_data - prev_data = 1) then
        test_data_is_ok   <= '1';
      else
        test_data_is_ok   <= '0';
      end if;
    end if;
  end process;

  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, clk, sync_reset);

end RTL;
