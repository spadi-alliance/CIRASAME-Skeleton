library IEEE, mylib;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use ieee.math_real.all;

use mylib.defBCT.all;
use mylib.defIOManager.all;

entity IOManager is
  generic(
    kNumInput           : integer:= 128
  );
  port(
    rst	                : in std_logic;
    clk	                : in std_logic;

    -- Module Input --
    discriIn            : in std_logic_vector(kNumInput-1 downto 0);

    -- Module output --
    discriMuxOut        : out std_logic;

    -- Local bus --
    addrLocalBus        : in LocalAddressType;
    dataLocalBusIn      : in LocalBusInType;
    dataLocalBusOut	    : out LocalBusOutType;
    reLocalBus          : in std_logic;
    weLocalBus          : in std_logic;
    readyLocalBus	      : out std_logic
    );
end IOManager;

architecture RTL of IOManager is

  -- System --
  signal sync_reset           : std_logic;

  -- internal signal declaration ----------------------------------------
  constant kWidthDiscriReg  : integer:= integer(ceil(log2(real(kNumInput))));

  signal reg_discri	: std_logic_vector(kWidthDiscriReg-1 downto 0);
  signal state_lbus	: BusProcessType;

-- =============================== body ===============================
begin

  discriMuxOut  <= discriIn(to_integer(unsigned(reg_discri)));

  u_BusProcess : process(clk, sync_reset)
  begin
    if(sync_reset = '1') then
      reg_discri		<= (others => '0');

      state_lbus	<= Init;
    elsif(clk'event and clk = '1') then
      case state_lbus is
        when Init =>
          dataLocalBusOut       <= x"00";
          readyLocalBus		<= '0';
          reg_discri		<= (others => '0');
          state_lbus		<= Idle;

        when Idle =>
          readyLocalBus	<= '0';
          if(weLocalBus = '1' or reLocalBus = '1') then
            state_lbus	<= Connect;
          end if;

        when Connect =>
          if(weLocalBus = '1') then
            state_lbus	<= Write;
          else
            state_lbus	<= Read;
          end if;

        when Write =>
          case addrLocalBus(kNonMultiByte'range) is
            when kSelDiscri(kNonMultiByte'range) =>
              reg_discri	<= dataLocalBusIn(kWidthDiscriReg-1 downto 0);
            when others => null;
          end case;
          state_lbus	<= Done;

        when Read =>
          case addrLocalBus(kNonMultiByte'range) is
            when kSelDiscri(kNonMultiByte'range) =>
              dataLocalBusOut <= '0' & reg_discri;
            when others =>
              dataLocalBusOut <= x"ff";
          end case;
          state_lbus	<= Done;

        when Done =>
          readyLocalBus	<= '1';
          if(weLocalBus = '0' and reLocalBus = '0') then
            state_lbus	<= Idle;
          end if;

        -- probably this is error --
        when others =>
          state_lbus	<= Init;
      end case;
    end if;
  end process u_BusProcess;

  -- Reset sequence --
  u_reset_gen_sys   : entity mylib.ResetGen
    port map(rst, clk, sync_reset);

end RTL;

