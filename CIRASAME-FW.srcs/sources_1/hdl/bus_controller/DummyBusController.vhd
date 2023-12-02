-----------------------------------------------------------------------------------------------
--
-- This module returns error data when SiTCP requests to access to local modules.
--
-----------------------------------------------------------------------------------------------
library ieee, mylib;
use ieee.std_logic_1164.all;
use mylib.defBCT.all;
use mylib.defRBCP.all;

entity DummyBusController is
  Port(
    rstSys    : in  std_logic;
    clk	      : in  std_logic;
    -- Local Bus --
    -- No local bus.
    -- RBCP --
--    addrRBCP              : in std_logic_vector(kWidthAddrRBCP-1 downto 0); -- address [31:0]
--    wdRBCP                : in std_logic_vector(kWidthDataRBCP-1 downto 0);  -- data from host [7:0]
    weRBCP    : in std_logic; 		    -- RBCP write enable
    reRBCP    : in std_logic; 		    -- RBCP read enable
    ackRBCP	  : out std_logic; 		    -- RBCP acknowledge
    rdRBCP    : out std_logic_vector(kWidthDataRBCP-1 downto 0)  -- data to host [7:0]
    );
end DummyBusController;

architecture RTL of DummyBusController is
  -- internal signal ---------------------------------------------------
  signal state_bus              : BusControlProcessType;

  -- external bus -------------------------------------------------------
--  signal data_ext_bus_in	      : LocalBusInType;
  signal data_ext_bus_out	      : LocalBusOutType;
  signal ack_ext_bus            : std_logic;

-- =============================== body ===============================
begin
  -- RBCP connection --
  rdRBCP	    <= data_ext_bus_out;
  ackRBCP	    <= ack_ext_bus;

  -- Bus control process --
  u_BusControlProcess : process (clk, rstSys)
  begin
    if(rstSys = '1') then
      data_ext_bus_out  <= x"00";
      ack_ext_bus	      <= '0';
      state_bus	        <= Init;
    elsif(clk'event and clk = '1') then
      case state_bus is
        when Init =>
          data_ext_bus_out  <= x"00";
          ack_ext_bus		    <= '0';
          state_bus		      <= Idle;

        when Idle =>
          if(reRBCP = '1' or weRBCP = '1') then
            state_bus		    <= SetBus;
          end if;

        when SetBus =>
          -- error state --
          data_ext_bus_out  <= x"fe";
          state_bus		      <= Done;

        when Done =>
          ack_ext_bus       <= '1';
          state_bus		      <= Init;

        when others =>
          state_bus         <= Init;

      end case;
    end if;
  end process u_BusControlProcess;

end RTL;

