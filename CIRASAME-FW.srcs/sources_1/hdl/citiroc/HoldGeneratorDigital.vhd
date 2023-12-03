LIBRARY ieee;
USE ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

library mylib;
use mylib.defHGD.all;
use mylib.defBCT.all;

Library xpm;
use xpm.vcomponents.all;

entity HoldGeneratorDigital is
  port(
    -- System --
    reset     : in std_logic; -- Async reset in
    clk       : in std_logic; -- System clock (100-125 MHz)
    clkFast   : in std_logic; -- 500 MHz fast clock

    -- CITIROC and AD9220 --
    holdIn    : in std_logic;  -- Async sig
    holdOut   : out std_logic; -- Sampled and delayed by 500 MHz clock
    startAdc  : out std_logic; -- AD9220 sequencer start request
    busyAdc   : in std_logic;  -- Busy from AD9220 sequencer

    -- Local bus --
    addrLocalBus	      : in LocalAddressType;
    dataLocalBusIn	    : in LocalBusInType;
    dataLocalBusOut	    : out LocalBusOutType;
    reLocalBus		      : in std_logic;
    weLocalBus		      : in std_logic;
    readyLocalBus	      : out std_logic
    );
end HoldGeneratorDigital;

architecture RTL of HoldGeneratorDigital is
  attribute mark_debug  : boolean;
  attribute keep        : boolean;

  -- System --
  signal sync_reset           : std_logic;

  -- Fast clock domain --
  signal sync_hold_fast       : std_logic;

  constant kWidthDelay        : positive:= 32;
  constant kAllZero           : std_logic_vector(kWidthDelay-1 downto 0):= (others => '0');
  signal reg_delay_line       : std_logic_vector(kAllZero'range);
  signal reg_bit_patt         : std_logic_vector(kAllZero'range);
  signal reg_mask             : std_logic_vector(kAllZero'range);
  signal delayed_hold         : std_logic;
  signal oneshot_hold_fast    : std_logic;

  -- Slow clock domain
  constant kWidthSr     : positive:= 256;
  signal oneshot_hold   : std_logic;
  signal delay_sr       : std_logic_vector(kWidthSr-1 downto 0);
  signal reset_hold     : std_logic;
  signal stretched_hold : std_logic;

  -- Local bus --
  signal state_lbus     : BusProcessType;

  attribute keep of reg_mask  : signal is true;

  -- debug --
  --attribute mark_debug of oneshot_hold       : signal is true;

  -- =============================== body ===============================
begin

  -- Port connection --
  holdOut   <= delayed_hold or stretched_hold or busyAdc;
  startAdc  <= oneshot_hold and (not busyAdc);

  -- Fast clock domain -------------------------------------------------------------------------
  u_sync : entity mylib.synchronizer port map(clkFast, holdIn, sync_hold_fast);
  u_digital_delay : process(clkFast)
  begin
    if(clkFast'event and clkFast = '1') then
      reg_delay_line  <= reg_delay_line(kWidthDelay-2 downto 0) & sync_hold_fast;
      reg_bit_patt    <= reg_delay_line and reg_mask;

      if(reg_bit_patt = kAllZero) then
        delayed_hold  <= '0';
      else
        delayed_hold  <= '1';
      end if;
    end if;
  end process;

  u_edge : entity mylib.EdgeDetector port map('0', clkFast, delayed_hold, oneshot_hold_fast);

  -- Clock domain crossing ---------------------------------------------------------------------
  u_cdc_hold : xpm_cdc_pulse
  generic map (
     DEST_SYNC_FF => 4,
     INIT_SYNC_FF => 0,
     REG_OUTPUT   => 1,
     RST_USED     => 0,
     SIM_ASSERT_CHK => 0
  )
  port map (
     dest_pulse => oneshot_hold,
     dest_clk   => clk,
     dest_rst   => '0',
     src_clk    => clkFast,
     src_pulse  => oneshot_hold_fast,
     src_rst    => '0'
  );

  -- System clock domain -----------------------------------------------------------------------
  u_selfgate : process(clk)
  begin
    if(clk'event and clk = '1') then
      if(reset_hold = '1') then
        stretched_hold  <= '0';
      elsif(reset_hold = '0' and oneshot_hold = '1') then
        stretched_hold  <= '1';
      end if;
    end if;
  end process;

  u_delay_sr : process(clk)
  begin
    if(clk'event and clk = '1') then
      delay_sr  <= delay_sr(kWidthSr-2 downto 0) & oneshot_hold;
    end if;
  end process;
  reset_hold  <= delay_sr(kWidthSr-1) or reset;

  -- Local bus --
  u_BusProcess : process(clk, sync_reset)
  begin
    if(sync_reset = '1') then
      state_lbus	<= Init;
    elsif(clk'event and clk = '1') then
      case state_lbus is
        when Init =>
          dataLocalBusOut <= x"00";
          readyLocalBus		<= '0';
          reg_mask    		<= (others => '0');
          state_lbus      <= Idle;

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
            when kDelayMask(kNonMultiByte'range) =>
            if( addrLocalBus(kMultiByte'range) = k1stByte) then
              reg_mask(7 downto 0)  <= dataLocalBusIn;
            elsif( addrLocalBus(kMultiByte'range) = k2ndByte) then
              reg_mask(15 downto 8)  <= dataLocalBusIn;
            elsif( addrLocalBus(kMultiByte'range) = k3rdByte) then
              reg_mask(23 downto 16)  <= dataLocalBusIn;
            elsif( addrLocalBus(kMultiByte'range) = k4thByte) then
              reg_mask(31 downto 24)  <= dataLocalBusIn;
            end if;

            when others => null;
          end case;
          state_lbus	<= Done;

        when Read =>
          case addrLocalBus(kNonMultiByte'range) is
            when kDelayMask(kNonMultiByte'range) =>
            if( addrLocalBus(kMultiByte'range) = k1stByte) then
              dataLocalBusOut   <= reg_mask(7 downto 0);
            elsif( addrLocalBus(kMultiByte'range) = k2ndByte) then
              dataLocalBusOut   <= reg_mask(15 downto 8);
            elsif( addrLocalBus(kMultiByte'range) = k3rdByte) then
              dataLocalBusOut   <= reg_mask(23 downto 16);
            elsif( addrLocalBus(kMultiByte'range) = k4thByte) then
              dataLocalBusOut   <= reg_mask(31 downto 24);
            end if;

            when others =>
              dataLocalBusOut <= x"ee";
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
    port map(reset, clk, sync_reset);



end RTL;
