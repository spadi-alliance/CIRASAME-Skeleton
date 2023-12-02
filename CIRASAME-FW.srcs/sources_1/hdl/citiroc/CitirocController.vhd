library IEEE, mylib;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use mylib.defBCT.all;
use mylib.defCitiroc.all;
use mylib.defToplevel.all;

Library xpm;
use xpm.vcomponents.all;

entity CitirocController is
  port(
    rst	                : in std_logic;
    clk	                : in std_logic;
    clk_machine         : in std_logic;
    -- Local bus --
    addrLocalBus        : in LocalAddressType;
    dataLocalBusIn      : in LocalBusInType;
    dataLocalBusOut	: out LocalBusOutType;
    reLocalBus          : in std_logic;
    weLocalBus          : in std_logic;
    readyLocalBus	: out std_logic;
    -- Module output --
    clk_sr              : out std_logic;
    clk_read            : out std_logic;
    srin_sr             : out std_logic;
    srin_read           : out std_logic;
    direct_control1     : out std_logic_vector(7 downto 0);
    direct_control2     : out std_logic_vector(7 downto 0)
    );
end CitirocController;

architecture RTL of CitirocController is
  -- internal signal declaration ----------------------------------------
  attribute mark_debug  : string;

  -- Local Bus ----------------------------------------------------------
  signal state_tbus	: BusProcessType;
  signal we_fifo        : std_logic := '0';
  signal en_control     : std_logic := '0';
  signal reg_control    : std_logic_vector(7 downto 0) := "00000000";
  signal start_cycle    : std_logic := '0';

  -- State Machine ------------------------------------------------------
  signal state_sc       : SlowControlProcessType;
  signal rd_fifo        : std_logic;
  signal en_count       : std_logic;
  signal clear_count    : std_logic := '0';
  signal clk_shift      : std_logic;
  signal end_cycle      : std_logic;
  signal count          : std_logic_vector(3 downto 0) := "0000";

  -- Flag ---------------------------------------------------------------
  signal sync_start_cycle    : std_logic := '0';
  signal sync_start_cycle_pulse : std_logic := '0';
  signal sync_start_cycle_machine : std_logic := '0';
  signal start_cycle_mem     : std_logic := '0';
  signal sel_w_sr       : std_logic := '0';
  signal sel_w_sr_pulse      : std_logic := '0';
  signal sel_w_sr_machine    : std_logic := '0';
  signal rst_sr         : std_logic := '0';
  signal sel_w_read     : std_logic := '0';
  signal sel_w_read_pulse    : std_logic := '0';
  signal sel_w_read_machine  : std_logic := '0';
  signal rst_read       : std_logic := '0';
  signal mem_w_sr       : std_logic := '0';
  signal mem_w_read     : std_logic := '0';
  signal mem_valid      : std_logic;
  signal n_cycle        : std_logic := '0';

  -- FIFO ---------------------------------------------------------------
  signal q_sig          : std_logic_vector(7 downto 0);
  signal out_shift      : std_logic;
  signal full           : std_logic;
  signal empty          : std_logic;
  signal valid          : std_logic;
  signal wr_rst_busy    : std_logic;
  signal rd_rst_busy    : std_logic;

  -- Signals -----------------------------------------------------------
  signal clk_sr0        : std_logic;
  signal clk_read0      : std_logic;
  signal srin_sr0       : std_logic;
  signal srin_read0     : std_logic;

--  attribute mark_debug of en_control : signal is "true";
--  attribute mark_debug of sel_w_sr : signal is "true";
--  attribute mark_debug of sel_w_read : signal is "true";
--  attribute mark_debug of sync_start_cycle : signal is "true";
--
--  attribute mark_debug of sync_start_cycle_machine : signal is "true";
--  -- attribute mark_debug of sync_start_cycle_pulse : signal is "true";
--  attribute mark_debug of state_sc : signal is "true";
--
--  attribute mark_debug of count : signal is "true";
--  attribute mark_debug of clear_count : signal is "true";
--
--  attribute mark_debug of clk_sr : signal is "true";
--  attribute mark_debug of srin_sr : signal is "true";
--  attribute mark_debug of clk_read : signal is "true";
--  attribute mark_debug of srin_read : signal is "true";
--  attribute mark_debug of sel_w_sr_machine : signal is "true";
--  attribute mark_debug of sel_w_read_machine : signal is "true";
--  attribute mark_debug of mem_w_sr : signal is "true";
--  attribute mark_debug of mem_w_read : signal is "true";
--  attribute mark_debug of start_cycle_mem : signal is "true";
--  attribute mark_debug of mem_valid : signal is "true";
--  attribute mark_debug of empty : signal is "true";
--
--  attribute mark_debug of we_fifo : signal is "true";
--  attribute mark_debug of rd_fifo : signal is "true";

  -- component declaration ---------------------------------------------
  component EdgeDetector
    port(
      rst    : in std_logic;
      clk    : in std_logic;
      dIn    : in std_logic;
      dOut   : out std_logic
      );
  end component;

  component JKFF
    port(
      ARST   : in std_logic;
      J      : in std_logic;
      K      : in std_logic;
      CLK    : in std_logic;
      Q      : out std_logic
      );
  end component;

  COMPONENT dual_clk_FIFO
    PORT (
      rst : IN STD_LOGIC;
      wr_clk : IN STD_LOGIC;
      rd_clk : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
      full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      valid : OUT STD_LOGIC;
      wr_rst_busy : OUT STD_LOGIC;
      rd_rst_busy : OUT STD_LOGIC
    );
  END COMPONENT;

-- =============================== body ===============================
begin
  u_SlowControlProcess : process(clk_machine, rst)
  begin
    if(rst = '1') then
      en_count <= '0';
      rd_fifo <= '0';
      clear_count <= '1';
      clk_shift <= '0';
      end_cycle <= '0';
      state_sc          <= Idle;
    elsif(clk_machine'event and clk_machine = '1') then
      case state_sc is
        when Idle =>
          en_count <= '0';
          clk_shift <= '0';
          if start_cycle_mem = '0' then
            state_sc <= Idle;
          else
            rd_fifo <= '1';
            clear_count <= '0';
            state_sc <= ReadFIFO;
          end if;

        when ReadFIFO =>
          en_count <= '1';
          rd_fifo <= '0';
          state_sc <= StartCount;

        when StartCount =>
          en_count <= '0';
          state_sc <= IdleCount;

        when IdleCount =>
          if(empty = '1' and count = "1000") then
            en_count <= '0';
            clk_shift <= '1';
            state_sc <= EmptyFIFO;
          else
            en_count <= '1';
            clk_shift <= '1';
            state_sc <= DoCount;
          end if;

        when DoCount =>
          en_count <= '0';
          clk_shift <= '0';
          if(empty = '0' and count = "1000") then
            rd_fifo <= '1';
            clear_count <= '1';
            state_sc <= ResetCount;
          else
            state_sc <= IdleCount;
          end if;

        when ResetCount =>
          en_count <= '1';
          rd_fifo <= '0';
          clear_count <= '0';
          state_sc <= StartCount;

        when EmptyFIFO =>
          rd_fifo <= '0';
          clk_shift <= '0';
          clear_count <= '1';
          end_cycle <= '1';
          state_sc <= EndCycle;

        when EndCycle =>
          clear_count <= '1';
          end_cycle <= '0';
          state_sc <= Idle;

        when others =>
          clear_count <= '1';
          state_sc <= Idle;
      end case;
    end if;
  end process u_SlowControlProcess;

  -- with state_sc select
  --   rd_fifo <= '1' when ReadFIFO | ResetCount,
  --              '0' when others;

  -- with state_sc select
  --   en_count <= '1' when StartCount | DoCount,
  --               '0' when others;

  -- with state_sc select
  --   clear_count <= '1' when Idle | ResetCount | EndCycle,
  --                  '0' when others;

  -- with state_sc select
  --   clk_shift <= '1' when DoCount | EmptyFIFO,
  --                '0' when others;

  -- with state_sc select
  --   end_cycle <= '1' when EndCycle,
  --                '0' when others;

  u_CountProcess : process(clk_machine, rst)
  begin
    if(rst = '1') then
      count <= (others => '0');
    elsif(clk_machine'event and clk_machine = '1') then
      if(clear_count = '1') then
        count <= (others => '0');
      elsif(en_count = '1') then
        count <= count + "0001";
      end if;
    end if;
  end process u_CountProcess;

  u_SyncCycle : EdgeDetector port map(
    rst     => rst,
    clk     => clk,
    dIn     => reg_control(0),
    dOut    => sync_start_cycle
    );

  u_PulseStartCycle : xpm_cdc_pulse generic map (
      DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      REG_OUTPUT => 0,     -- DECIMAL; 0=disable registered output, 1=enable registered output
      RST_USED => 0,       -- DECIMAL; 0=no reset, 1=implement reset
      SIM_ASSERT_CHK => 0  -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
   )
   port map (
      dest_pulse => sync_start_cycle_pulse, -- 1-bit output: Outputs a pulse the size of one dest_clk period when a pulse
                                -- transfer is correctly initiated on src_pulse input. This output is
                                -- combinatorial unless REG_OUTPUT is set to 1.

      dest_clk => clk_machine,     -- 1-bit input: Destination clock.
      dest_rst => '0',     -- 1-bit input: optional; required when RST_USED = 1
      src_clk => clk,       -- 1-bit input: Source clock.
      src_pulse => sync_start_cycle,   -- 1-bit input: Rising edge of this signal initiates a pulse transfer to the
                                -- destination clock domain. The minimum gap between each pulse transfer must
                                -- be at the minimum 2*(larger(src_clk period, dest_clk period)). This is
                                -- measured between the falling edge of a src_pulse to the rising edge of the
                                -- next src_pulse. This minimum gap will guarantee that each rising edge of
                                -- src_pulse will generate a pulse the size of one dest_clk period in the
                                -- destination clock domain. When RST_USED = 1, pulse transfers will not be
                                -- guaranteed while src_rst and/or dest_rst are asserted.

      src_rst => '0'        -- 1-bit input: optional; required when RST_USED = 1
   );

  u_SynchroStartCycle : xpm_cdc_single generic map (
      DEST_SYNC_FF   => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF   => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      SIM_ASSERT_CHK => 0, -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      SRC_INPUT_REG  => 1   -- DECIMAL; 0=do not register input, 1=register input
   )
   port map (
      dest_out     => sync_start_cycle_machine, -- 1-bit output: src_in synchronized to the destination clock domain. This output is registered.
      dest_clk     => clk_machine, -- 1-bit input: Clock signal for the destination clock domain.
      src_clk      => clk,   -- 1-bit input: optional; required when SRC_INPUT_REG = 1
      src_in       => sync_start_cycle_pulse      -- 1-bit input: Input signal to be synchronized to dest_clk domain.
   );

  u_MemCycle : JKFF port map(
    ARST    => rst,
    J       => sync_start_cycle_machine,
    K       => end_cycle,
    CLK     => clk_machine,
    Q       => start_cycle_mem
    );

  u_MakeSCFlagProcess : process(en_control, end_cycle)
  begin
    -- if(rst = '1') then
    --   sel_w_sr <= '0';
    --   sel_w_read <= '0';
    if(en_control = '1') then
      if(reg_control(1) = '0') then
        sel_w_sr   <= '1';
        sel_w_read <= '0';
      else
        sel_w_sr   <= '0';
        sel_w_read <= '1';
      end if;
    elsif(end_cycle = '1') then
      sel_w_sr <= '0';
      sel_w_read <= '0';
    end if;
  end process u_MakeSCFlagProcess;

  u_PulseSC : xpm_cdc_pulse generic map (
      DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      REG_OUTPUT => 0,     -- DECIMAL; 0=disable registered output, 1=enable registered output
      RST_USED => 0,       -- DECIMAL; 0=no reset, 1=implement reset
      SIM_ASSERT_CHK => 0  -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
   )
   port map (
      dest_pulse => sel_w_sr_pulse, -- 1-bit output: Outputs a pulse the size of one dest_clk period when a pulse
                                -- transfer is correctly initiated on src_pulse input. This output is
                                -- combinatorial unless REG_OUTPUT is set to 1.

      dest_clk => clk_machine,     -- 1-bit input: Destination clock.
      dest_rst => '0',     -- 1-bit input: optional; required when RST_USED = 1
      src_clk => clk,       -- 1-bit input: Source clock.
      src_pulse => sel_w_sr,   -- 1-bit input: Rising edge of this signal initiates a pulse transfer to the
                                -- destination clock domain. The minimum gap between each pulse transfer must
                                -- be at the minimum 2*(larger(src_clk period, dest_clk period)). This is
                                -- measured between the falling edge of a src_pulse to the rising edge of the
                                -- next src_pulse. This minimum gap will guarantee that each rising edge of
                                -- src_pulse will generate a pulse the size of one dest_clk period in the
                                -- destination clock domain. When RST_USED = 1, pulse transfers will not be
                                -- guaranteed while src_rst and/or dest_rst are asserted.

      src_rst => '0'        -- 1-bit input: optional; required when RST_USED = 1
   );

  u_SynchroSC : xpm_cdc_single generic map (
      DEST_SYNC_FF   => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF   => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      SIM_ASSERT_CHK => 0, -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      SRC_INPUT_REG  => 1   -- DECIMAL; 0=do not register input, 1=register input
   )
   port map (
      dest_out     => sel_w_sr_machine, -- 1-bit output: src_in synchronized to the destination clock domain. This output is registered.
      dest_clk     => clk_machine, -- 1-bit input: Clock signal for the destination clock domain.
      src_clk      => clk,   -- 1-bit input: optional; required when SRC_INPUT_REG = 1
      src_in       => sel_w_sr_pulse      -- 1-bit input: Input signal to be synchronized to dest_clk domain.
   );

  u_PulseReadSC : xpm_cdc_pulse generic map (
      DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      REG_OUTPUT => 0,     -- DECIMAL; 0=disable registered output, 1=enable registered output
      RST_USED => 0,       -- DECIMAL; 0=no reset, 1=implement reset
      SIM_ASSERT_CHK => 0  -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
   )
   port map (
      dest_pulse => sel_w_read_pulse, -- 1-bit output: Outputs a pulse the size of one dest_clk period when a pulse
                                -- transfer is correctly initiated on src_pulse input. This output is
                                -- combinatorial unless REG_OUTPUT is set to 1.

      dest_clk => clk_machine,     -- 1-bit input: Destination clock.
      dest_rst => '0',     -- 1-bit input: optional; required when RST_USED = 1
      src_clk => clk,       -- 1-bit input: Source clock.
      src_pulse => sel_w_read,   -- 1-bit input: Rising edge of this signal initiates a pulse transfer to the
                                -- destination clock domain. The minimum gap between each pulse transfer must
                                -- be at the minimum 2*(larger(src_clk period, dest_clk period)). This is
                                -- measured between the falling edge of a src_pulse to the rising edge of the
                                -- next src_pulse. This minimum gap will guarantee that each rising edge of
                                -- src_pulse will generate a pulse the size of one dest_clk period in the
                                -- destination clock domain. When RST_USED = 1, pulse transfers will not be
                                -- guaranteed while src_rst and/or dest_rst are asserted.

      src_rst => '0'        -- 1-bit input: optional; required when RST_USED = 1
   );

   u_SynchroReadSC : xpm_cdc_single generic map (
      DEST_SYNC_FF   => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF   => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      SIM_ASSERT_CHK => 0, -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      SRC_INPUT_REG  => 1   -- DECIMAL; 0=do not register input, 1=register input
   )
   port map (
      dest_out     => sel_w_read_machine, -- 1-bit output: src_in synchronized to the destination clock domain. This output is registered.
      dest_clk     => clk_machine, -- 1-bit input: Clock signal for the destination clock domain.
      src_clk      => clk,   -- 1-bit input: optional; required when SRC_INPUT_REG = 1
      src_in       => sel_w_read_pulse      -- 1-bit input: Input signal to be synchronized to dest_clk domain.
   );

  rst_sr    <= end_cycle or sel_w_read_machine;
  rst_read  <= end_cycle or sel_w_sr_machine;

  u_SCFlag : JKFF port map(
    ARST    => rst,
    J       => sel_w_sr_machine,
    K       => rst_sr,
    CLK     => clk_machine,
    Q       => mem_w_sr
    );

  u_ReadSCFlag : JKFF port map(
    ARST    => rst,
    J       => sel_w_read_machine,
    K       => rst_read,
    CLK     => clk_machine,
    Q       => mem_w_read
    );

  u_DualClkFIFO : dual_clk_FIFO PORT MAP (
    rst => rst,
    wr_clk => clk,
    rd_clk => clk_machine,
    din => dataLocalBusIn,
    wr_en => we_fifo,
    rd_en => rd_fifo,
    dout => q_sig,
    full => full,
    empty => empty,
    valid => valid,
    wr_rst_busy => wr_rst_busy,
    rd_rst_busy => rd_rst_busy
    );

  u_MemValidProcess : JKFF port map(
    ARST    => rst,
    J       => valid,
    K       => clear_count,
    CLK     => clk_machine,
    Q       => mem_valid
  );

  u_OutShiftProcess : process(count)
  begin
    if(mem_valid = '1')then
      case count is
        when "0001" => out_shift <= q_sig(0);
        when "0010" => out_shift <= q_sig(1);
        when "0011" => out_shift <= q_sig(2);
        when "0100" => out_shift <= q_sig(3);
        when "0101" => out_shift <= q_sig(4);
        when "0110" => out_shift <= q_sig(5);
        when "0111" => out_shift <= q_sig(6);
        when "1000" => out_shift <= q_sig(7);
        when others => out_shift <= '0';
      end case;
    else
      out_shift <= '0';
    end if;
  end process u_OutShiftProcess;

  -- create slow control signal
  clk_sr0   <= (clk_shift and mem_w_sr and start_cycle_mem);
  clk_sr    <= clk_sr0;
  srin_sr0  <= (out_shift and mem_w_sr and start_cycle_mem);
  srin_sr   <= srin_sr0;
  clk_read0 <= (clk_shift and mem_w_read and start_cycle_mem);
  clk_read  <= clk_read0;
  srin_read0      <= (out_shift and mem_w_read and start_cycle_mem);
  srin_read       <= srin_read0;

  u_BusProcess : process(clk, rst)
  begin
    if(rst = '1') then
      state_tbus	<= Init;
      we_fifo           <= '0';
      start_cycle       <= '0';
      en_control        <= '0';
      reg_control       <= "00000000";
      direct_control1   <= "00000000";
      direct_control2   <= "00000000";
    elsif(clk'event and clk = '1') then
      case state_tbus is
        when Init =>
          dataLocalBusOut       <= x"00";
          readyLocalBus		<= '0';
          we_fifo               <= '0';
          en_control            <= '0';
          reg_control           <= "00000000";
          direct_control1   <= "00000000";
          direct_control2   <= "00000000";
          state_tbus		<= Idle;

        when Idle =>
          readyLocalBus	<= '0';
          if(weLocalBus = '1' or reLocalBus = '1') then
            state_tbus	<= Connect;
          end if;

        when Connect =>
          if(weLocalBus = '1') then
            state_tbus	<= Write;
          else
            state_tbus	<= Read;
          end if;

        when Write =>
          case addrLocalBus(kNonMultiByte'range) is
            when kAddrSCFIFO(kNonMultiByte'range) =>
              we_fifo <= '1';
              en_control <= '0';
            when kAddrPDC(kNonMultiByte'range) =>
              we_fifo <= '0';
              en_control <= '0';
              if(addrLocalBus(kMultiByte'range) = "0000") then
                direct_control1 <= dataLocalBusIn;
              elsif(addrLocalBus(kMultiByte'range) = "0001") then
                direct_control2 <= dataLocalBusIn;
              end if;
            when kAddrCC(kNonMultiByte'range) =>
              we_fifo <= '0';
              en_control <= '1';
              reg_control <= dataLocalBusIn;
            when others =>
              we_fifo <= '0';
          end case;
          state_tbus	<= Done;

        when Read =>
          case addrLocalBus(kNonMultiByte'range) is
            when kAddrSCFIFO(kNonMultiByte'range) =>
              dataLocalBusOut <= x"00";
            when kAddrPDC(kNonMultiByte'range) =>
              dataLocalBusOut <= x"01";
            when kAddrCC(kNonMultiByte'range) =>
              dataLocalBusOut <= x"02";
            when others =>
              dataLocalBusOut <= x"FF";
          end case;
          state_tbus	<= Done;

        when Done =>
          we_fifo <= '0';
          en_control <= '0';
          reg_control <= "00000000";
          start_cycle <= '0';
          readyLocalBus	<= '1';
          if(weLocalBus = '0' and reLocalBus = '0') then
            state_tbus	<= Idle;
          end if;

        -- probably this is error --
        when others =>
          state_tbus	<= Init;
      end case;
    end if;
  end process u_BusProcess;

end RTL;

