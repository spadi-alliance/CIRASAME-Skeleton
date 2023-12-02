LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

library mylib;
use mylib.defAD9220.all;
use mylib.defBCT.all;

entity AD9220 is
  generic(
    freqSysClk  : integer := 50_000_000; --input clock speed from user logic in Hz
    freqBusClk  : integer := 1_000_000;  --speed the i2c bus (scl) will run at in Hz
    enDebug     : boolean := false
    );
  port(
    -- System --
    clk       : in  std_logic;                    -- system clock
    reset     : in  std_logic;                    -- active high reset
    busyOut   : out std_logic;                    -- ADQ DAQ sequence busy
    startIn   : in std_logic;                     -- Sequence start

    -- CITIROC --
    readClk   : out std_logic;                    -- READ_CLK of CITIROC
    readIn    : out std_logic;                    -- READ_IN of CITIROC

    -- AD9220 --
    adcClk    : out std_logic;                    -- AD9220_CLK
    adcBit    : in std_logic_vector(kWdithAdcData-1 downto 0); -- AD9220_BIT
    adcOTR    : in std_logic;                     -- AD9220_OTR

    -- Local bus --
    addrLocalBus	      : in LocalAddressType;
    dataLocalBusIn	    : in LocalBusInType;
    dataLocalBusOut	    : out LocalBusOutType;
    reLocalBus		      : in std_logic;
    weLocalBus		      : in std_logic;
    readyLocalBus	      : out std_logic
    );
end AD9220;

architecture RTL of AD9220 is
  attribute mark_debug  : boolean;

  -- System --
  signal sync_reset           : std_logic;

  signal reg_busy             : std_logic;
  signal reg_start            : std_logic;

  constant divider     : integer := (freqSysClk/freqBusClk)/4; --number of clocks in 1/4 cycle of sck
  signal data_clk      : std_logic;
  signal data_clk_prev : std_logic;

  -- CITIROC read bit sequence --
  type ReadSeqType is(Idle, StartSeq, Command, StopSeq, Finalize);
  signal state_read    : ReadSeqType;

  signal read_clk      : std_logic;
  signal read_clk_prev : std_logic;
  signal read_clk_ena  : std_logic := '0';
  signal read_in_bit   : std_logic;
  signal bit_cnt_read  : integer RANGE 0 TO kReadLength-1 := kReadLength-1;

  -- AD9220 sequence --
  type AdcSeqType is(Idle, WaitLatency, Command, StopSeq, Finalize);
  signal state_adc  : AdcSeqType;

  signal adc_clk       : std_logic;
  signal adc_clk_prev  : std_logic;
  signal adc_clk_ena   : std_logic := '0';
  signal bit_cnt_adc   : integer RANGE 0 TO kReadLength+kAdcLatency-1 := kReadLength+kAdcLatency-1;

  signal en_adc_fifo   : std_logic;
  signal adc_data      : std_logic_vector(adcBit'range);
  signal reg_adc_data  : std_logic_vector(adcBit'range);
  signal reg_adc_otr   : std_logic;

  -- DAQ seq --
  signal adc_daq_gate   : std_logic;
  signal adc_daq_busy   : std_logic;
  signal adc_clk_ena_edge   : std_logic_vector(1 downto 0);
  signal event_num      : integer range 0 to 20;
  signal release_busy   : std_logic;

  -- ADC FIFO --
  signal fifo_reset         : std_logic;
  signal we_fifo, re_fifo   : std_logic;
  signal din_fifo           : std_logic_vector(15 downto 0);
  signal dout_fifo          : std_logic_vector(7 downto 0);
  signal empty_fifo, rv_fifo  : std_logic;
  COMPONENT adc_fifo
  PORT (
    clk : IN STD_LOGIC;
    srst : IN STD_LOGIC;
    din : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    wr_en : IN STD_LOGIC;
    rd_en : IN STD_LOGIC;
    dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    full : OUT STD_LOGIC;
    empty : OUT STD_LOGIC;
    valid : OUT STD_LOGIC
  );
  END COMPONENT;

  -- Local bus --
  type ADCBusProcessType is (
    Init, Idle, Connect,
    Write, Read,
    ReadFIFO,
    ReleaseBusy,
    Finalize,
    Done
    );
  signal state_lbus   : ADCBusProcessType;

  -- debug --
  attribute mark_debug of adc_clk        : signal is enDebug;
  attribute mark_debug of reg_adc_data   : signal is enDebug;
  attribute mark_debug of reg_adc_otr    : signal is enDebug;

  attribute mark_debug of read_in_bit    : signal is enDebug;
  attribute mark_debug of read_clk       : signal is enDebug;

  attribute mark_debug of data_clk       : signal is enDebug;

  attribute mark_debug of read_clk_ena   : signal is enDebug;
  attribute mark_debug of adc_clk_ena    : signal is enDebug;
  attribute mark_debug of en_adc_fifo    : signal is enDebug;

  attribute mark_debug of reg_start      : signal is enDebug;
  attribute mark_debug of reg_busy       : signal is enDebug;

  attribute mark_debug of adc_daq_gate   : signal is enDebug;
  attribute mark_debug of adc_daq_busy   : signal is enDebug;
  attribute mark_debug of event_num      : signal is enDebug;
  attribute mark_debug of release_busy   : signal is enDebug;

  -- ================================== body ======================================
begin

  -- port connection --
  busyOut   <= reg_busy;

  readIn    <= read_in_bit;
  readClk   <= '1' when (read_clk_ena = '1' AND read_clk = '1') else '0';
  adcClk    <= '1' when (adc_clk_ena = '1' AND adc_clk = '1') else '0';

  -- Clock domain crossing --
  u_start_latch : process(clk, sync_reset)
  begin
    if(sync_reset = '1') then
      reg_start   <= '0';
    elsif(clk'event and clk = '1') then
      if(startIn = '1' and adc_daq_gate = '1' and adc_daq_busy = '0') then
        reg_start   <= '1';
      elsif(reg_start = '1' and reg_busy = '1') then
        reg_start   <= '0';
      end if;
    end if;
  end process;

  --generate the timing for the bus clock (read_clk) and the data clock (data_clk)
  process(clk, sync_reset)
    VARIABLE count  :  integer RANGE 0 TO divider*4;
  begin
    if(sync_reset = '1') THEN
      count := 0;
    ELSIF(clk'event AND clk = '1') THEN
      data_clk_prev  <= data_clk;
      read_clk_prev  <= read_clk;
      adc_clk_prev    <= adc_clk;
      if(count = divider*4-1) THEN
        count := 0;
      else
        count := count + 1;
      end if;
      case count is
        when 0 TO divider-1 =>
          read_clk    <= '0';
          adc_clk     <= '1';
          data_clk    <= '0';
        when divider TO divider*2-1 =>
          read_clk    <= '0';
          adc_clk     <= '1';
          data_clk    <= '1';
        when divider*2 TO divider*3-1 =>
          read_clk    <= '1';
          adc_clk     <= '0';
          data_clk    <= '1';
        when OTHERS =>
          read_clk    <= '1';
          adc_clk     <= '0';
          data_clk    <= '0';
      end case;
    end if;
  end process;

  --state_read machine and writing to sda during sck low (data_clk rising edge)
  u_read_seq : process(clk, sync_reset)
  begin
    if(sync_reset = '1') THEN
      state_read     <= Idle;
      reg_busy       <= '0';
      read_clk_ena   <= '0';
      read_in_bit    <= '0';
      bit_cnt_read   <= kReadLength-1;
    ELSIF(clk'event AND clk = '1') THEN
      if(data_clk = '1' AND data_clk_prev  = '0') THEN
        case state_read is
          when Idle =>
            if(reg_start = '1') THEN
              reg_busy    <= '1';
              state_read  <= StartSeq;
            else
              reg_busy    <= '0';
              read_in_bit <= '0';
              state_read  <= Idle;
            end if;
          when StartSeq =>
            reg_busy      <= '1';
            read_clk_ena  <= '1';
            read_in_bit   <= '1';
            state_read    <= Command;
          when Command =>
            read_in_bit   <= '0';
            if(bit_cnt_read = 0) THEN
              bit_cnt_read <= kReadLength-1;
              read_clk_ena <= '0';
              state_read   <= StopSeq;
            else
              bit_cnt_read <= bit_cnt_read - 1;
              state_read   <= Command;
            end if;
          when StopSeq =>
            state_read     <= Finalize;
          when Finalize =>
            reg_busy       <= '0';
            state_read     <= Idle;
        end case;
      end if;
    end if;
  end process;


  u_adc_seq : process(clk, sync_reset)
  begin
    if(sync_reset = '1') THEN
      state_adc     <= Idle;
      adc_clk_ena   <= '0';
      en_adc_fifo   <= '0';
      bit_cnt_adc   <= kReadLength+kAdcLatency-1;
    ELSIF(clk'event AND clk = '1') THEN
      if(read_clk = '1' AND read_clk_prev  = '0') THEN
        case state_adc is
          when Idle =>
            if(read_clk_ena = '1') THEN
              adc_clk_ena <= '1';
              bit_cnt_adc <= kAdcLatency-1;
              state_adc   <= WaitLatency;
            else
              state_adc   <= Idle;
            end if;
          when WaitLatency =>
            if(bit_cnt_adc = 0) THEN
              en_adc_fifo <= '1';
              bit_cnt_adc <= kReadLength-1;
              state_adc   <= Command;
            else
              bit_cnt_adc <= bit_cnt_adc - 1;
              state_adc   <= WaitLatency;
            end if;
          when Command =>
            if(bit_cnt_adc = 0) THEN
              bit_cnt_adc <= kReadLength-1;
              adc_clk_ena   <= '0';
              state_adc   <= StopSeq;
            else
              bit_cnt_adc <= bit_cnt_adc - 1;
              state_adc   <= Command;
            end if;
          when StopSeq =>
            en_adc_fifo   <= '0';
            state_adc     <= Finalize;
          when Finalize =>
            state_adc     <= Idle;
        end case;
      end if;
    end if;
  end process;

  -- ADC data latch --
  adc_data(0)   <= adcBit(11);
  adc_data(1)   <= adcBit(10);
  adc_data(2)   <= adcBit(9);
  adc_data(3)   <= adcBit(8);
  adc_data(4)   <= adcBit(7);
  adc_data(5)   <= adcBit(6);
  adc_data(6)   <= adcBit(5);
  adc_data(7)   <= adcBit(4);
  adc_data(8)   <= adcBit(3);
  adc_data(9)   <= adcBit(2);
  adc_data(10)  <= adcBit(1);
  adc_data(11)  <= adcBit(0);

  process(clk)
  begin
    if(clk'event AND clk = '1') THEN
      if(adc_clk = '0' AND adc_clk_prev  = '1' and adc_clk_ena = '1' and en_adc_fifo = '1') then
        we_fifo        <= '1';
        reg_adc_data   <= adc_data;
        reg_adc_otr    <= adcOTR;
      else
        we_fifo        <= '0';
      end if;
    end if;
  end process;


  fifo_reset  <= sync_reset or (not adc_daq_gate);
  din_fifo  <= reg_adc_data(7 downto 0) & "000" & reg_adc_otr & reg_adc_data(11 downto 8);
  u_adc_fifo : adc_fifo
    port map (
      clk     => clk,
      srst    => fifo_reset,
      din     => din_fifo,
      wr_en   => we_fifo,
      rd_en   => re_fifo,
      dout    => dout_fifo,
      full    => open,
      empty   => empty_fifo,
      valid   => rv_fifo
  );

  u_event_seq : process(clk, sync_reset)
  begin
    if(sync_reset = '1') then
      adc_daq_busy  <= '0';
      event_num     <= 0;
    elsif(clk'event and clk = '1') then
      adc_clk_ena_edge  <= adc_clk_ena_edge(0) & adc_clk_ena;

      if(release_busy = '1') then
        event_num  <= 0;
      elsif(adc_clk_ena_edge = "01") then
        event_num  <= event_num +1;
      end if;

      if(release_busy = '1') then
        adc_daq_busy  <= '0';
      elsif(release_busy = '0' and event_num = 1) then
        adc_daq_busy  <= '1';
      end if;
    end if;
  end process;



  -- Local bus process ------------------------------------------------
  u_BusProcess : process(clk, sync_reset)
  begin
    if(sync_reset = '1') then
      adc_daq_gate     <= '0';
      re_fifo          <= '0';

      state_lbus	     <= Init;
    elsif(clk'event and clk = '1') then
      case state_lbus is
        when Init =>
        adc_daq_gate       <= '0';
          re_fifo          <= '0';
          dataLocalBusOut  <= x"00";
          readyLocalBus		 <= '0';
          state_lbus		   <= Idle;

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
            when kAdcDaqGate(kNonMultiByte'range) =>
              adc_daq_gate  <= dataLocalBusIn(0);
              state_lbus	  <= Done;

            when kReleaseBusy(kNonMultiByte'range) =>
              state_lbus  <= ReleaseBusy;

            when others =>
              state_lbus	<= Done;
          end case;

        when Read =>
          case addrLocalBus(kNonMultiByte'range) is
            when kAdcDaqGate(kNonMultiByte'range) =>
              dataLocalBusOut <= B"0000_000" & adc_daq_gate;
              state_lbus	    <= Done;

            when kEventFull(kNonMultiByte'range) =>
              dataLocalBusOut <= B"0000_000" & adc_daq_busy;
              state_lbus	    <= Done;

            when kReadFIFO(kNonMultiByte'range) =>
              if(empty_fifo = '1') then
                dataLocalBusOut   <= X"ee";
                state_lbus        <= Done;
              else
                re_fifo        <= '1';
                state_lbus	      <= ReadFIFO;
              end if;

            when others =>
              dataLocalBusOut   <= X"ee";
              state_lbus        <= Done;
          end case;

        when ReadFIFO =>
          re_fifo <= '0';
          if(rv_fifo = '1') then
            dataLocalBusOut   <= dout_fifo;
            state_lbus        <= Done;
          end if;

        when ReleaseBusy =>
          release_busy  <= '1';
          state_lbus    <= Finalize;

        when Finalize =>
          release_busy  <= '0';
          state_lbus    <= Done;

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
