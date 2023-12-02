library IEEE, mylib;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use mylib.defBCT.all;
use mylib.defMIG.all;

Library xpm;
use xpm.vcomponents.all;

entity MigDD3SDRAM is
  port(
    rst	                : in std_logic;
    clk	                : in std_logic;
    refClkIn            : in std_logic;
    xadcTempIn          : in std_logic_vector(11 downto 0);

    -- DDR3-SDRAM toplevel ports--
    DDR3_CK_P           : out std_logic_vector(0 downto 0);
    DDR3_CK_N           : out std_logic_vector(0 downto 0);
    DDR3_RESET_N        : out std_logic;
    DDR3_CKE            : out std_logic_vector(0 downto 0);
    DDR3_CS_N           : out std_logic_vector(0 downto 0);
    DDR3_RAS_N          : out std_logic;
    DDR3_CAS_N          : out std_logic;
    DDR3_WE_N           : out std_logic;
    DDR3_ODT            : out std_logic_vector(0 downto 0);
    DDR3_BA             : out std_logic_vector(2 downto 0);
    DDR3_ADDR           : out std_logic_vector(13 downto 0);
    DDR3_DQ             : inout std_logic_vector(15 downto 0);
    DDR3_DM             : out std_logic_vector(1 downto 0);
    DDR3_DQS_P          : inout std_logic_vector(1 downto 0);
    DDR3_DQS_N          : inout std_logic_vector(1 downto 0);
    MIGS_CLKP           : in std_logic;
    MIGS_CLKN           : in std_logic;

    -- User ports --
    dIn                 : in std_logic_vector(kWidthDaqData-1 downto 0);
    writeEnable         : in std_logic;
    progFull            : out std_logic;
    dOut                : out std_logic_vector(kWidthDaqData-1 downto 0);
    readEnable          : in std_logic;
    readValid           : out std_logic;
    emptyReadBuf        : out std_logic;

    -- Local bus --
    addrLocalBus	      : in LocalAddressType;
    dataLocalBusIn	    : in LocalBusInType;
    dataLocalBusOut	    : out LocalBusOutType;
    reLocalBus		      : in std_logic;
    weLocalBus		      : in std_logic;
    readyLocalBus	      : out std_logic
    );
end MigDD3SDRAM;

architecture RTL of MigDD3SDRAM is
  attribute mark_debug        : string;

  -- System --
  signal sync_reset           : std_logic;

  -- internal signal declaration --------------------------------------
  -- Pre-fetch process --
  type PfArray is array (integer range 0 to kLengthPF)
   of std_logic_vector(kWidthAppData-1 downto 0);
  signal pf_buffer            : PfArray;
  signal pf_valid             : std_logic_vector(kLengthPF downto 0);
  signal fetch_progressing    : std_logic;
  signal is_fetched           : std_logic;
  signal state_pf             : PreFetchType;

  -- MIG write buffer --
  signal din_write_buf        : std_logic_vector(kWidthDaqData-1 downto 0);
  signal dout_write_buf       : std_logic_vector(kWidthAppData-1 downto 0);
  signal wen_write_buf        : std_logic;
  signal ren_write_buf        : std_logic;
  signal rv_write_buf         : std_logic;
  signal wcount_write_buf     : std_logic_vector(9 downto 0);
  signal empty_write_buf      : std_logic;
  signal pfull_write_buf      : std_logic;
  COMPONENT mig_wbuf
    PORT (
      rst           : IN STD_LOGIC;
      wr_clk        : IN STD_LOGIC;
      rd_clk        : IN STD_LOGIC;
      din           : IN STD_LOGIC_VECTOR(kWidthDaqData-1 DOWNTO 0);
      wr_en         : IN STD_LOGIC;
      rd_en         : IN STD_LOGIC;
      dout          : OUT STD_LOGIC_VECTOR(kWidthAppData-1 DOWNTO 0);
      full          : OUT STD_LOGIC;
      empty         : OUT STD_LOGIC;
      valid         : OUT STD_LOGIC;
      rd_data_count : OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
      prog_full     : OUT STD_LOGIC;
      wr_rst_busy   : OUT STD_LOGIC;
      rd_rst_busy   : OUT STD_LOGIC
  );
END COMPONENT;

-- MIG read buffer --
signal din_read_buf        : std_logic_vector(kWidthAppData-1 downto 0);
signal dout_read_buf       : std_logic_vector(kWidthDaqData-1 downto 0);
signal wen_read_buf        : std_logic;
signal ren_read_buf        : std_logic;
signal rv_read_buf         : std_logic;
signal pfull_read_buf      : std_logic;
signal empty_read_buf      : std_logic;
COMPONENT mig_rbuf
  PORT (
    rst         : IN STD_LOGIC;
    wr_clk      : IN STD_LOGIC;
    rd_clk      : IN STD_LOGIC;
    din         : IN STD_LOGIC_VECTOR(kWidthAppData-1 DOWNTO 0);
    wr_en       : IN STD_LOGIC;
    rd_en       : IN STD_LOGIC;
    dout        : OUT STD_LOGIC_VECTOR(kWidthDaqData-1 DOWNTO 0);
    full        : OUT STD_LOGIC;
    empty       : OUT STD_LOGIC;
    valid       : OUT STD_LOGIC;
    prog_full   : OUT STD_LOGIC;
    wr_rst_busy : OUT STD_LOGIC;
    rd_rst_busy : OUT STD_LOGIC
  );
  END COMPONENT;

  -- MIG UI --
  signal app_addr   : std_logic_vector(kWidthAppAddr-1 downto 0);
  signal app_cmd    : std_logic_vector(kCmdWrite'range);
  signal app_en, app_rdy, app_wdf_rdy   : std_logic;

  signal app_wdf_data   : std_logic_vector(kWidthAppData-1 downto 0);
  signal app_wdf_end, app_wdf_wren  : std_logic;
  signal app_rd_data    : std_logic_vector(kWidthAppData-1 downto 0);
  signal app_rd_data_end, app_rd_data_valid   : std_logic;

  signal ui_clk           : std_logic;
  signal ui_clk_sync_rst  : std_logic;

  -- MIG control process --
  signal state_mig        : MigProcessType;
  signal move_to_pfidle   : std_logic;

  component mig_7series_0
    port (
      ddr3_dq       : inout std_logic_vector(15 downto 0);
      ddr3_dqs_p    : inout std_logic_vector(1 downto 0);
      ddr3_dqs_n    : inout std_logic_vector(1 downto 0);

      ddr3_addr     : out   std_logic_vector(13 downto 0);
      ddr3_ba       : out   std_logic_vector(2 downto 0);
      ddr3_ras_n    : out   std_logic;
      ddr3_cas_n    : out   std_logic;
      ddr3_we_n     : out   std_logic;
      ddr3_reset_n  : out   std_logic;
      ddr3_ck_p     : out   std_logic_vector(0 downto 0);
      ddr3_ck_n     : out   std_logic_vector(0 downto 0);
      ddr3_cke      : out   std_logic_vector(0 downto 0);
      ddr3_cs_n     : out   std_logic_vector(0 downto 0);
      ddr3_dm       : out   std_logic_vector(1 downto 0);
      ddr3_odt      : out   std_logic_vector(0 downto 0);
      app_addr                  : in    std_logic_vector(27 downto 0);
      app_cmd                   : in    std_logic_vector(2 downto 0);
      app_en                    : in    std_logic;
      app_wdf_data              : in    std_logic_vector(127 downto 0);
      app_wdf_end               : in    std_logic;
      app_wdf_mask              : in    std_logic_vector(15 downto 0);
      app_wdf_wren              : in    std_logic;
      app_rd_data               : out   std_logic_vector(127 downto 0);
      app_rd_data_end           : out   std_logic;
      app_rd_data_valid         : out   std_logic;
      app_rdy                   : out   std_logic;
      app_wdf_rdy               : out   std_logic;
      app_sr_req                : in    std_logic;
      app_ref_req               : in    std_logic;
      app_zq_req                : in    std_logic;
      app_sr_active             : out   std_logic;
      app_ref_ack               : out   std_logic;
      app_zq_ack                : out   std_logic;
      ui_clk                    : out   std_logic;
      ui_clk_sync_rst           : out   std_logic;
      init_calib_complete       : out   std_logic;
      -- System Clock Ports
      sys_clk_p                 : in    std_logic;
      sys_clk_n                 : in    std_logic;
      -- Reference Clock Ports
      clk_ref_i                 : in    std_logic;
      device_temp_i             : in    std_logic_vector(11 downto 0);
      sys_rst                   : in    std_logic
      );
  end component mig_7series_0;


  -- Local bus --
  signal reg_num_pre_fetch  : std_logic_vector(kDefaultNum'range);
  signal reg_num_read       : std_logic_vector(kDefaultNum'range);
  signal state_lbus	        : BusProcessType;

  -- debug --
  -- attribute mark_debug of state_mig       : signal is "true";
  -- attribute mark_debug of app_cmd         : signal is "true";
  -- attribute mark_debug of app_en          : signal is "true";
  -- attribute mark_debug of app_addr          : signal is "true";
  -- attribute mark_debug of app_rdy          : signal is "true";
  -- attribute mark_debug of app_wdf_data         : signal is "true";
  -- attribute mark_debug of app_wdf_rdy          : signal is "true";
  -- attribute mark_debug of app_wdf_end          : signal is "true";
  -- attribute mark_debug of app_wdf_wren          : signal is "true";
  -- attribute mark_debug of app_rd_data          : signal is "true";
  -- attribute mark_debug of app_rd_data_valid         : signal is "true";
  -- attribute mark_debug of app_rd_data_end         : signal is "true";

  -- attribute mark_debug of state_pf          : signal is "true";
  -- attribute mark_debug of wcount_write_buf  : signal is "true";

  -- attribute mark_debug of is_fetched     : signal is "true";
  -- attribute mark_debug of fetch_progressing     : signal is "true";
  -- attribute mark_debug of ui_clk_sync_rst     : signal is "true";


-----------------------------------------------------------------------

-- =============================== body ===============================
begin
  din_write_buf   <= dIn;
  wen_write_buf   <= writeEnable;
  progFull        <= pfull_write_buf;
  dOut            <= dout_read_buf;
  ren_read_buf    <= readEnable;
  readValid       <= rv_read_buf;
  emptyReadBuf    <= empty_read_buf;


  ---------------------------------------------------------------------
  -- MIG clock domain
  ---------------------------------------------------------------------
  u_mig_7series_0 : mig_7series_0
    port map (
      -- Memory interface ports
      ddr3_addr                      => DDR3_ADDR,
      ddr3_ba                        => DDR3_BA,
      ddr3_cas_n                     => DDR3_CAS_N,
      ddr3_ck_n                      => DDR3_CK_N,
      ddr3_ck_p                      => DDR3_CK_P,
      ddr3_cke                       => DDR3_CKE,
      ddr3_ras_n                     => DDR3_RAS_N,
      ddr3_reset_n                   => DDR3_RESET_N,
      ddr3_we_n                      => DDR3_WE_N,
      ddr3_dq                        => DDR3_DQ,
      ddr3_dqs_n                     => DDR3_DQS_N,
      ddr3_dqs_p                     => DDR3_DQS_P,
      init_calib_complete            => OPEN,
      ddr3_cs_n                      => DDR3_CS_N,
      ddr3_dm                        => DDR3_DM,
      ddr3_odt                       => DDR3_ODT,
      -- Application interface ports
      app_addr                       => app_addr,
      app_cmd                        => app_cmd,
      app_en                         => app_en,
      app_rdy                        => app_rdy,
      app_wdf_rdy                    => app_wdf_rdy,

      app_wdf_data                   => app_wdf_data,
      app_wdf_end                    => app_wdf_end,
      app_wdf_wren                   => app_wdf_wren,
      app_wdf_mask                   => x"0000",

      app_rd_data                    => app_rd_data,
      app_rd_data_end                => app_rd_data_end,
      app_rd_data_valid              => app_rd_data_valid,

      app_sr_req                     => '0',
      app_ref_req                    => '0',
      app_zq_req                     => '0',
      app_sr_active                  => open,
      app_ref_ack                    => open,
      app_zq_ack                     => open,

      ui_clk                         => ui_clk,
      ui_clk_sync_rst                => ui_clk_sync_rst,

      -- System Clock Ports
      sys_clk_p                      => MIGS_CLKP,
      sys_clk_n                      => MIGS_CLKN,
      -- Reference Clock Ports
      clk_ref_i                      => refClkIn,
      device_temp_i                  => xadcTempIn,
      sys_rst                        => rst
    );

  write_buf : mig_wbuf port map
    (
      rst           => ui_clk_sync_rst,
      wr_clk        => clk,
      rd_clk        => ui_clk,
      din           => din_write_buf,
      wr_en         => wen_write_buf,
      rd_en         => ren_write_buf,
      dout          => dout_write_buf,
      full          => open,
      empty         => empty_write_buf,
      valid         => rv_write_buf,
      rd_data_count => wcount_write_buf,
      prog_full     => pfull_write_buf,
      wr_rst_busy   => open,
      rd_rst_busy   => open
    );

  pre_fetch : process(ui_clk, ui_clk_sync_rst)
    variable index_pfbuf  : integer range 0 to kLengthPF;
    variable read_count   : integer range 0 to kLengthPF;
    variable pfwait_count : integer range 0 to kWidthAppData-1;
    -- variable valid_count : integer range 0 to kMaxPreFetch-1;
  begin
    if(ui_clk_sync_rst = '1') then
      index_pfbuf   := 0;
      read_count    := 0;
      pfwait_count  := 0;
      fetch_progressing <= '0';
      is_fetched    <= '0';
      ren_write_buf <= '0';
      pf_valid      <= (others => '0');
      state_pf      <= Idle;
    elsif(ui_clk'event and ui_clk = '1') then
      case state_pf is
        when Idle =>
          index_pfbuf       := 0;
          read_count        := 0;
          is_fetched        <= '0';
          fetch_progressing <= '0';
          pf_valid          <= (others => '0');
          if(empty_write_buf = '0') then
            pfwait_count  := pfwait_count +1;

--            if((req_pre_fetch = '1' and to_integer(wcount_write_buf) >= to_integer(reg_num_pre_fetch)) or pfwait_count > 100) then
            if(to_integer(unsigned(wcount_write_buf)) >= to_integer(unsigned(reg_num_pre_fetch)) or pfwait_count > 100) then
              ren_write_buf   <= '1';
              read_count      := read_count +1;
              state_pf        <= Fetch;
            end if;
          end if;

        when Fetch =>
          if(read_count = to_integer(unsigned(reg_num_pre_fetch)) or empty_write_buf = '1') then
            ren_write_buf   <= '0';
          else
            read_count      := read_count +1;
          end if;

          if(rv_write_buf = '1') then
            fetch_progressing       <= '1';
            pf_buffer(index_pfbuf)  <= dout_write_buf;
            pf_valid(index_pfbuf)   <= '1';
            index_pfbuf             := index_pfbuf +1;
          end if;

          if(index_pfbuf = to_integer(unsigned(reg_num_pre_fetch)) or empty_write_buf = '1') then
            is_fetched  <= '1';
            state_pf    <= Done;
          end if;

        when Done =>
          if(move_to_pfidle = '1') then
            pfwait_count      := 0;
            is_fetched        <= '0';
            fetch_progressing <= '0';
            state_pf          <= Idle;
          end if;

        when others =>
          state_pf  <= Idle;

      end case;
    end if;
  end process;

  u_mig_seq : process(ui_clk, ui_clk_sync_rst)
    variable index_pfbuf : integer range 0 to kLengthPF;
    variable index_inst  : integer range 0 to kLengthPF;
    variable index_read  : integer range 0 to kLengthPF;
    variable addr_write  : std_logic_vector(kWidthAppAddr-5 downto 0);
    variable addr_read   : std_logic_vector(kWidthAppAddr-5 downto 0);
  begin
    if(ui_clk_sync_rst = '1') then
      app_en        <= '0';
      app_wdf_wren  <= '0';
      app_wdf_end   <= '0';
      app_cmd       <= kCmdWrite;
      app_addr      <= (others => '0');
      index_pfbuf   := 0;
      index_inst    := 0;
      index_read    := 0;
      addr_write    := (others => '0');
      addr_read     := (others => '0');
      app_wdf_data  <= (others => '0');
      --req_pre_fetch <= '0';
      move_to_pfidle <= '0';
      state_mig     <= Idle;
    elsif(ui_clk'event and ui_clk = '1') then
      case state_mig is
        when Idle =>
          index_pfbuf     := 0;
          index_inst      := 0;
          index_read      := 0;
          app_en          <= '0';
          if(fetch_progressing = '1') then
--            req_pre_fetch     <= '0';

            app_wdf_wren      <= '1';
            app_wdf_end       <= '1';
            app_addr          <= '0' & addr_write & "000";
            app_wdf_data      <= pf_buffer(index_pfbuf);

            app_cmd           <= kCmdWrite;
            state_mig         <= WriteCmd;
--          else
--            req_pre_fetch     <= '1';
          end if;

        when WriteCmd =>
          index_read      := 0;
          if(app_rdy = '1' and app_en = '1') then
            if(index_inst+1 = to_integer(unsigned(reg_num_pre_fetch)) or pf_valid(index_inst+1)= '0') then
              if(pfull_read_buf = '1') then
                app_en          <= '0';
                state_mig       <= Idle;
              else
                move_to_pfidle  <= '1';
                addr_write      := std_logic_vector(unsigned(addr_write) +1);
                app_addr        <= '0' & addr_read & "000";
                app_cmd         <= kCmdRead;
                state_mig       <= ReadCmd;
              end if;
            else
              index_inst    := index_inst +1;
              addr_write    := std_logic_vector(unsigned(addr_write) +1);
              app_addr      <= '0' & addr_write & "000";
            end if;
          end if;

          if(app_wdf_rdy = '1') then
            app_en            <= '1';
            if(index_pfbuf+1 = to_integer(unsigned(reg_num_pre_fetch)) or pf_valid(index_pfbuf+1) = '0') then


              app_wdf_wren    <= '0';
              app_wdf_end     <= '0';
            else
              index_pfbuf   := index_pfbuf +1;
              app_wdf_data  <= pf_buffer(index_pfbuf);
            end if;
          end if;

        when ReadCmd =>
          index_pfbuf      := 0;
          index_inst       := 0;

          if(is_fetched = '1') then
--            req_pre_fetch   <= '1';
            move_to_pfidle  <= '0';
          end if;

          if(app_rdy = '1') then
            addr_read   := std_logic_vector(unsigned(addr_read) +1);
            index_read  := index_read +1;
            if(index_read = to_integer(unsigned(reg_num_read)) or addr_write = addr_read) then

              if(is_fetched = '1') then
                app_en           <= '1';
                app_wdf_wren     <= '1';
                app_wdf_end      <= '1';
                app_addr         <= '0' & addr_write & "000";
                app_wdf_data     <= pf_buffer(index_pfbuf);

                app_cmd          <= kCmdWrite;
                state_mig        <= WriteCmd;
              -- elsif(addr_write /= addr_read) then
              elsif(empty_write_buf = '1' and addr_write /= addr_read) then
                app_addr        <= '0' & addr_read & "000";
              else
                app_en      <= '0';
                state_mig   <= Idle;
              end if;
            else
              app_addr        <= '0' & addr_read & "000";
            end if;
          end if;

        when others =>
          state_mig   <= Idle;

      end case;
    end if;
  end process;

read_buf : mig_rbuf port map
  (
    rst         => ui_clk_sync_rst,
    wr_clk      => ui_clk,
    rd_clk      => clk,
    din         => app_rd_data,
    wr_en       => app_rd_data_valid,
    rd_en       => ren_read_buf,
    dout        => dout_read_buf,
    full        => open,
    empty       => empty_read_buf,
    valid       => rv_read_buf,
    prog_full   => pfull_read_buf,
    wr_rst_busy => open,
    rd_rst_busy => open
  );



  ---------------------------------------------------------------------
  -- Clock domain crossing
  ---------------------------------------------------------------------

  ---------------------------------------------------------------------
  -- System clock domain
  ---------------------------------------------------------------------
  -- Local bus process ------------------------------------------------
  u_BusProcess : process(clk, sync_reset)
  begin
    if(sync_reset = '1') then
      reg_num_pre_fetch <= kDefaultNum;
      reg_num_read      <= kDefaultNum;

      state_lbus	      <= Init;
    elsif(clk'event and clk = '1') then
      case state_lbus is
        when Init =>
        reg_num_pre_fetch <= kDefaultNum;
        reg_num_read      <= kDefaultNum;

          dataLocalBusOut <= x"00";
          readyLocalBus		<= '0';
          state_lbus		  <= Idle;

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
            when kNumWrite(kNonMultiByte'range) =>
              reg_num_pre_fetch  <= dataLocalBusIn(reg_num_pre_fetch'range);

            when kNumRead(kNonMultiByte'range) =>
              reg_num_read  <= dataLocalBusIn(reg_num_read'range);

            when others =>
              null;
          end case;
          state_lbus	<= Done;

        when Read =>
          case addrLocalBus(kNonMultiByte'range) is
            when kNumWrite(kNonMultiByte'range) =>
              dataLocalBusOut   <= "00" & reg_num_pre_fetch;

            when kNumRead(kNonMultiByte'range) =>
               dataLocalBusOut   <= "00" & reg_num_read;

             when others => null;
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

