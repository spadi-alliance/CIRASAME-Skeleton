library IEEE, mylib;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity HitDetector is
  generic(
    kIoStandard         : string:= "LVCMOS18";    -- IOSTANDARD of OBUFDS
    kNumHitInput        : integer:= 128
  );
  port(
    rst	                : in std_logic;
    clkPar	            : in std_logic;
    clkSer	            : in std_logic;

    -- Module Input --
    dataInFromPin       : in std_logic_vector(kNumHitInput-1 downto 0);

    -- Module Output --
    ibufOut             : out std_logic_vector(kNumHitInput-1 downto 0);
    hitOut              : out std_logic_vector(kNumHitInput-1 downto 0)
    );
end HitDetector;

architecture RTL of HitDetector is
  attribute mark_debug        : boolean;

  -- internal signal declaration ----------------------------------------
  constant kWidthData         : integer:= 8;

  type SerdesOutType is array (kNumHitInput-1 downto 0) of std_logic_vector(kWidthData-1 downto 0);
  signal dout_to_device       : SerdesOutType;

  signal hit_signal           : std_logic_vector(kNumHitInput-1 downto 0);

  -- Debug ------------------------------------------------------------


-- =============================== body ===============================
begin

  gen_iserdes : for i in 0 to kNumHitInput-1 generate
  begin
    u_Iserdes : entity mylib.Iserdes8Impl
      generic map
      (
        kRxPolarity     => false,
        kIoStandard     => kIoStandard
      )
      port map
      (
        -- From the system to the device
        dInFromPin      => dataInFromPin(i),
        ibufOut         => ibufOut(i),

        -- ISERDES
        dOutToDevice    => dout_to_device(i),
        bitslip         => '0',

        -- Clock and reset
        clkIn           => clkSer,
        clkDivIn        => clkPar,
        ioReset         => rst
      );

      hit_signal(i)  <= '1' when (to_integer(unsigned(dout_to_device(i))) /= 0) else '0';

  end generate;

  u_buf : process(clkPar)
  begin
    if(clkPar'event and clkPar = '1') then
      hitOut  <= hit_signal;
    end if;
  end process;


end RTL;

