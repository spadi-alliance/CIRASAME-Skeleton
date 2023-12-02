library ieee, mylib;
use ieee.std_logic_1164.all;
use mylib.defBCT.all;

package defMIG is
  -- Module Entity ports --
  constant kWidthDaqData      : positive:= 64;

  -- Pre-fetch process--
  constant kWidthNumPF        : positive:= 6;
  constant kLengthPF          : positive:= 65;

  constant kDefaultNum        : std_logic_vector(kWidthNumPF-1 downto 0):= "010000"; -- 16

  type PreFetchType is
    (
      Idle, Fetch, Done
    );

  -- MIG --
  -- app_cmd(2:0):   "000": wrihte, "001": read.
  -- app_addr(27:0): RankAddr(1) + BankAddr(3) + RowAddr(14) + ColumnAddr(10)
  constant kWidthAppData      : positive:= 128;
  constant kWidthAppAddr      : positive:= 28;

  constant kCmdWrite          : std_logic_vector(2 downto 0):= "000";
  constant kCmdRead           : std_logic_vector(2 downto 0):= "001";

  type MigProcessType is
    (Idle,
     WriteCmd, ReadCmd
     );

  -- Local Address  -------------------------------------------------------
  constant kNumWrite           : LocalAddressType := x"000"; -- R/W,   [5:0]
  constant kNumRead            : LocalAddressType := x"010"; -- R/W,   [5:0]

end package defMIG;

