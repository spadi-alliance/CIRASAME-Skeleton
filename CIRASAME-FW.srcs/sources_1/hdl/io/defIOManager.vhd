library ieee, mylib;
use ieee.std_logic_1164.all;
use mylib.defBCT.all;

package defIOManager is
  -- Local Address  -------------------------------------------------------
  constant kSelDiscri              : LocalAddressType := x"000"; -- W/R, [6:0], select discriminator output

end package defIOManager;

