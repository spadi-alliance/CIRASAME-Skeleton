library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

package defToplevel is
  -- Number of input per a main port.
  constant kNumInput            : positive:= 32;

  -- BEM specification
  constant kNumLED              : positive:= 4;
  constant kNumBitDIP           : positive:= 4;
  constant kNumSfp              : positive:= 1;
  constant kNumNim              : positive:= 2;

end package defToplevel;
