library ieee;
use ieee.std_logic_1164.all;

package defToplevel is
  -- Number of input per a main port.
  constant kNumInput            : positive:= 128;

  -- BEM specification
  constant kNumLED              : positive:= 4;
  constant kNumBitDIP           : positive:= 4;
  constant kNumGtx              : positive:= 1;
  constant kNumNim              : positive:= 2;

end package defToplevel;
