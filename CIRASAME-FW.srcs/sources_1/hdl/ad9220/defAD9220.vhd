library ieee, mylib;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use mylib.defBCT.all;

package defAD9220 is

  -- AD9220 --
  constant kWdithAdcData    : positive:= 12;
  constant kAdcLatency      : integer:= 3;

  -- CITIROC --
  constant kReadLength      : integer:= 33*4;

  -- Local Address  -------------------------------------------------------
  constant kAdcDaqGate      : LocalAddressType := x"000"; -- W/R, [0:0]
  constant kEventFull       : LocalAddressType := x"010"; -- R, [0:0]

  constant kReadFIFO        : LocalAddressType := x"100"; -- R,
  constant kReleaseBusy     : LocalAddressType := x"200"; -- W,

end package defAD9220;

