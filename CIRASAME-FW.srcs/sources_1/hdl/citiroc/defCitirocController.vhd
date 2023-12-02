library ieee, mylib;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use mylib.defBCT.all;

package defCitiroc is
  -- Local Address  -------------------------------------------------------
  constant kAddrSCFIFO          : LocalAddressType := x"000"; -- Wtite FIFO
  constant kAddrPDC             : LocalAddressType := x"010"; -- Pin direct control
  constant kAddrCC              : LocalAddressType := x"020"; -- Cycle control
  

  -- Slow Control State Machine -------------------------------------------
  type SlowControlProcessType is (
    Idle,
    ReadFIFO,
    StartCount,
    IdleCount,
    DoCount,
    ResetCount,
    EmptyFIFO,
    EndCycle
    );
  
end package defCitiroc;	

