library ieee, mylib;
use ieee.std_logic_1164.all;
use mylib.defBCT.all;

package defFreeRunScaler is
  constant kWidthCnt              : integer:= 32;

  -- Systerm information --
  constant kNumSysInput           : integer:= 8;

  constant kIndexClk              : integer:= 0;
  constant kIndexRealTime         : integer:= 1;
  constant kIndexDaqRunTime       : integer:= 2;
  constant kIndexTotalThrotTime   : integer:= 3;
  constant kIndexInThrot1Time     : integer:= 4;
  constant kIndexInThrot2Time     : integer:= 5;
  constant kIndexOutThrotTime     : integer:= 6;
  constant kIndexHbfThrotTime     : integer:= 7;


  -- Local Address  -------------------------------------------------------
  constant kCntReset              : LocalAddressType := x"000"; -- W, [0:0], assert counter reset
  constant kLatchSrc              : LocalAddressType := x"010"; -- R, [0:0], read busy state and assert latch_scr signal
  constant kNumCh                 : LocalAddressType := x"020"; -- R, [7:0], # of scaler channel
  constant kReadFIFO              : LocalAddressType := x"100"; -- R, [7:0], Read FIFO data

end package defFreeRunScaler;

