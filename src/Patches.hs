module Patches
( bass1
) where
import Csound.Base
import Lib


bass1 :: D -> SE (Sig, Sig)
bass1 freq = 
    liftToSESigs (sub +) overtones
      where
        sub       = env * (osc $ sig freq)
        env       = leg 0.001 0.4 0.8 0.1
        overtones :: SE (Sig, Sig)
        overtones = liftToSESigs (\x -> otFilter $ otDist $ otEnv * x) $ uniOsc otOsc 2 6 (sig freq)
        otFilter  = lp (0.2 * (sig freq) + otFEnv) 3.1
        otOsc     = saw
        otDist    = (\x -> x)
        otEnv     = leg 0.001 0.2 0.1 0.2
        otFEnv    = xeg 0.001 0.6 0.1 0.2



