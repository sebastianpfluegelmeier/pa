module Tracks
( track1
, track2
)
where 
import Csound.Base
import Csound.Sam   
import Lib
import Scores
import Patches

track1 :: (Sig, Sig)
track1 = mix $ har [sco phatFM (applyGroove (str 0.4 groove1) score1),
                    sco bass1 (applyGroove (str 0.4 groove2) score2)]


samDir x = "/home/sebastian/samples/" ++ x
drumsDir x = "/home/sebastian/samples/drums_and_single_hits/" ++ x

track2 = mix $ loopBy 4 $ str 0.6 $ 
           har [ sco kick kickGroove
               , sco snare snareGroove
               ]

kickGroove :: Sco D
kickGroove = applyGroove (groove [7/4, 1/4, 3/2, 1/2]) (loopBy 4 $ temp 1)
snareGroove :: Sco D
snareGroove = applyGroove (groove [-1, 1, -1, 1])       (loopBy 4 $ temp 1)


kick  :: D -> SE (Sig2)
kick  _ = runSam 1 $ wav1 $ drumsDir "kicks/808bd2.aif"

snare :: D -> SE (Sig2)
snare _ = runSam 1 $ wav1 $ drumsDir "snares/acoustic_snare.aif"

hh :: D -> SE Sig2
hh    _ = runSam 1 $ wav1 $ drumsDir "hat"

temp3 :: SE (Sig, Sig)
temp3 = uniOsc myOsc 
            (linseg [0.4, riser2Length / 2, 10, riser2Length / 2, 102])
            16 
            (expseg [baseFreq, riser2Length, baseFreq * 8])

          where
            myOsc x = uosc (expseg [0.4, riser2Length, 100]) *
                      uosc (expseg [1000, riser2Length, 0.1]) *
                      (mlp ((x**1.1) * 2.9) 0.3 $ 
                        osc (x + linseg [0.2, riser2Length, 1.1] * 
                        (100 * (osc2 x) + 40* (osc3 x ))))
            osc2 x = tri (x * 3) * uosc 4
            osc3 x = osc (x * 2) + 0.03 * saw (x * 10) * uosc 5

riser2Length = 30.72 / 2

baseFreq = 41.20
riserLength = 30.72
fade = fadeIn riserLength

temp2 :: SE (Sig, Sig)
temp2 = 
    return $
    (\(l, r) -> (fx l, fx r)) (rise1 baseFreq, rise1 (baseFreq * 1.1))
      where
        rise1 :: D -> Sig
        rise1 x = osc1 (linseg [x, riserLength, x * 16]) (linseg [x * 8, riserLength, x * 2])

        osc1 :: Sig -> Sig -> Sig
        osc1 x y = osc x * 
                 saw (x) * saw (x + osc (3 + 10 * fade)) + 
                 osc (x * 1.014) * 
                 osc (x * 0.9356) * 
                 lp (x * 3 + y * uosc 2) 0.4 (saw (y * 2.5))

        fx :: Sig -> Sig
        fx x = hp (100 + (expseg [0.4, riserLength, 2])  * 2000) 0.8 x

temp1 :: SE (Sig, Sig)
temp1 = fmap (\x -> (toStereo sub + x) * 0.3 * toStereo aenv) overtones
    where 
      sub = osc (sig baseFreq)

      overtones = uniOsc ((mlp fenv 0.4).(distortion 3).(ring *).osc1) (30 * fadeIn 0.2) 32 (sig baseFreq)

      osc1 = (mlp fenv 0.2).osc.(\x -> x + 1000 * aenv * osc (10 * x) * saw (4.01 * x))
      fenv = expseg [20000, 0.1, 5000, 0.1, 400, 0.1, 100]
      aenv = expseg [1, 0.2, 0.8, 0.15, 0.2, 0.1, 0.0001]
      ring :: Sig
      ring = 1 + 0.1 * (usaw (sig baseFreq) * 0.8) * fadeOut 1
