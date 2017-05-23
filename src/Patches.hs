module Patches
( bass1
--, fmu
) where
import Csound.Base
import Lib


bass1 :: D -> SE (Sig, Sig)
bass1 freq = 
    uniOsc (\x -> lp (fenv * 10000) (sig 0.4) $ saw x) (sig 8) 8 (sig freq)
      where
        fenv       = xeg 0.001 0.4 0.2 0.01

{-


fmu :: D -> D -> D -> D -> D -> D -> D -> SE (Sig, Sig)
fmu coarse cNoise amp aNoise attack decay freq =
    (\x y -> (x, y)) <$> side <*> side
      where
        side :: SE Sig
        side = (\x y -> 
                   leg attack decay 0 decay 
                 * (amp + (aNoise * x))
                 * osc (freq * coarse + y * cNoise)
               ) 
               <$> noise 
               <*> noise

        noise :: SE Sig
        noise = fmap (lp 1 1) white

-}    
