module Patches
( bass1
, op
, phatFM
) where
import Csound.Base
import Lib


bass1 :: D -> SE (Sig, Sig)
bass1 freq = 
    uniOsc (\x -> sub + (hp 130 0.2 $ mlp (fenv * 900) (sig 0.4) $ aenv * customOsc x * 0.2)) (sig 4) 4 (sig freq)
      where
        fenv = xeg 0.01 0.1 0.2 0.01
        aenv = leg 0.01 0.2 0.3 0.01
        sub = osc (sig freq) * aenv * 0.3
        customOsc x = saw (2 * x + 100 * osc (x * 3) * (0.5 + aenv/2))


op :: (Sig -> Sig) -> D -> D -> D -> D -> SE Sig -> D -> SE Sig
op oscillator coarse noiseIntensity attack decay modulator freq =
    mono
      where
        mono :: SE Sig
        mono = (\n1 n2 mod -> 
                   leg attack decay 0 decay 
                 * (1 + n1)
                 * oscillator (sig freq * sig coarse + mod +  n2)
               ) 
               <$> noise 
               <*> noise
               <*> modulator

        noise :: SE Sig
        noise = fmap ((*sig noiseIntensity).lp 1 1) (randomi (-1) 1 100)

phatFM :: D -> SE (Sig, Sig)
phatFM freq = 
        fmap ((\x -> (x, x)).(* 0.4)) (mono freq)
      where
        mono x = op osc1 1 0.1 0.001 0.5 ((\x y -> x * 0.1 + y * 0.1) <$> op2 <*> op3) x
        op2  = op osc2 2 0.1 0.01 0.3 (return $ sig 0) freq 
        op3  = op osc3 2 0.1 0.01 0.2 (op4) freq 
        op4  = op osc1 2 0.1 0.01 0.2 (return $ sig 0) freq 
        osc1 x = sin x + 0.3 * tri (x * 3) + 0.5  * sin (x * 8)
        osc2 x = sin x + 0.2 * sin (x * 6) + 0.4  * tri (x * 7)
        osc3 x = sin x + 0.4 * (lp 100 0.3 $ saw (x * 2)) + 0.03 * sqr (x * 4)
