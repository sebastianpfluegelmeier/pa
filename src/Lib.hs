module Lib
    ( bass1
    , mix1
    ) where

import Csound.Base

bass1 :: D -> SE Sig

bass1 freq = 

    return $ setDur 10 $ env * filter (sosc freq) * 0.06

        where 
            env      :: Sig
            env      = xeg 0.1 0.8 2 0.2

            filter   :: Sig -> Sig
            filter   = lp ( lfo 
                          + 200.0 
                          + linsegr [400.0, 0.4, 250, 1.2, 220] 0.2 100.0
                          ) 1.0

            sosc      :: D -> Sig
            sosc x    = sum $ map (\y -> saw (sig x + y)) (map (sig.double) [-2.0..2.0])

            lfo      :: Sig
            lfo      = osc (7 + linseg [1.0, 2.0, 0.0, 3.0, 0.1]) * 10 + 100
    

mel1 :: Sco D
mel1 = mel $ map temp (map (*440) [1/2,2/3,3/4,4/5,5/6,6/7,7/8,8/9,9/10])

score1 :: Sco (Mix Sig)
score1 = sco bass1 mel1

mix1 :: Sig
mix1 = mix score1
