module Lib
    ( uniOsc
    , stereoWidth
    , inst1
    , mel1
    ) where
import Csound.Base
import Data.List (intersperse)

uniOsc :: (Sig -> Sig) -> Sig -> Int -> Sig -> SE (Sig, Sig)

uniOsc iosc detune voices freq = 
    (\x y -> (x, y)) <$> side <*> side
      where
        side :: SE Sig
        side = foldr1 (+) (replicate voices voice)

        voice :: SE Sig
        voice = fmap (\x -> iosc $ freq + expseg [10000, 0.001, 1] * detune * lp 100 1 x) pink

stereoWidth :: Sig -> (Sig, Sig) -> (Sig, Sig)
stereoWidth amt (left, right) =
    (amt * left + mid * (1 - amt), amt * right + mid * (1 - amt))
      where
        mid = left + right

inst1 :: Sig -> SE (Sig, Sig)
inst1 melo = 
    fmap 
      ((stereoWidth 0.6).(\(l, r) -> ((filter l)*amp, (filter r)*amp))) 
      $ uniOsc 
        (\x -> osc (lp (200 + 200 * slowsaw ) 0.9 
                       (x + 200 * (filter.osc) (1 + 0.9 * osc 0.232 * osc 0.083) 
                       * (saw (x * 7) + tri (x * 9)))))
        (0.7 + 0.5 + osc 0.25) 
        8 melo 


      where
        filter x = hp (10000 + 400 * osc 1.5) 0.3 x + lp (lp 2 1 $ 150 + 90 * saw 16) 1 x
        amp = (* 0.5) $ (1 + slowsaw) 
              * (1 + saw 8) + (3 - slowsaw) 
              * (1 + tri 8) + 0.3 
              * (1 + slowsaw)
        slowsaw = saw 0.25
        


mel1 :: Sig 
mel1 = constSeq (map (* 80) [1/2, 1, 1/2, 2, 1, 1/2, 3/4, 1/2, 16/9, 16/11, 16/13, 7/3, 2,1,2,1, 1, 1, 3/2, 7/6, 2, 1, 5/3, 1, 6/3, 7/3, 8/3, 9/3, 6/3, 5/3, 4/3, 3/3]) 8

