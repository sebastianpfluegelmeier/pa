module Lib
    ( uniOsc
    , stereoWidth
    , test
    -- , mmel
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

test :: SE (Sig, Sig)
test = 
    fmap (stereoWidth 0.8) $ uniOsc (\x -> osc (x + 400 * osc (2 + 0.9 * osc 0.3 * osc 0.183) * (saw (x * 4) + tri (x * 3)))) 0.9 4 440 -- [220,1,220,1,440,1,440,1,220]

-- line :: Sig 
-- line = linseg mmel
-- 
-- mmel :: [D]
-- mmel = melody 
--        (map (* 0.5) [1/4, 1/4, 1/2, 3/8, 3/8, 1/4]) 
--        (map (* 50) [1, 2/3, 4/3, 5/3, 5/2, 3/5])
--           where
--             melody [] [] = []
--             melody (x:xs) (y:ys) =
--               [y, x, y, 0.001] ++ melody xs ys
