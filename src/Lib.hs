module Lib
    ( uniOsc
    , stereoWidth
    , liftToSESigs
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

liftToSESigs :: (Sig -> Sig) -> SE (Sig, Sig) -> SE (Sig, Sig)
liftToSESigs fn = fmap (\(x, y) -> (fn x, fn y))
