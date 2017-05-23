module Lib
    ( uniOsc
    , stereoWidth
    , liftToSESigs
    ) where
import Csound.Base
import Data.List (intersperse)


-- TODO: solve :
uniOsc :: (Sig -> Sig) -> Sig -> Int -> Sig -> SE (Sig, Sig)

uniOsc iosc detune voicesNum freq = 
    seed 4 >> fmap spread (sequence voices) 
      where
        voices :: [SE Sig]
        voices = replicate voicesNum voice

        voice :: SE Sig
        voice = (\x y-> 
                    iosc $ freq 
                         -- + 10000 * y * linseg [1 , 0.01, 0]
                         + lp 1 0.5 x)
                <$> randomi (-detune) detune 1000
                <*> randomi (-detune) detune 2 

        spread :: [Sig] -> (Sig, Sig)
        spread x = 
            foldr1 (\(x1, x2) (y1, y2) -> (x1 + y1, x2 + y2)) $ zipped x

        zipped :: [Sig] -> [(Sig, Sig)]
        zipped x = 
            zipWith 
                pan
                x 
                [0.0, (0.0 + 1.0 / (fromIntegral $ length x)) .. 1]

        pan :: Sig -> Double -> (Sig, Sig)
        pan signal lr = (signal * (sig $ double $ lr), signal * (1.0 - (sig $ double lr)))

            

stereoWidth :: Sig -> (Sig, Sig) -> (Sig, Sig)
stereoWidth amt (left, right) =
    (amt * left + mid * (1 - amt), amt * right + mid * (1 - amt))
      where
        mid = left + right

liftToSESigs :: (Sig -> Sig) -> SE (Sig, Sig) -> SE (Sig, Sig)
liftToSESigs fn = fmap (\(x, y) -> (fn x, fn y))
