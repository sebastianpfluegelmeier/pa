module TempA
( write1
, temp1
)
where
import Csound.Base
import Lib
import Patches
import Scores
import Tracks

baseFreq = 60

write1 = writeSnd "/home/sebastian/p/hs/pa/TempA.wav" $ fmap (\(x, y) -> (setDur 8 x, setDur 8 y))$ temp1

temp1 :: SE (Sig, Sig)
temp1 = 
    (\(xl, xr) (yl, yr) (zl, zr) -> (xl + yl + zl, xr + yr + zr)) <$> bass <*> perc <*> pad
      where
        bass :: SE (Sig, Sig)
        bass = return $ (\x -> (x, x)) $ 
                20 * lp (500 + 100 * usaw 3) 0.4 $
                osc (freq * 1.008) *
                bassAmp * (lp 100 0.7 $ 
                        mlp (sig 330 + 90 * uosc 9) 0.8 
                           $ osc freq * 
                           (((2 + uosc 8) / 3) * oscA))
        -- bass = return $ (sig 0, sig 0)
        freq :: Sig
        freq = baseFreq * expseg [2, 1, 1, 1, 1, 0.2, 6/5, 0.3, 5/3, 0.5, 1]
        
        oscA :: Sig
        oscA = osc (baseFreq + osc (baseFreq * 10)) * saw (baseFreq * 4)

        bassAmp :: Sig 
        bassAmp = linseg [1, 1, 0.4, 0.001, 1, 1, 1, 1, 0.1,1, 1]

        perc = return (sig 0, sig 0)
         
        pad = (\x y -> (x, y)) <$> chord <*> chord
        --pad = return $ (sig 0, sig 0)

        chord :: SE Sig
        chord = 
            fmap (foldr1 (+)) $ 
                sequence 
                    [ note 1
                    , note 2
                    , note (3/2)
                    , note (4/3)
                    , note (5/4)
                    , note (5/3)
                    ]

        note :: D -> SE Sig
        note freq = (\x y -> y * (lp (sig freq * 1.5 * x) 0.8 $ saw $ sig freq)) <$>
                    noise <*>
                    noise

        noise :: SE Sig
        noise = randomi 0.7 1 5

