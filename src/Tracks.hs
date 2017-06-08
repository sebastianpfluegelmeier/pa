module Tracks
( track1
, kick
, clap
, hat
, bass1
)
where 
import Csound.Base
import Csound.Sam   
import Lib

track1 :: IO ()
track1 = (sFmap2 
             blp 
             (liftToStereo $ fmap (\x -> 500 + 400 * uosc (4 + 3 * blp 0.4 x )) white)
             (liftToStereo $ return $ saw 220))
         >-> (distortion 10) 
         >>> dac 

kick :: SE Sig2
kick = 
    (loadSam1 "drums_and_single_hits/kicks/giant-afro-kick1.wav"
        >-> (* linseg [0.8, 0.03, 0.2, 0.2, 0.01])
        >-> bhp 900
    )
    >->
    (+ sub)
    >>>
    (+ (collectTuple (pink, pink))
        >-> (* expseg [1, 0.06, 0.1, 0.2, 0.0001])
        >-> bhp 300
    )
    >->
    distortion 0.18
    >-> 
    (* linseg [1, 0.2, 1.3, 0.5, 0.3, 0.1, 0.001])
    >->
    (mlp filterEnv 0.2)

    where
      sub = osc pitchEnv * subAmpEnv
      pitchEnv  = expseg [1800, 0.011, 120, 0.1, 45, 1, 20]
      subAmpEnv = linseg [1, 0.2, 0.5, 0.3, 0.001]
      filterEnv = expseg [22000, 0.2, 3000, 0.3, 100, 0.3, 40]

clap :: SE Sig2
clap = 
    return (saw 584, saw 434) 
    >>> (* loadSam1 "drums_and_single_hits/claps/electro-cricket-clap4.wav")
    >>> (+ loadSam1 "drums_and_single_hits/claps/pop-clap.wav" * 2)

hat :: D -> SE Sig2 
hat x = 
    loadSam1 "drums_and_single_hits/hats/chh.wav"
    >-> (* (expseg [1, x, 0.01] * (1/2 + sig x/2)))

bass1 :: Sig -> Sig -> SE Sig2
bass1 expr freq =
    collectTuple (white, white)
    >->
    (* 0)
    >->
    (+ (osc (freq + (expr * 1000 * saw (freq * 7)) >>> blp 1000 * expr) * (1 + expr)))
    >->
    distortion 1
    >->
    chory3
    >->
    phasy1


