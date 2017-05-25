module Tracks
( track1
)
where 
import Csound.Base
import Lib
import Scores
import Patches

track1 :: (Sig, Sig)
track1 = mix $ har [sco phatFM (applyGroove (str 0.4 groove1) score1),
                    sco bass1 (applyGroove (str 0.4 groove2) score2)]
