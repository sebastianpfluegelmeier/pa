module Tracks
( track1
)
where 
import Csound.Base
import Lib
import Scores
import Patches

track1 = mix $ sco bass1 score1
