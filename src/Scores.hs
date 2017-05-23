module Scores 
( score1
)
where
import Csound.Base

score1 :: Sco D 
score1 = har $ [ mel $ map temp (map (*220) [6/3,    7/3, 8/3,   9/3])
               , mel $ map temp (map (*220) [6/4, 7/4, 8/4,   9/4])
               , mel $ map temp (map (*220) [6/5, 7/5, 8/5,   9/5])
               ]
