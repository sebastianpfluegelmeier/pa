module Scores 
( score1
)
where
import Csound.Base

score1 :: Sco D 
score1 = har $ [ mel $ map temp [440,       660,     880,       440 * 3 / 6]
               , mel $ map temp [880 * 5/4, 660*4/5, 660 * 4/5, 440 * 5 / 6]
               ]
