module Scores 
( score1
, groove1
, score2
, groove2
)
where
import Csound.Base
import Lib

toScore :: [Double] -> Sco D 
toScore list = mel $ fmap (temp.double) list

list1 = [1.0,2.0,1.0,2.0,1.0]
list2 = [2.0,3/2,4/3,5/6,5/3]
list3 = [9/5,8/4,7/3,6/2,8/5]

score1 = toScore $ fmap (* 125) $ list1 ++ list1 ++ list2 ++ list3

groove1 = 
    str 0.3 $ loopBy 4 g1
      where 
        g1 = groove [1, 1, -2, 1,  -1, 2, 8]

groove2 = 
    str 0.3 $ loopBy 4 g1
      where
        g1 = groove [2, 2, -1, 3, 3, 5]

score2 = toScore $ fmap (* (125/2)) $ list1 ++ list4 ++ list1 ++ list5

list4 = [1.0, 6/2, 2.0, 1.0, 6/4]
list5 = [1.0, 10/4, 6/2, 2.0, 9/5]

