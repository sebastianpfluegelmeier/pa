module Scores 
( score1
)
where
import Csound.Base
import Lib

score1 :: Sco D 
score1 = sco $ fmap ((220 *).double) [ 7.0/4.0
                                     , 6.0/5.0
                                     , 1.0.0.0
                                     , 4.0/3.0
                                     , 3.0/2.0
                                     , 5.0/3.0
                                     , 6.0/4.0
                                     ]

groove1 = 
    str 0.3 g1
      where 
        g1 = groove [3, 3, -2, 2,  1, 3, 3]

