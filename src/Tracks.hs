module Tracks
( track1
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

