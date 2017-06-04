module Lib
    ( (>>>)
    , (==>)
    , (<==)
    , (>->)
    , (<->)
    , (<-<)
    , (>>-)
    , (-<<)
    , sFmap
    , sFmap2 
    , sFmap3 
    , sFmap4 
    , collectTuple
    , collectTupleA
    , toStereo
    , toStereoFX
    , liftToStereo
    , liftToStereoFX
    , uniOsc
    , stereoWidth
    , applyGroove
    , groove
    , loadSam
    , loadSam1
    ) where
import Csound.Base
import Csound.Sam
import Data.List (intersperse)

-- ################  conversion operators ################

{- conversion operator table
 -
 -  X >-= Y
 -
 -                     ************ X ******************
 -                     | Sig | Sig2 | SE Sig | SE Sig2 |
 -  *  Sig  ->    Sig  | >>> | ==>  |  <->   |   >->   |
 -  Y  Sig2 ->    Sig2 | <== | >>>  |  <-<   |   <->   |
 -  *  Sig  -> SE Sig  | >>> | ==>  |  >>=   |   >>-   |
 -  *  Sig2 -> SE Sig2 | <== | >>>  |  -<<   |   >>=   |
 -}

sFmap :: (Functor f) => (a -> b) -> f (a, a) -> f (b, b)
sFmap fn val = fmap (\(l, r) -> (fn l, fn r)) val

sFmap2 :: (Applicative f) => (a -> b -> c) -> f (a, a) -> f (b, b) -> f (c, c)
sFmap2 fn val1 val2 = (\(l1, r1) (l2, r2) -> (fn l1 l2, fn r1 r2)) <$> val1 <*> val2

sFmap3 :: (Applicative f) => (a -> b -> c -> d) -> 
                             f (a, a) ->
                             f (b, b) ->
                             f (c, c) ->
                             f (d, d)
sFmap3 fn val1 val2 val3 = (\(l1, r1) (l2, r2) (l3, r3) -> 
                               (fn l1 l2 l3 , fn r1 r2 r3)
                           ) 
                           <$> val1 <*> val2 <*> val3

sFmap4 :: (Applicative f) => (a -> b -> c -> d -> e) -> 
                             f (a, a) ->
                             f (b, b) ->
                             f (c, c) ->
                             f (d, d) ->
                             f (e, e)
sFmap4 fn val1 val2 val3 val4 = (\(l1, r1) (l2, r2) (l3, r3) (l4, r4) -> 
                                     (fn l1 l2 l3 l4, fn r1 r2 r3 r4)
                                 ) 
                                 <$> val1 <*> val2 <*> val3 <*> val4


(>>>) :: a -> (a -> b) -> b
val >>> fn = fn val

(==>) :: (a, a) -> (a -> a) -> (a, a) 
(valL, valR) ==> fn = (fn valL, fn valR)

(<==) :: a -> ((a, a) -> (b, b)) -> (b, b)
val <== fn = fn (val, val)

(>->) :: (Functor f) => f (a, a) -> (a -> b) -> f (b, b)
val >-> fn = fmap (\(l, r) -> (fn l, fn r)) val

(<-<) :: (Functor f) => (f a) -> ((a, a) -> (b, b)) -> f (b, b)
val <-< fn = fmap (\x -> fn (x, x)) val

(<->) :: (Functor f) => f a -> (a -> b) -> f b
val <-> fn = fmap fn val

(>>-) :: (Monad m) => m (a, a) -> (a -> m b) -> m (b, b)
val >>- fn = val >>= (\(x, y) -> fmap (\(a:b:_) -> (a, b)) $ sequence [fn x, fn y])

(-<<) :: (Monad m) => m a -> ((a, a) -> m (b, b)) -> m (b, b)
val -<< fn = val >>= (\x -> fn (x, x))

-- ################  conversion functions  ################

collectTuple :: (Monad m) => (m a, m a) -> m (a, a)
collectTuple (x, y) = fmap (\(a:b:_) -> (a, b)) $ sequence [x, y]

collectTupleA :: (Applicative m) => (m a, m a) -> m (a, a)
collectTupleA (x, y) = fmap (\(a:b:_) -> (a, b)) $ sequenceA [x, y]

toStereo :: a -> (a, a)
toStereo x = (x, x)

liftToStereo :: (Applicative f) => f a -> f (a, a)
liftToStereo x = collectTupleA (x, x)

toStereoFX :: (a -> b) -> a -> (b, b)
toStereoFX fn val = (fn val, fn val)

liftToStereoFX :: (Applicative f) => (a -> b) -> f a -> f (b, b)
liftToStereoFX fn val = collectTupleA (fmap fn val, fmap fn val)

-- ################    audio functions     ################

-- TODO: solve : phase problem
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

-- ################  Score Functions  ################

applyGroove :: Sco a -> Sco b -> Sco b
applyGroove groove score =
    har $ zipWith 
        (\(start, length) content -> 
            singleEvent start length content)
        grooveTime 
        scoreContent
      where 
        grooveTime = fmap (\x -> (eventStart x, eventDur x)) renderedGroove
        renderedGroove = render groove
        scoreContent = fmap eventContent renderedScore
        renderedScore = render score

groove :: [Double] -> Sco ()
groove nList =
    mel $ fmap note nList
      where 
        note :: Double -> Sco ()
        note x = if x > 0 then
                   str (double x) $ temp ()
                 else
                   rest (double (-x))

{- a function to create scores easily
 - you can write notes in the form "c d d# e"
 - it creates a score with a whole note c, a whole note d, a whole note d# etc.
 - you can also write notes in the form "#1/4 c d $1/2 d# e"
 - it creaes a score with a quarter note c and d, then a half note d# and then a quarter note e"
 - the #length sets the note length for all folowing notes
 - the $length sets the length for one following note
 - you can write a pause with "_" 
 - you can set the octave with ''' or '3 and ,,, or ,3 and $,3 for a single note
sc :: [Char] -> Sco D
sc string = sc' 0
  where
    sc' :: Int -> Double -> [Char] -> Sco D
    sc' _ _ [] = rest 0
    sc' octave length string = 
        |isNote $ firstToken string =
            mix [note, sc' octave length ()]
 -}


-- ################  Sampler Fucntions  ################

loadSam :: [Char] -> SE Sig2
loadSam directory = 
    runSam 1 $ wav samDir 
      where 
        samDir = "/home/sebastian/samples/" ++ directory

loadSam1 :: [Char] -> SE Sig2
loadSam1 directory = 
    runSam 1 $ wav1 samDir 
      where 
        samDir = "/home/sebastian/samples/" ++ directory

