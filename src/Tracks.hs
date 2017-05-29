module Tracks
( track1
, bd
, sn 
, h1
, h2
, ho
, p1
, p2
, p3
, sh
, cr
, unGrooved1
, groove1
, groove2
, grooved1
, grooved2
, grooved3
, grooved4
, shgroove
, mixGroove1
, padSynth
)
where 
import Csound.Base
import Csound.Sam   
import Lib

track1 :: SE Sig2
track1 = return $ toStereo $ sig 0

-- ### Drum Samples ###

sn :: D -> SE Sig2
sn _ = loadSam "drums_and_single_hits/snares/metal-sd3.wav"

bd :: D -> SE Sig2
bd _ = 
    (+) <$> fadeInOut 0.2 4 kick1 <*> fadeInOut 0.001 0.3 kick2
      where
        fadeInOut in_ out = (fmap (toStereoFX (\x -> 1 * x * fadeIn in_ * fadeOut out)))
        kick1 = loadSam1 "drums_and_single_hits/kicks/giant-afro-kick1.wav"
        kick2 = loadSam1 "drums_and_single_hits/kicks/gorgeous-kick.wav"

h1 :: D -> SE Sig2
h1 _ = fmap (toStereoFX (* 0.7)) $ loadSam1 "drums_and_single_hits/hats/clear-hat.wav"

h2 :: D -> SE Sig2
h2 _ = fmap (toStereoFX (* 0.5)) $ loadSam1 "drums_and_single_hits/hats/chh.wav"

ho :: D -> SE Sig2
ho _ = fmap (toStereoFX (* 0.7)) $ loadSam1 "drums_and_single_hits/hats/bigopenhh.wav"

p1 :: D -> SE Sig2
p1 _ = fmap (toStereoFX (* 0.7)) $ loadSam1 "drums_and_single_hits/tabla/tabla5.aif"

p2 :: D -> SE Sig2
p2 _ = fmap (toStereoFX (* 0.7)) $ loadSam1 "drums_and_single_hits/tabla/dhi.aif"

p3 :: D -> SE Sig2
p3 _ = fmap (toStereoFX (* 0.7)) $ loadSam1 "drums_and_single_hits/japanese/shime_hi_2.aif"

sh :: D -> SE Sig2
sh x = fmap (toStereoFX (* (sig x * 0.6))) $ loadSam1 
           "drums_and_single_hits/percussion_western_and_latin/maraca.aif"

cr :: D -> SE Sig2
cr _ = fmap (toStereoFX (* 0.2)) $ loadSam1 
           "drums_and_single_hits/crashes/muted_crash.aif"

crash :: Sco (Mix (Sig2))
crash = str 0.11 $ sco cr (temp 0)

shgroove :: Sco (Mix (Sig2))
shgroove = str 0.11 $ mel $ fmap (\x -> sco sh (temp x)) [1, 0.3,0.6, 0.4]

groove1 :: Sco ()
groove1 = str 0.11 $ groove [3, 1, 2, 1, 1, 2, 2, 1, 2, 1]
groove2 = str 0.11 $ groove [1, 3, 2, 1, 1, 2, 2, 1, 2, 1]

unGrooved1 = mel $ fmap (\x -> sco x $ temp $ double 1) $ [bd, h1, sn, ho, bd, p2, h2, sn, h1, p3]
unGrooved2 = mel $ fmap (\x -> sco x $ temp $ double 1) $ [bd, ho, sn, h2, h1, h2, bd, sn, p3, h1]

grooved1 = applyGroove groove1 unGrooved1
grooved2 = applyGroove groove2 unGrooved1
grooved3 = applyGroove groove1 unGrooved2
grooved4 = applyGroove groove2 unGrooved2

mixGroove1 = 
    mix $ 
        loopBy 2 $ 
            har 
                [ loopBy 32 shgroove
                , loopBy 2  $ str 64 $ crash
                , mel 
                    [ grooved1
                    , grooved2
                    , grooved1
                    , grooved3
                    , grooved2
                    , grooved3
                    , grooved2
                    , grooved4
                    ]
                ]

padSynth :: D -> SE Sig2
padSynth freq =
    (\(xl, xr) (yl, yr) -> 
        ((filter $ env1 * xl + env2 * yl), 
         (filter $ env1 * yr + env2 * xr)
        )
    ) <$> 
    uniOsc osc1 (sig 50) 3 (sig freq) <*> 
    uniOsc osc2 (70 * env1) 3 (sig freq)
      where
        env1 :: Sig
        env1 = fadeIn (0.1 + decay / 10) * fOut (decay * 1.4)
        env2 :: Sig
        env2 = fOut decay
        osc1 :: Sig -> Sig
        osc1 x = mlp ((4 + env1 * 2 * (0.5 + uosc 2 * 100) ) * x) 1.9 
                   (osc (x + 10000 * sqr (4.1 * x))) + 
               osc (x + 100 * env2 * osc (x * 4))
        osc2 :: Sig -> Sig
        osc2 x = oscBy (sines [1, 0.2, 1.5, 0.1, 1.8, 0.4, 0.2, 0.1, 0.1, 1.01]) x
        filter :: Sig -> Sig
        filter = lp (sig freq * 2 * (1 + 2 * env1 * uosc (1 + 8 * env2))) 1.4
        fOut :: D -> Sig
        fOut x = linseg [1, x, 0]
        decay = 5

-- padScore :: Sco D

{-
padScore = fmap (fmap cpspch) $
                            [ [ 3.00
                              , 3.03
                              , 3.10
                              , 4.02
                              ]
                            , [ 3.05
                              , 3.08
                              , 3.12
                              , 4.07
                              ]
                            , [ 2.10
                              , 3.01
                              , 3.05
                              , 3.12
                              ]
                            , [ 3.05
                              , 3.08
                              , 3.12
                              , 4.07
                              ]
                            ]
                            -}
