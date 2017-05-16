module Plugin 
   ( p_main
   ) where 
import Csound.Base
import Csound.Cabbage as C

ui = do
    C.cabbage $ do
        C.form $ do
            C.size 100 100
            C.pluginid "plugin"
        C.button $ do
            C.bounds 10 10 80 80
            C.channel "button"
            C.text1 "Click me"
            C.colour0 (C.Rgb 150 30 0)
            C.colour1 (C.Rgb 30 150 12)
    res <- chnGetCtrl $ text "button"
    return res

p_main = writeCsd "/home/sebastian/p/hs/pa/test.csd" $ do
    btn <- ui
    return $ btn * osc 220
