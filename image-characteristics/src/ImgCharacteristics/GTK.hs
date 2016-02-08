-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.GTK
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleInstances #-}

module ImgCharacteristics.GTK (

  ClassesInterview(ciWindow, ciAskClass, ciDestroy)
, imagesCInterview
, ImageToPixbuf(..)

, gtkExtractLearnData

) where


import Graphics.UI.Gtk

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import ImgCharacteristics


-----------------------------------------------------------------------------


data ClassesInterview a class' = ClassesInterview {
    ciWindow   :: Window
  , ciAskClass :: a -> IO class'
  , ciDestroy  :: IO ()
}

class ImageToPixbuf img where img2Pixbuf :: img -> IO Pixbuf

imagesCInterview :: ( Enum class', Bounded class', Show class'
                    , ImageToPixbuf img
                    ) => IO (ClassesInterview img class')
imagesCInterview = do
    window <- windowNew
    wImg   <- imageNew
    cVar   <- newEmptyMVar
    let classes = [minBound..maxBound]
    bBox  <- hBoxNew True $ length classes
    cBtns <- sequence $ do c <- classes
                           return $ do btn <- buttonNewWithLabel (show c)
                                       btn `on` buttonActivated $ putMVar cVar c
                                       return btn
    forM_ cBtns $ containerAdd bBox

    box   <- vBoxNew True 2
    containerAdd box wImg
    containerAdd box bBox

    containerAdd window box

    let setImage img = do pbuf <- img2Pixbuf img
                          imageSetFromPixbuf wImg pbuf
                          widgetQueueDraw wImg
        lockUI   = widgetHide bBox -- return () -- TODO
        unlockUI = widgetShow bBox -- return () -- TODO

    let askClass img = do putStrLn "! ask"
                          postGUISync $ setImage img
                          postGUISync unlockUI
                          c <- takeMVar cVar
                          postGUISync lockUI
                          return c

    widgetShowAll window
    lockUI

    return $ ClassesInterview window askClass mainQuit


instance RegionsClassesProvider ClassesInterview where regionClass = ciAskClass


gtkExtractLearnData :: ( Num num
                       , Show num --tmp
                       , RegionsExtractor img, ImageToPixbuf img
                       , Enum class', Bounded class', Show class'
                       ) =>
    CharacteristicsExtractor img num l -> img -> IO [LearnDataEntry l num class']

gtkExtractLearnData ces img = do
    askVar <- newEmptyMVar

    uiThread <- forkOS $ do initGUI
                            putStrLn "initGUI"
                            ci <- imagesCInterview
                            putStrLn "ci"
                            putMVar askVar ci
                            putStrLn "put ci in MVar"
                            mainGUI
                            putStrLn "mainGUI finished"
    ci <- takeMVar askVar
    putStrLn "got ci from MVar"

    lData <- extractLearnData ci ces img
    putStrLn "lData"
    ciDestroy ci
    return lData








