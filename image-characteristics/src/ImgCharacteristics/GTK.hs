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
import System.Exit

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
    bBox  <- hButtonBoxNew
    cBtns <- sequence $ do c <- classes
                           return $ do btn <- buttonNewWithLabel (show c)
                                       btn `on` buttonActivated $ putMVar cVar c
                                       return btn
    forM_ cBtns $ containerAdd bBox

    box   <- vBoxNew False 0
    fixed <- fixedNew
    containerAdd box wImg
    containerAdd box bBox

    containerAdd window box

    on window objectDestroy exitFailure

    let setImage img = do pbuf <- img2Pixbuf img
                          imageSetFromPixbuf wImg pbuf
                          widgetQueueDraw wImg
        lockUI   = widgetSetSensitive bBox False
        unlockUI = widgetSetSensitive bBox True

    let askClass img = do postGUISync $ setImage img
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
                            ci <- imagesCInterview
                            putMVar askVar ci
                            mainGUI
    ci <- takeMVar askVar
    lData <- extractLearnData ci ces img
    ciDestroy ci
    return lData








