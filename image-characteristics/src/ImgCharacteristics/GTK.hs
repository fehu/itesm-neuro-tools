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
--, imagesCInterview
, ImageToPixbuf(..)

, forkGUI

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

imagesCInterview :: ( Class class', ImageToPixbuf img ) =>
                         Bool -- ^ show classUnknown?
                      -> IO (ClassesInterview img class')
imagesCInterview showCU = do
    window <- windowNew
    wImg   <- imageNew
    cVar   <- newEmptyMVar
    let classes' = [minBound..maxBound]
        classes  = if showCU then classes'
                             else filter (classUnknown /=) classes'
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


instance (ImageToPixbuf img) => RegionsClassesProvider ClassesInterview img where
    regionClass = ciAskClass
    classProvider showCU = forkGUI (imagesCInterview showCU)



-----------------------------------------------------------------------------

-- | Create a separate GUI thread and wait for the return value.
forkGUI create = do var <- newEmptyMVar
                    uiThread <- forkOS $ do initGUI
                                            c <- create
                                            putMVar var c
                                            mainGUI
                    takeMVar var




-----------------------------------------------------------------------------

