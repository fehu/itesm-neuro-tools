-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.ImageTiles
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module ImgCharacteristics.GTK.ImageTiles (

  ImageTiles(..)
, ImageTilesWindow(..)
, imageTilesWindow

) where

import ImgCharacteristics.GTK

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader (liftIO)

import Data.List (intercalate)
import qualified Data.Text as Text

import System.Exit

-----------------------------------------------------------------------------

data ImageTilesWindow img = ImageTileWindow {
    itWindow    :: Window
  , itDestroy   :: IO ()
  , newImgTiles :: (ImageToPixbuf img) => (Int, Int) -> (Int, Int) -> IO (ImageTiles img)
}

data ImageTiles img = ImageTile {
    regionsCount :: (Int, Int)
  , regionSize   :: (Int, Int)

  , setRegion :: (ImageToPixbuf img) => (Int, Int) -> img -> Color -> IO ()
  , waitClick :: IO ()
}


imageTilesWindow = forkGUI $ do
    window <- windowNew

    wTable <- tableNew 0 0 True
    bNext <- buttonNewWithLabel "Next"
    wBox <- vBoxNew False 5

    containerAdd wBox wTable
    containerAdd wBox bNext
    containerAdd window wBox

    containerSetBorderWidth wTable 10

    let lockUI   = widgetSetSensitive bNext False
        unlockUI = widgetSetSensitive bNext True
        isLocked = not <$> widgetIsSensitive bNext

    nextVar <- newEmptyMVar
    let next = void $ tryPutMVar nextVar ()
    bNext  `on` buttonActivated $ next

    -- press Return
    let kVal = keyFromName $ Text.pack "Return"
    window `on` keyPressEvent $ do k <- eventKeyVal
                                   liftIO $ do locked <- isLocked
                                               when (kVal == k && not locked) next
                                   return True

    let clearTable = containerForeach wTable $ \w -> do containerRemove wTable w
                                                        widgetDestroy w

        waitForClick = do postGUISync unlockUI
                          takeMVar nextVar
                          postGUISync lockUI

    on window objectDestroy exitFailure
    widgetShowAll window
    lockUI

    return ImageTileWindow{
      itWindow  = window
    , itDestroy = postGUISync $ widgetDestroy window
    , newImgTiles = \rc rs -> do
                       postGUISync clearTable
                       postGUISync$ windowResize window 10 10
                       it <- imageTiles wTable waitForClick rc rs
                       postGUISync $ widgetShowAll wTable
                       return it
    }

imageTiles wTable  wClick rCount@(rows, cols) rSize = do
    postGUISync $ tableResize wTable rows cols

    let newTile :: (ImageToPixbuf img) => (Int, Int) -> img -> Color -> IO ()
        newTile (y,x) img color = do
            wImg <- imageNew
            pbuf <- img2Pixbuf img
            iCnt <- alignmentNew 0.5 0.5 0 0
            iBox <- eventBoxNew

            -- Set backbround color
            let Color r g b = color
                colorStr = "rgb(" ++ intercalate "," (map show [r,g,b]) ++ ")"
                styleStr = "* { background-color: " ++ colorStr ++ "; }"
            cProvider <- cssProviderNew
            cssProviderLoadFromString cProvider styleStr
            styleContext <- widgetGetStyleContext iBox
            styleContextAddProvider styleContext cProvider 1

            -- Set image
            imageSetFromPixbuf wImg pbuf
            -- Set margins
            let p = 2
            alignmentSetPadding iCnt p p p p
            containerSetBorderWidth iCnt p

            -- Put widgets
            containerAdd iCnt wImg
            containerAdd iBox iCnt

            tableAttachDefaults wTable iBox (x-1) x (y-1) y
            widgetShowAll iBox

    return ImageTile{
        regionsCount = rCount
      , regionSize   = rSize

      , setRegion = ((postGUISync .) .) . newTile
      , waitClick = wClick

    }

-----------------------------------------------------------------------------


