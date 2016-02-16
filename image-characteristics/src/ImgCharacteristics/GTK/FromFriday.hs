-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.GTK.FromFriday
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module ImgCharacteristics.GTK.FromFriday where

import ImgCharacteristics.GTK

import Graphics.UI.Gtk.Gdk.Pixbuf

import Vision.Primitive
import Vision.Image hiding (map)

import Data.Ix (range)
import Data.Array.MArray (writeArray)

-----------------------------------------------------------------------------


instance ImageToPixbuf RGB  where img2Pixbuf = rgbImg2Pixbuf
instance ImageToPixbuf HSV  where img2Pixbuf = rgbImg2Pixbuf . (convert :: HSV  -> RGB)
instance ImageToPixbuf Grey where img2Pixbuf = rgbImg2Pixbuf . (convert :: Grey -> RGB)



-- from http://stackoverflow.com/questions/25417109/how-to-display-an-image-from-repa-devil-in-gtkimage-in-haskell
rgbImg2Pixbuf img =
    do let Z :. h :. w = shape img
           n = nChannels img
       pbuf <- pixbufNew ColorspaceRgb False 8 w h
       rowStrideBytes <- pixbufGetRowstride pbuf
       pixbufPixels <- pixbufGetPixels pbuf
       let copyPixel (x, y) = sequence_ $ do i <- [0..n-1]
                                             return $ writeArray pixbufPixels
                                                                 (y * rowStrideBytes + x * n + i)
                                                                 (pixIndex pixel i)
                            where pixel = index img (Z:. y :. x)
       mapM_ copyPixel $ range ((0, 0), (w-1, h-1))
       return pbuf
