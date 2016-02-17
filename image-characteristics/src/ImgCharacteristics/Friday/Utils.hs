    -----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.Friday.Utils
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

module ImgCharacteristics.Friday.Utils (

  Dim2Nat
, Dim2Vec(..)

, PixelDimsNat
, PixelToVec(..)

, readImage

) where

import Nat.Vec

import Vision.Primitive
import Vision.Primitive.Shape

import Vision.Image.RGB.Type
import Vision.Image.HSV.Type
import Vision.Image.Grey.Type

import qualified Codec.Picture as P
import Vision.Image.JuicyPixels (toFridayRGB)

import System.FilePath

import Data.Word

import Control.Arrow

-----------------------------------------------------------------------------

type family Dim2Nat d :: Nat where
    Dim2Nat Z = N0
    Dim2Nat (t :. h) = Succ (Dim2Nat t)

class Dim2Vec d where dim2vec :: (Dim2Nat d ~ n) => d -> Vec n Int

instance Dim2Vec Z where dim2vec _ = VNil
instance (Dim2Vec t) =>
    Dim2Vec (t :. Int) where dim2vec (t :. h) = h +: dim2vec t

-----------------------------------------------------------------------------

type family PixelDimsNat p :: Nat

type instance PixelDimsNat RGBPixel = N3
type instance PixelDimsNat HSVPixel = N3
type instance PixelDimsNat GreyPixel = N1

class PixelToVec p where pix2vec :: (PixelDimsNat p ~ n) => p -> Vec n Word8

instance PixelToVec RGBPixel where pix2vec p = rgbRed p +: rgbGreen p +: rgbBlue p +: VNil
instance PixelToVec HSVPixel where pix2vec p = hsvHue p +: hsvSat p +: hsvValue p +: VNil
instance PixelToVec GreyPixel where pix2vec (GreyPixel p) = p +: VNil

-----------------------------------------------------------------------------

readImage imgPath = do img' <- P.readImage imgPath
                       let name = takeFileName imgPath
                       return $ fmap ((toFridayRGB . P.convertRGB8) &&& const name) img'


-----------------------------------------------------------------------------


