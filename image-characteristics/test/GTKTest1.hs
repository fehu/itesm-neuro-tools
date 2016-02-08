
{-# LANGUAGE FlexibleInstances, TypeOperators, FlexibleContexts #-}

module Main where

import Nat.Vec

import ImgCharacteristics
import ImgCharacteristics.GTK
import ImgCharacteristics.Friday

import System.Environment
import System.Exit
import Control.Monad

import Vision.Primitive
import Vision.Image hiding (map)
import Vision.Image.JuicyPixels (toFridayRGB)
import Vision.Image.RGB.Type
import Codec.Picture

import Graphics.UI.Gtk.Gdk.Pixbuf

import Data.Ix (range)
import Data.Array.MArray (writeArray)

main = getArgs >>= parseArgs

fixedRegions = FixedColRowRegions{ rRow = 5
                                 , rCol = 5
                                 , minRegionSize = (200, 200)
                                 }           -- (height, width)


data Class = A | B | C deriving (Show, Enum, Bounded)

instance RegionsExtractor RGB where foreachRegion = fixedColRowRegions fixedRegions
                                    foreachRegionIO = fixedColRowRegions fixedRegions

-- from http://stackoverflow.com/questions/25417109/how-to-display-an-image-from-repa-devil-in-gtkimage-in-haskell
instance ImageToPixbuf RGB where
    img2Pixbuf img = do let Z :. h :. w = shape img
                        pbuf <- pixbufNew ColorspaceRgb False 8 w h
                        rowStrideBytes <- pixbufGetRowstride pbuf
                        let rowStride = rowStrideBytes `quot` 3
                        pixbufPixels <- pixbufGetPixels pbuf
                        let copyPixel (x, y) = writeArray pixbufPixels
                                                         (y * rowStride + x)
                                                         (index img (Z:. y :. x))
                        mapM_ copyPixel $ range ((0, 0), (w-1, h-1))
                        return pbuf


parseArgs [imgPath] = do putStrLn $ "Processing " ++ imgPath ++ " ...\n"
                         Right img' <- readImage imgPath
                         let img = toFridayRGB $ convertRGB8 img'

                             ce :: CharacteristicsExtractor RGB Double (N8 :*: N3)
                             ce = extractorRGB descriptiveStats

                         cs <- gtkExtractLearnData ce img :: IO [LearnDataEntry (N8 :*: N3) Double Class]

                         putStrLn "Characteristics:"
                         forM_ (vec2list $ characteristicsNames ce) $ putStrLn . ("\t" ++ )
                         putStrLn "Characteristic Vectors:"
                         forM_ cs print



parseArgs args = putStrLn ("wrong arguments: " ++ show args) >> exitFailure

