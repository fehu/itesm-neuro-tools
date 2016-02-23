-- WildfireDescriptiveStats_ClassUnknown

module Main where

import ImgCharacteristics

import Vision.Image (RGB)

import WildfireClass
import WildfireMainTemplate


main = do ci <- classProvider False :: IO (RegionsClassUnknown RGB WildfireClass)
          wildfireMain False ci




