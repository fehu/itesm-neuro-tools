-- WildfireDescriptiveStats

module Main where

import ImgCharacteristics
import ImgCharacteristics.GTK

import Vision.Image (RGB)

import WildfireClass
import WildfireMainTemplate


main = do ci <- classProvider False :: IO (ClassesInterview RGB WildfireClass)
          wildfireMain True ci
