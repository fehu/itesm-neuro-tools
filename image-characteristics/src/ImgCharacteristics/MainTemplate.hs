-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.MainTemplate
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module ImgCharacteristics.MainTemplate where

import ImgCharacteristics
import ImgCharacteristics.Friday
import ImgCharacteristics.WekaData

import Data.Either
import Control.Monad
import Control.Arrow
import Vision.Image (convert, RGB, Convertible)


-----------------------------------------------------------------------------

collectImagesCharacteristics ci ce relName imgPaths target mbSave = do
    imgs' <- mapM readImage imgPaths

    let (failed, imgs) = partitionEithers imgs'

    forM_ failed putStrLn

    cs <- collectCharacteristics ci ce mbSave $ map (first convert) imgs

    let classDomain = filter (classUnknown /=) [minBound..maxBound]
        !weka = learnDataToWeka (characteristicsNames ce) classDomain cs

    writeFile target $ stringifyWekaData relName weka

    putStrLn $ "wrote characteristics to file: " ++ target



collectCharacteristics ci ce mbSave imgs
    | null imgs = error "Nothing to do: no images provided"
    | otherwise = fmap concat . sequence $ map (uncurry $ extractLearnData ci ce mbSave) imgs


-----------------------------------------------------------------------------

