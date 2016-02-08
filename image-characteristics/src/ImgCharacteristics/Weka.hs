-----------------------------------------------------------------------------
--
-- Module      :  ImgCharacteristics.Weka
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--


module ImgCharacteristics.Weka (

  learnDataToWeka

, stringifyWekaData

) where

import WekaData
import Nat.Vec
import ImgCharacteristics

import qualified Data.Set as Set

-----------------------------------------------------------------------------

learnDataToWeka :: ( Class class'
                   , Show num
                   ) =>
                Vec l String -> [class'] -> [LearnDataEntry l num class'] -> [WekaEntry]
learnDataToWeka names classes = map f
    where f (LearnDataEntry (v,c)) = WSortedEntry . zipWith WVal attrs
                                   $ map show (vec2list v) ++ [show c]
          attrs = map WekaAttrNum (vec2list names) ++ [cAttr]
          cAttr = WekaAttrNom "class" $ map show classes








