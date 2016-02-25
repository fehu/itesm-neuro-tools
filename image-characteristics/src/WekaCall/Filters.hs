-----------------------------------------------------------------------------
--
-- Module      :  WekaCall.Filters
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ExistentialQuantification #-}

module WekaCall.Filters (


  withFilter
, applyFilter

, SomeFilter(..)
, successiveFilters

, nominalToBinary
, removeWithValues

) where

import Weka.Core (Attribute, Instances')
import Weka.Classifiers
import Weka.Filters (Filter, MultiFilter''(..), MultiFilter')
import Weka.Classifiers.Meta (FilteredClassifier''(..), FilteredClassifier')
import Weka.Filters.Unsupervised.Instance(RemoveWithValues''(..), RemoveWithValues')
import Weka.Filters.Unsupervised.Attribute(NominalToBinary''(..), NominalToBinary')

import qualified Weka.Classifiers.Meta.FilteredClassifier as FilteredClassifier
import qualified Weka.Filters.MultiFilter as MultiFilter
import qualified Weka.Filters.Filter as Filter
import qualified Weka.Filters.Unsupervised.Instance.RemoveWithValues as RemoveWithValues
import qualified Weka.Core.Attribute as Attribute

import qualified Java.Lang.Integer as JInteger

import qualified Weka.WekaCalls as Helper

-----------------------------------------------------------------------------

import JUtils

import Data.Int
import Data.Maybe (fromJust)
import Data.List (intercalate)

import Control.Monad

-----------------------------------------------------------------------------

instance JavaClassID FilteredClassifier''
    where classId _ = "weka.classifiers.meta.FilteredClassifier"

instance JavaClassID MultiFilter'' where classId _ = "weka.filters.MultiFilter"



instance JavaClassID NominalToBinary''
    where classId _ = "weka.filters.unsupervised.attribute.NominalToBinary"

instance JavaClassID RemoveWithValues''
    where classId _ = "weka.filters.unsupervised.instance.RemoveWithValues"

-----------------------------------------------------------------------------

data SomeFilter = forall f . Filter f => SomeFilter f

instance JavaObject SomeFilter where asObject (SomeFilter f) = asObject f

successiveFilters :: [SomeFilter] -> Java MultiFilter'
successiveFilters filts = do Just multif <- newInstance0 MultiFilter
--                             Just lst <- Helper.filtersList multif
                             filts' <- mapM asObject filts
--                             appendToJList lst filts'
                             lst <- jObjectsList filts'
                             Helper.setFiltersList multif lst :: Java ()
                             return multif


-- | Link a dilter with a classifier (stream filter).
withFilter :: (Filter f, Classifier c) =>
              f -- ^ Filter.
           -> c -- Classifier.
           -> Java FilteredClassifier'

withFilter f c = do Just fc <- newInstance0 FilteredClassifier
                    FilteredClassifier.setFilter fc f :: Java ()
                    FilteredClassifier.setClassifier fc c :: Java ()
                    return fc

-- | Apply a filter to instances (batch filter).
applyFilter :: (Filter f) => Instances' -> f -> Java Instances'
applyFilter instances f = do Filter.setInputFormat f instances :: Java Bool
                             liftM fromJust $ Filter.useFilter instances f

-----------------------------------------------------------------------------

nominalToBinary = return . fromJust =<< newInstance0 NominalToBinary

removeWithValues :: (Attribute attr) =>
                    attr
                 -> [String]
                 -> Java RemoveWithValues'
removeWithValues a vs = do Just f <- newInstance0 RemoveWithValues
                           ai  <- Attribute.index a :: Java Int32
                           Just ais <- JInteger.toString'' $ ai + 1
                           aVsCount <- Attribute.numValues a :: Java Int32
                           vInds <- mapM (jString >=> Attribute.indexOfValue a) vs :: Java [Int32]
                           let rangeList = map (1+) vInds
                           rangeListStr <- jString . intercalate "," $ map show rangeList
                           RemoveWithValues.setAttributeIndex f ais :: Java ()
                           RemoveWithValues.setNominalIndices f rangeListStr :: Java ()
                           return f



