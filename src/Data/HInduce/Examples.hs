-- | This package / module provides example data and example code to help you
-- get started with HInduce. You are advised to import this module (ghci: 
-- @:m + Data.HInduce.Examples@), not the individual ones below, because
-- @Data.HInduce.Examples@ re-exports modules that are required to run the
-- examples yourself.
--
-- Click on a module below to view the examples you're interested in. Also note
-- the grey "Source" links at the right hand site of the webpage.
module Data.HInduce.Examples ( -- * The Examples
                             module Data.HInduce.Examples.DecisionTree
                             , module Data.HInduce.Examples.Associations
  -- * Re-exports for convenience
  , module Data.HInduce.Classifier
  , module Data.HInduce.Classifier.DecisionTree
  , module Data.HInduce.Associations.Apriori
  , module Data.List.HIUtils
  , module Text.Layout
                             ) where

import Data.HInduce.Examples.DecisionTree
import Data.HInduce.Examples.Associations
import Data.HInduce.Classifier
import Data.HInduce.Classifier.DecisionTree
import Data.HInduce.Associations.Apriori
import Data.List.HIUtils
import Text.Layout

