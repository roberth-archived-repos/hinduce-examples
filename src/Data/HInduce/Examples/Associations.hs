module Data.HInduce.Examples.Associations where
import Data.HInduce.Associations.Apriori
import Text.Layout
import System.IO.Unsafe
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Arrow

import Paths_hinduce_examples

transactions = fst _tip
items = snd _tip
_tip = unsafePerformIO $ do
  name <- getDataFileName "data/T10I4D100K.dat"
  ds <- loadDataSet name
  return $ V.fromList &&& S.unions $ ds
