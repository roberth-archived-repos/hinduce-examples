{-# LANGUAGE MultiParamTypeClasses #-}
module Data.HInduce.Examples (
  -- * Re-exports
  module Data.HInduce.Classifier
  , module Data.HInduce.Classifier.DecisionTree
  , module Data.List.HIUtils
  , module Text.Layout
  , module Text.Layout.DisplayText
  , module Text.Layout.DisplayLatex
  , module Data.Convertible
  -- * Helpers (TODO move to module)
  , readCSV
  -- * Iris data set
  -- | Taken from the UCI Machine Learning Repository: <http://archive.ics.uci.edu/ml/datasets/Iris>
  -- 
  -- Let's build a decision tree and try it:
  --
  -- >>> let model = buildDTree (genMany autoDeciders) irisAttrs irisClass iris
  --
  -- >>> classify model [5,4,2,1]
  -- Setosa
  -- >>> iris !! 10
  -- Iris {sepalLength = 5.4, sepalWidth = 3.7, petalLength = 1.5, petalWidth = 0.2, irisClass = Setosa}
  --
  -- Seems good! But can we really know that?
  -- Let's train and test on separate data
  --
  -- >>> let model' = buildDTree (genMany autoDeciders) irisAttrs irisClass (oddIx iris)
  --
  -- >>> dt $ confusion' model' (map (irisAttrs &&& irisClass) $ evenIx iris)
  -- Table: Confusion Matrix
  --          ||-->Actual
  -- Predicted\/             Setosa           Versicolor           Virginica
  --     Setosa 0.3333333333333333                                         
  -- Versicolor                     0.30666666666666664              4.0e-2
  --  Virginica                    2.666666666666667e-2 0.29333333333333333
  --
  -- Now we see that even though not the whole data set was available
  -- when the model was induced, only few misclassifications occur.

  , Iris(..), IrisClass(..), irisAttrs, irisAttrs', readIris, iris
  ) where
import Paths_hinduce_examples

import Data.HInduce.Classifier
import Data.HInduce.Classifier.DecisionTree
import Text.Layout
import Text.Layout.DisplayText
import Text.Layout.DisplayLatex
import Data.Convertible
import Data.List.HIUtils
import IO
import Text.CSV
import System.IO.Unsafe

test :: FilePath -> IO FilePath
test = getDataFileName

readCSV x = do
  f <- getDataFileName $ "data/" ++ x
  parseCSVFromFile f

openDataR x = do
  f <- getDataFileName $ "data/" ++ x
  openFile f ReadMode

data IrisClass = Setosa | Versicolor | Virginica
                                       deriving (Eq, Ord, Show, Read)
instance Layout IrisClass DisplayText where format = fromShow

data Iris = Iris { sepalLength :: Double 
                 , sepalWidth :: Double 
                 , petalLength :: Double 
                 , petalWidth :: Double 
                 , irisClass :: IrisClass
                 }
          deriving (Eq, Ord, Show, Read)
  
irisAttrs (Iris p q r s _) = [p, q, r, s]
irisAttrs' (Iris p q r s _) = ((p, q), (r, s))

readIris = do
  (Right csv) <- readCSV "iris/iris.data"
  return $ map readIrisEntry $ filter (/=[""]) csv

readIrisEntry [p,q,r,s,"Iris-setosa"] = 
  Iris (read p) (read q) (read r) (read s) Setosa
readIrisEntry [p,q,r,s,"Iris-versicolor"] = 
  Iris (read p) (read q) (read r) (read s) Versicolor
readIrisEntry [p,q,r,s,"Iris-virginica"] =
  Iris (read p) (read q) (read r) (read s) Virginica

iris = unsafePerformIO readIris
