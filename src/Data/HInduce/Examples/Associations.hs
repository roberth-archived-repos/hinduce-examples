-- | An example of association rule mining:
--
-- >>> rules transactions items (top ((take 40) . (filter (\(_,a)->a>= 60))))
-- > fromList [((fromList [32],fromList [947]),0.1694915254237288),((fromList [39],fromList [145]),0.17238139971817754),((fromList [39],fromList [145,419]),8.266791921089714e-2),((fromList [39],fromList [368]),0.1326914044152184),((fromList [39],fromList [419]),0.1200093940817285),
module Data.HInduce.Examples.Associations where
import Data.HInduce.Associations.Apriori
import Text.Layout
import System.IO.Unsafe
import qualified Data.Set as S
import qualified Data.Vector as V
import Control.Arrow
import Data.Set
import Data.Vector

import Paths_hinduce_examples

-- | The transactions in the T10I4D100K.dat data set.
transactions :: Data.Vector.Vector (Data.Set.Set Int)

-- | The items in the T10I4D100K.dat data set.
items :: Data.Set.Set Int

(transactions, items) = unsafePerformIO $ do
  name <- getDataFileName "data/T10I4D100K.dat"
  ds <- loadDataSet name
  return $ V.fromList &&& S.unions $ ds

