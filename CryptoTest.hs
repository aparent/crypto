import Crypto
import Test.QuickCheck
import Debug.Trace
import qualified Data.Map as M

import Text.Printf

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests


prop_getFreq_normal s = (sum . M.elems . getFreq) s - 1 < 0.001

prop_singleCharXOR_identity a b = b == (singleCharXOR a $ singleCharXOR a b)

tests = [
          ("getFreq/Normal", quickCheck prop_getFreq_normal),
          ("singleCharXOR/identity", quickCheck prop_singleCharXOR_identity)
        ]
