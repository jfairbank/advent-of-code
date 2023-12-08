import Data.List.Extra (mconcatMap)
import qualified Day01Spec
import qualified Day02Spec
import qualified Day03Spec
import qualified Day04Spec
import qualified Day06Spec
import qualified Day07Spec
import qualified Day08Spec
import Test.Hspec (hspec)

main :: IO ()
main =
  mconcatMap
    hspec
    [ Day01Spec.suite,
      Day02Spec.suite,
      Day03Spec.suite,
      Day04Spec.suite,
      Day06Spec.suite,
      Day07Spec.suite,
      Day08Spec.suite
    ]
