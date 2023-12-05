import Data.List.Extra (mconcatMap)
import qualified Day01Spec
import qualified Day02Spec
import qualified Day03Spec
import Test.Hspec (hspec)

main :: IO ()
main =
  mconcatMap
    hspec
    [ Day01Spec.suite,
      Day02Spec.suite,
      Day03Spec.suite
    ]
