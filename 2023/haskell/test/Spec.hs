import Data.List.Extra (mconcatMap)
import qualified Day01Spec
import Test.Hspec (hspec)

main :: IO ()
main =
  mconcatMap
    hspec
    [ Day01Spec.suite
    ]
