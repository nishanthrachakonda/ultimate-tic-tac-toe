

import Test.HUnit
import Grid
import Utils
import qualified Data.Map as Map

import Test.HUnit
import System.Exit


test1 = TestCase (assertEqual "Insert X" (Success) (put Map.empty 3 X))


main :: IO ()
main = do
    counts <- runTestTT ( test [
        test1
        ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure