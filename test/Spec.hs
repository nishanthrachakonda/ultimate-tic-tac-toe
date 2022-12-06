--main :: IO ()
--main = putStrLn "Test suite not yet implemented"


import Test.HUnit
import Grid
import Utils
import qualified Data.Map as Map

import System.Exit


test1 :: Test
test1 = TestCase (assertEqual "Insert X" (Success Draw) (putg Map.empty 3 X))

test2 :: Test
test2 = TestCase (assertEqual "Insert O" (Success Ongoing) (putg Map.empty 3 O))


main :: IO ()
main = do
    counts <- runTestTT ( test [
        test1, 
        test2
        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
