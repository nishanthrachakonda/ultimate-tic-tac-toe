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

test3 :: Test
test3 = TestCase (assertEqual "Up 1" (1) (up 1))

test4 :: Test
test4 = TestCase (assertEqual "Up 8" (5) (up 8))

test5 :: Test
test5 = TestCase (assertEqual "Down 9" (9) (down 9))

test6 :: Test
test6 = TestCase (assertEqual "Down 4" (7) (down 4))

test7 :: Test
test7 = TestCase (assertEqual "Left 4" (3) (left 4))

test8 :: Test
test8 = TestCase (assertEqual "Left 6" (5) (left 6))

test9 :: Test
test9 = TestCase (assertEqual "Left 1" (1) (left 1))

test10 :: Test
test10 = TestCase (assertEqual "Right 7" (8) (right 7))

test11 :: Test
test11 = TestCase (assertEqual "Right 9" (9) (right 9))

test12 :: Test
test12 = TestCase (assertEqual "Right 3" (4) (right 3))

test13 :: Test
test13 = TestCase (assertEqual "Insert into same position" (Error) (put (Map.fromList([(3,O)])) 3 O))

test14 :: Test
test14 = TestCase (assertEqual "Insert into different position" (Success Ongoing) (put (Map.fromList([(3,O)])) 4 O))

test15 :: Test
test15 = TestCase (assertEqual "Draw scenario" (Success Draw) (put (Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X)])) 9 O))

test16 :: Test
test16 = TestCase (assertEqual "Win X - Row case" (Success (Win X)) (put (Map.fromList([(3,X), (1,X), (4,O), (5,O)])) 2 X))


test17 :: Test
test17 = TestCase (assertEqual "Win O - Column case" (Success (Win O)) (put (Map.fromList([(1,O), (2,X), (6,O), (8,X)])) 5 X))


test18 :: Test
test18 = TestCase (assertEqual "Win O - Diagonal case" (Success (Win O)) (put (Map.fromList([(1,O), (2,X), (3,X), (5,O)])) 9 O))


test19 :: Test
test19 = TestCase (assertEqual "Win X - Anti Diagonal case" (Success (Win X)) (put (Map.fromList([(2,O), (3,X), (5,X), (6,O)])) 7 X))

main :: IO ()
main = do
    counts <- runTestTT ( test [
        test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19
        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
