import Test.HUnit
import Grid
import Utils
import Board
import qualified Data.Map as Map
import System.Exit


test1 :: Test
test1 = TestCase (assertEqual "Insert X" (Success,Just (Ongoing, Map.fromList [(3,X)])) (putg Map.empty 3 X))

test2 :: Test
test2 = TestCase (assertEqual "Insert O" (Success,Just (Ongoing, Map.fromList [(3,O)])) (putg Map.empty 3 O))

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
test13 = TestCase (assertEqual "Insert into same position" (Error, Nothing) (putg (Map.fromList([(3,O)])) 3 O))

test14 :: Test
test14 = TestCase (assertEqual "Insert into different position" (Success,Just (Ongoing,Map.fromList [(3,O),(4,O)])) (putg (Map.fromList([(3,O)])) 4 O))

test15 :: Test
test15 = TestCase (assertEqual "Draw scenario" (Success,Just (Draw,Map.fromList [(1,X),(2,O),(3,X),(4,X),(5,O),(6,X),(7,O),(8,X),(9,O)])) (putg (Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X)])) 9 O))

test16 :: Test
test16 = TestCase (assertEqual "Win X - Row case" (Success,Just (Win X,Map.fromList [(1,X),(2,X),(3,X),(4,O),(5,O)])) (putg (Map.fromList([(3,X), (1,X), (4,O), (5,O)])) 2 X))


test17 :: Test
test17 = TestCase (assertEqual "Win X - Column case" (Success,Just (Win X,Map.fromList [(1,O),(2,X),(5,X),(6,O),(8,X)])) (putg (Map.fromList([(1,O), (2,X), (6,O), (8,X)])) 5 X))


test18 :: Test
test18 = TestCase (assertEqual "Win O - Diagonal case" (Success,Just (Win O,Map.fromList [(1,O),(2,X),(3,X),(5,O),(9,O)])) (putg (Map.fromList([(1,O), (2,X), (3,X), (5,O)])) 9 O))


test19 :: Test
test19 = TestCase (assertEqual "Win X - Anti Diagonal case" (Success,Just (Win X,Map.fromList [(2,O),(3,X),(5,X),(6,O),(7,X)])) (putg (Map.fromList([(2,O), (3,X), (5,X), (6,O)])) 7 X))

test20 :: Test
test20 = TestCase (assertEqual "Special case of win in an almost draw" (Success, Just (Win X, Map.fromList [(1,X),(2,O),(3,O),(4,O),(5,X),(6,X),(7,X),(8,O),(9,X)])) (putg (Map.fromList([(1,X),(2,O), (3,O),(4,O),(5,X),(6,X), (7,X), (8,O)])) 9 X))

test21 :: Test
test21 = TestCase (assertEqual "Insert X in empty board" (Success,Just (Ongoing,Map.fromList [(1,(Ongoing,Map.fromList [(3,X)]))])) (putb Map.empty 0 (1,3) X))

test22 :: Test
test22 = TestCase (assertEqual "Insert O in empty board" (Success,Just (Ongoing,Map.fromList [(4,(Ongoing,Map.fromList [(7,O)]))])) (putb Map.empty 0 (4,7) O))

test23 :: Test
test23 = TestCase (assertEqual "Insert O in non-empty board" (Success,Just (Ongoing,Map.fromList [(4,(Ongoing,Map.fromList [(7,O)])),(6,(Ongoing,Map.fromList [(4,X)]))])) (putb (Map.fromList([(6,(Ongoing,(Map.fromList([(4,X)])) ))])) 4 (4,7) O))

test24 :: Test
test24 = TestCase (assertEqual "Insert x in invalid grid (ppos != cur pos grid)" (Error, Nothing) (putb (Map.fromList([(6,(Ongoing,(Map.fromList([(4,X)])) ))])) 4 (8,7) O))

test25 :: Test
test25 = TestCase (assertEqual "Insert X in Draw grid" (Error, Nothing) (putb (Map.fromList([(6,(Ongoing,(Map.fromList([(4,X)])) )), (8,(Draw,(Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X), (9,O)])) )) ])) 8 (8,7) X))

test26 :: Test
test26 = TestCase (assertEqual "Insert X in (Win X) grid" (Error, Nothing) (putb (Map.fromList([(3,((Win X),(Map.fromList([(2,O), (3,X), (5,X), (6,O), (7,X)])) )),(6,(Ongoing,(Map.fromList([(4,X)])) )), (8,(Draw,(Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X), (9,O)])) )) ])) 3 (3,2) X))

test27 :: Test
test27 = TestCase (assertEqual "Insert O in (Win O) grid" (Error, Nothing) (putb (Map.fromList([(3,((Win X),(Map.fromList([(2,O), (3,X), (5,X), (6,O), (7,X)])) )),(5, ((Win O), (Map.fromList([(1,O), (2,X), (3,X), (5,O), (9,O)])) )), (6,(Ongoing,(Map.fromList([(4,X)])) )), (8,(Draw,(Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X), (9,O)])) )) ])) 5 (5,9) O))

test28 :: Test
test28 = TestCase (assertEqual "Insert O in ongoing grid and wins" (Success,Just (Ongoing,Map.fromList [(3,(Win X,Map.fromList [(2,O),(3,X),(5,X),(6,O),(7,X)])),(5,(Win O,Map.fromList [(1,O),(2,X),(3,X),(5,O),(9,O)])),(6,(Ongoing,Map.fromList [(4,X)])),(8,(Draw,Map.fromList [(1,X),(2,O),(3,X),(4,X),(5,O),(6,X),(7,O),(8,X),(9,O)]))])) (putb (Map.fromList([(3,((Win X),(Map.fromList([(2,O), (3,X), (5,X), (6,O), (7,X)])) )),(5, ((Ongoing), (Map.fromList([(1,O), (2,X), (3,X), (5,O)])) )), (6,(Ongoing,(Map.fromList([(4,X)])) )), (8,(Draw,(Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X), (9,O)])) )) ])) 5 (5,9) O))

test29 :: Test
test29 = TestCase (assertEqual "Moveb up 4th grid" (4,1) (moveb up (4,4)))

test30 :: Test
test30 = TestCase (assertEqual "Moveb up 1st grid" (1,1) (moveb up (1,3)))

test31 :: Test
test31 = TestCase (assertEqual "Moveb up between grids" (2,1) (moveb up (5,3)))

main :: IO ()
main = do
    counts <- runTestTT ( test [
        test1 , test2, test3, test4, test5, 
        test6, test7, test8, test9, test10, 
        test11, test12, test13, test14, test15, 
        test16, test17, test18, test19, test20, 
        test21, test22, test23, test24, test25, 
        test26, test27, test28, test29, test30, test31
        ])
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
