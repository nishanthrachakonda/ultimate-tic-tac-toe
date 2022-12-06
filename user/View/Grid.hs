module View.Grid (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)
import Data.Maybe

import Model
import Grid
import Board
import Utils
import qualified Data.Map as Map
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = vLimit 35 $ hLimit 130 $ mkBoard board
          where
            board = (Map.fromList([(1,(Ongoing,(Map.fromList([(4,X),(6,X),(9,O)])) )), (2,(Ongoing,(Map.fromList([(4,X), (5,O), (6,X)])) )), (6,(Ongoing,(Map.fromList([(4,X)])) )),(3,(Draw,(Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X), (9,O)])) )), (8,(Draw,(Map.fromList([(1,X), (2,O), (3,X), (4,X), (5,O), (6,X), (7,O), (8,X), (9,O)])) )) ])) -- psBoard s
  -- withBorderStyle unicode $
      -- vTile [ mkRow s row | row <- [1..3] ]


mkRow :: Int -> Grid -> Widget n
mkRow row g = hTile [ mkCell row i g | i <- [1..3] ]

mkCell :: Int -> Int -> Grid -> Widget n
mkCell r c g = raw
  -- | isCurr s r c = withCursor raw 
  -- | otherwise    = raw 
  where
    raw = mkCell' r c g

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: Int -> Int -> Grid -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' r c g = center (hLimit 5 $ vLimit 3 $ (mkValue xoMb))
  where 
    xoMb      = Map.lookup ((r-1)*3+c) g
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkValue :: Maybe Value -> Widget n
mkValue (Just X) = blockX
mkValue (Just O) = blockO
mkValue Nothing = blockB

mkBoard ::  Board -> Widget n
mkBoard b = withBorderStyle unicode $
            vTile ([ mkgRow row b | row <- [1..3]])
mkgRow :: Int -> Board -> Widget n
--mkgRow row = hTile [ (padAll 2 (mkGrid Map.empty)) | i <- [1..3] ]
mkgRow row b = hTile [ (padAll 2 (hLimit 40 $ mkGrid (getGrid ((row-1)*3+i) b))) | i <- [1..3] ]
       

getGrid :: Int -> Board -> Grid
getGrid i b = grid
              where gridL = Map.lookup i b
                    grid | isNothing gridL = Map.empty
                         | otherwise       = snd (fromJust gridL)   
                       

mkGrid ::  Grid -> Widget n
mkGrid g = withBorderStyle unicode $ hLimit 40 $ vLimit 30 $
            vTile [ mkRow row g | row <- [1..3] ]


blockB, blockX, blockO :: Widget n
blockB = str " "--vBox [ str " "]
blockX = str "X" --vBox [ str "X"]
blockO =  str "O" --vBox [ str "O"]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox ( b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b| b <- bs])
hTile _      = emptyWidget