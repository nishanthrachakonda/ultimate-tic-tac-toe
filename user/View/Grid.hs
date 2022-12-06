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
view' s = mkBoard s board
          where
            board = psBoard s

mkBoard :: PlayState -> Board -> Widget n
mkBoard s b = withBorderStyle unicode $
              vTile ([ mkgRow s row b | row <- [1..3]])

mkgRow :: PlayState -> Int -> Board -> Widget n
mkgRow s r b = hTile [ (padAll 2 (mkGrid s ((r-1)*3+i) (getGrid ((r-1)*3+i) b))) | i <- [1..3] ]

mkGrid :: PlayState -> Int -> Grid -> Widget n
mkGrid s gp g = withBorderStyle unicode $
                 vTile [ mkRow s i gp g | i <- [1..3] ]

mkRow :: PlayState -> Int -> Int -> Grid -> Widget n
mkRow s r gp g = hTile [ mkCell s ((gp, (r-1)*3+i)) g | i <- [1..3] ]

mkCell :: PlayState -> CurPos -> Grid -> Widget n
mkCell s cp g | isCurr s cp = withCursor raw 
              | otherwise    = raw 
               where
                 raw = mkCell' cp g

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: CurPos -> Grid -> Widget n
mkCell' cp g = center (mkValue v)
  where 
    v      = Map.lookup (snd cp) g

mkValue :: Maybe Value -> Widget n
mkValue (Just X) = blockX
mkValue (Just O) = blockO
mkValue Nothing  = blockB

getGrid :: Int -> Board -> Grid
getGrid i b = grid
              where gridL = Map.lookup i b
                    grid | isNothing gridL = Map.empty
                         | otherwise       = snd (fromJust gridL)   

blockB, blockX, blockO :: Widget n
blockB = str " "
blockX = str "X" 
blockO =  str "O"

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox ( b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b| b <- bs])
hTile _      = emptyWidget