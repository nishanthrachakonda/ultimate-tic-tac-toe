module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Board
import Utils
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
-- view s = [view' s]
view s = [vBox [ str "X   X"
              , str " X X "
              , str "  X  "
              , str " X X " 
              , str "X   X"]]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
      vTile [ mkRow s row | row <- [1..3] ]


mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ mkCell s row i | i <- [1..3] ]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell s r c = raw
--   | isCurr s r c = withCursor raw 
--   | otherwise    = raw 
  where
    raw = mkCell' s r c

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkXO xoMb)
  where 
    xoMb      = Just X -- psBoard s ! Pos r c
    -- xoMb 
    --   | r == c    = Just X 
    --   | r > c     = Just O 
    --   | otherwise = Nothing

mkXO :: Maybe Value -> Widget n
mkXO Nothing  = blockB
mkXO (Just X) = blockX
mkXO (Just O) = blockO

blockB, blockX, blockO :: Widget n
blockB = vBox (replicate 5 (str "     "))
blockX = vBox [ str "X   X"
              , str " X X "
              , str "  X  "
              , str " X X " 
              , str "X   X"]
blockO = vBox [ str "OOOOO"
              , str "O   O"
              , str "O   O"
              , str "O   O"
              , str "OOOOO"]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget