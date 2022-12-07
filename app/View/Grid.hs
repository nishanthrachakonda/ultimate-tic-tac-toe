module View.Grid (view) where

import Brick
import Brick.Widgets.Center
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
view s = [v]
      where v = ((view' s) <+> ((view_vp s) <=> (view_hp s) ))

view' :: PlayState -> Widget String

view' s = 
    joinBorders $ withBorderStyle unicode $
    borderWithLabel (changeColor (psTurn s) (str (header s))) $
  --  vLimitPercent 90 $
    hLimitPercent 60 $
    (mkBoard s board)
          where
            board = psBoard s

changeColor :: Value ->  Widget n -> Widget n
changeColor val = modifyDefAttr (`withBackColor` col)
            where col | val == X = magenta
                      | val == O = green

header :: PlayState -> String
header s = printf "Ultimate Tic-Tac-Toe Turn = %s" (show (psTurn s)) 


mkBoard :: PlayState -> Board -> Widget n
mkBoard s b = withBorderStyle unicode $
              vTile ([ mkgRow s row b | row <- [1..3]])

mkgRow :: PlayState -> Int -> Board -> Widget n
mkgRow s r b = hTile [( showState (getGridStatus ((r-1)*3+i) b)  (padAll 2 (mkGrid s ((r-1)*3+i) (getGrid ((r-1)*3+i) b)))) | i <- [1..3] ]

showState :: GridStatus -> Widget n -> Widget n
showState stat = modifyDefAttr (`withBackColor` col)
              where col  | stat == (Win X) = magenta
                         | stat == (Win O) = green 
                         | stat == (Draw)  = blue
                         | otherwise = black

getGridStatus :: Int -> Board -> GridStatus
getGridStatus i b = stat
              where gridL = Map.lookup i b
                    stat | isNothing gridL = Ongoing
                         | otherwise       = fst (fromJust gridL)


getGrid :: Int -> Board -> Grid
getGrid i b = grid
              where gridL = Map.lookup i b
                    grid | isNothing gridL = Map.empty
                         | otherwise       = snd (fromJust gridL)   

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
mkCell' cp g = center (hLimit 1 $ vLimit 3 $ (mkValue v))
  where 
    v      = Map.lookup (snd cp) g

mkValue :: Maybe Value -> Widget n
mkValue (Just X) = blockX
mkValue (Just O) = blockO
mkValue Nothing  = blockB     

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


view_vp :: PlayState -> Widget String

view_vp s = 
    withBorderStyle unicode $
    borderWithLabel (changeColor (psTurn s) (str (header_vp s))) $
    vLimitPercent 90 $
     (mkVPane s whoseTurn currPosition)
          where
            whoseTurn = psTurn s
            currPosition = psCur s
 
mkVPane :: PlayState -> Value -> CurPos -> Widget n
mkVPane s whoseTurn currPosition = withBorderStyle unicode $
              (padAll 5 (vTile ([playerName, turn, gameStatus, gameResult])))
              where 
                playerName = (str "Player :") <+> (str "X or O") 
                turn = (str "Current Turn :") <+> (str "whoseTurn")
                gameStatus = (str "Game Status :") <+> (str "Ongoing")
                gameResult | "Ongoing" == "Ongoing" = (hCenter (vBox ([str "X won"])))
                           | "Ongoing" == "Win O" = (str "O won")
                           | "Ongoing" == "Draw"  = (str "Draw")
                           | otherwise = emptyWidget

--gameResult :: GridStatus -> Widget n
--gameResult stat | stat == Win X    = (printResultWidget "X won")
--                | stat == Win O    = (printResultWidget "O won")
--                | stat == "Draw"   = (printResultWidget "Draw")
--                | otherwise = emptyWidget

-- printResultWidget :: String -> Widget n
-- printResultWidget res = (hCenter (vBox ([str res])))

 
header_vp :: PlayState -> String
header_vp s = "Game Statistics"


view_hp :: PlayState -> Widget String
view_hp s = 
    withBorderStyle unicode $
    borderWithLabel (changeColor (psTurn s) (str (header_hp s))) $
    center (displayServerMessage (psMessage s))  -- psMessage s

displayServerMessage :: String -> Widget n
displayServerMessage "" = emptyWidget
displayServerMessage m  = (strWrap m)

header_hp :: PlayState -> String
header_hp s = "Network Messages"