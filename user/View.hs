module View where

import Brick

import qualified View.Grid as Grid
import Model (PlayState)

view :: PlayState -> [Widget String]
view = Grid.view
