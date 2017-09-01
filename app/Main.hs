module Main where

import Window
import Light
import Entity
import Movement
import Render.Render

main :: IO ()
main = do
  window   <- initWindow
  moveRef  <- initMovementRefs
  entities <- initEntities
  setupWindow window moveRef
  initDisplay window moveRef entities [centerLight]


