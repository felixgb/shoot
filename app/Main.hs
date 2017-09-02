module Main where

import Window
import Light
import Entity
import Movement
import Render.Render
import Render.Uniforms

main :: IO ()
main = do
  window   <- initWindow
  moveRef  <- initMovementRefs
  setupWindow window moveRef
  uniforms <- initUniforms
  entities <- initEntities
  initDisplay window uniforms moveRef entities [centerLight]


