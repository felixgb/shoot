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
  clickRef <- initClickRef
  setupWindow window moveRef clickRef
  uniforms <- initUniforms
  entities <- initEntities
  initDisplay window uniforms moveRef clickRef entities [centerLight]
