module Main where

import Window
import Light
import Movement
import Entity.Entity
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
