module Main where

import Window
import Light
import Entity
import Movement
import Graphics.Uniforms
import Graphics.Render

main :: IO ()
main = do
  window <- initWindow
  moveRef <- initMovementRefs
  setupWindow window moveRef
  entities <- initEntities
  uniforms <- initUniforms
  initDisplay window uniforms moveRef entities [centerLight] terminate


