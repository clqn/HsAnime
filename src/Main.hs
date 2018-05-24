module Main where

import Graphics.Gloss as Gl

main
  = Gl.display 
      (Gl.InWindow
         "Hello World" -- window title
         (400, 150)    -- window size
         (10, 10))     -- window position
      Gl.white         -- background color
      picture          -- picture to display

picture
  = Gl.Translate (-170) (-20) -- shift the text to the middle of the window
  $ Gl.Scale 0.5 0.5          -- display it half the original size
  $ Gl.Text "Hello World"     -- text to display
