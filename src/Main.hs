module Main where

import Graphics.Gloss as Gl
import Cell as C


main :: IO ()
main = do
  let gld = [[False, True, False], [False, False, True], [True, True, True]]
  Gl.display
    (Gl.InWindow
      "Hello World" -- window title
      (800, 800)    -- window size
      (10, 10))     -- window position
    Gl.white        -- background color
    (picture gld)   -- picture to display

picture :: [[Bool]] -> Gl.Picture
picture bs = Gl.Pictures circles where
  bs' = map (zip [0,20..]) bs
  rows = zip [0,20..] bs'
  circles = concat [ (flip map)
                       r (\(x,b) -> if b
                                    then Gl.Translate x (-y) (Gl.Circle 10)
                                    else Gl.Blank)
                   | (y, r) <- rows
                   ]

