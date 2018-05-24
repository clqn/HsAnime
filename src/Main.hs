module Main where

import Graphics.Gloss as Gl
import Cell as C
import System.Environment as Env

main :: IO ()
main = do
  [fname] <- Env.getArgs
  gstring <- readFile fname
  let g = C.focus (C.fromString gstring) 10
  print g
  Gl.display
    (Gl.InWindow
      "Hello World" -- window title
      (800, 800)    -- window size
      (10, 10))     -- window position
    Gl.white        -- background color
    (picture g)   -- picture to display

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

