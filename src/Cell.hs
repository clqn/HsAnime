module Cell
  ( Cell
  , newGrid
  , fromList
  , north
  , west
  , east
  , south
  )
  where

import qualified Control.Comonad as Co
{- | A Cell represents a point of focus on an infinite grid. It has a state value,
which can contain some arbitrary piece of information.

                     north :: (Cell a)

                           ^^
   west :: (Cell a)  <<  i :: a   >>   east :: (Cell a)
                           vv

                     south :: (Cell a)

-}
data Cell a = Cell { info :: a
                   , north :: Cell a
                   , west :: Cell a
                   , east :: Cell a
                   , south :: Cell a
                   }

{- | cmap is in fact the extend function for Cell (extend is the comonad
equivalent of bind)

cmap takes a (Cell a -> b) function (i.e. one that takes a cells and returns a
value based on that cell) and creates a new grid of cells where each new cell's
information record is the result of that function run on the old cell.

it uses a "stem-leaf" algorithm to propagate the function throughout a
grid of cells. The diagram below gives a general idea of the shape of
this algorithm.

               stemN
                 ^
      stemW <--- | ---> stemE
      stemW <--- | ---> stemE
      stemW <--- c ---> stemE
      stemW <--- | ---> stemE
      stemW <--- | ---> stemE
                 V
               stemS

when you see x' in the code for cmap (and many other functions in this module), it represents the updated version of a cell x.

cmap (and many other functions in this module) makes use of the haskell idiom of
"knot-tying". for a full explanation of this, see
https://wiki.haskell.org/Tying_the_Knot, but the short of it is that since
haskell is lazy (stuff's only evaluated when it's necessary), we can do stuff
like passing c' to other functions even though it appears to not be fully
defined. this is crucial for cyclic data structures (like Cell!). it's a bit
hard to wrap your head around at first, but it's very much worth understanding!

a very simple example of a tied knot is `let x = 1:x`, which would be the infinite list `1:1:1:1:1 [...]`
-}

cmap :: (Cell a -> b) -> Cell a -> Cell b
cmap f c@(Cell i n s e w) = c' where
  c' = Cell (f c) (stemN c' f n) (leafW c' f w) (leafE c' f e) (stemS c' f s)

{- | stemN is a helper function for cmap which takes an updated south cell (s')
and a (Cell a -> b) function and does the following things:

  • applies the function to the current cell, making a new cell containing the
    value it got (c')
  • updates all the cells to the east and west of the current cell via the leaf
    functions, obtaining the updated cells e' and w'.
  • passes the function on to the cell to the north, obtaning the updated north
    cell n'.

-}
stemN :: Cell b -> (Cell a -> b) -> Cell a -> Cell b
stemN s' f c@(Cell i n w e s) = c' where
  c' = Cell (f c) n' w' e' s'
  (n', w', e') = (stemN c' f n, leafW c' f w, leafE c' f e)

-- | see stemN. this function works in almost the same way, but moves south.
stemS :: Cell b -> (Cell a -> b) -> Cell a -> Cell b
stemS n' f c@(Cell i n w e s) = c' where
  c' = Cell (f c) n' w' e' s'
  (w', e', s') = (leafW c' f w, leafE c' f e, stemS c' f s)

{- | leafW is a helper function for cmap which takes an updated east cell (e'),
and a (Cell a -> b) function, and does the following things:

  • applies the function to the current cell, making a new cell (c') out of the result
  • gets the updated cells to the north and south of it (n' and s') from the cells to the northwest and southwest of e'
  • updates all the cells to its west using leafW, obtaining w'
-}
leafW :: Cell b -> (Cell a -> b) -> Cell a -> Cell b
leafW e' f c@(Cell i n w e s) = c' where
  c' = Cell (f c) n' w' e' s'
  (n', w', s') = (west$north e', leafW c' f w, west$south e')

-- | see leafW. leafE works in almost the same way, but moves south
leafE :: Cell b -> (Cell a -> b) -> Cell a -> Cell b
leafE w' f c@(Cell i n w e s) = c' where
  c' = Cell (f c) n' w' e' s'
  (n', e', s') = (east$north w', leafE c' f e, east$south w')

{- | iswap allows you to swap the piece of information in a cell out with a new one. it must also update all other cells in the grid to point back to the newly
updated cell.
-}
iswap :: a -> Cell a -> Cell a
iswap v (Cell i n w e s) = c' where 
  c' = Cell v n' w' e' s'
  (n', s') = (stemN c' info n, stemS c' info s)
  (w', e') = (leafW c' info w, leafE c' info e)


instance Functor Cell where
  fmap f = cmap (\c -> f $ info c)

instance Co.Comonad Cell where
  extract = info -- comonadic "return"
  extend = cmap -- comonadic "bind"
  duplicate = cmap id -- comonadic "join" 

{- | newGrid creates a fresh grid, initialized to a given value (i). it uses an
algorithm similar to cmap's. i've gone for terser variable names here for
brevity's sake, but n, s, w, and e play similar roles as stemN, stemS, leafW,
and leafE, so if you understood cmap's code, this one should not be too foreign
to you either.
-}
newGrid :: a -> Cell a
newGrid i = c where
    c = Cell i (n c) (w c) (e c) (s c)
    n s' = let k = Cell i (n k) (w k) (e k) s' in k
    s n' = let k = Cell i n' (w k) (e k) (s k) in k
    w e' = let k = Cell i (west$north e') (w k) e' (west$south e') in k
    e w' = let k = Cell i (east$north w') w' (e k) (east$south w') in k

{--

--}

fromList :: [[a]] -> a -> (Cell a)
fromList ls a = col g ls where
  g = newGrid a
  col c [] = c
  col c (l:ls) = north $ col (south $ row c l) ls
  row c [] = c
  row c (v:vs) = iswap v (west $ row (east c) vs)

fromStringBy :: (Char -> a) -> a -> String -> Cell a
fromStringBy f d s = fromList (map (map f) $ lines s) d

fromString :: String -> Cell Bool
fromString = fromStringBy f False where
  f '#' = True
  f _   = False

