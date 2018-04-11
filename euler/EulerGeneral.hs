module EulerGeneral
  ( strides
  , omnidirectional_strides) where

import Data.List

-- Deals with historical accident of lists requiring Int indexes
list !. index = list `genericIndex` index

-- Iterates through a list in "strides" of length n. Doesn't split the list into
-- length n groups, rather each stride overlaps with the previous.
-- Example: strides 3 [1, 2, 3, 4, 5] = [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
-- If there are not enough elements in the list, returns nothing.
strides :: Int -> [b] -> [[b]]
strides n bs
  | length bs < n = []
  | otherwise = (take n bs):(strides n $ tail bs)

-- Given a 2D array, gets strides of length n in all directions (including diagonal)
omnidirectional_strides :: (Integral a) => a -> [[b]] -> [[b]]
omnidirectional_strides n grid
  | k < n = error "Grid too small"
  | k == n = diagonal:horizontal_vertical 
  | otherwise = diagonal:(horizontal_vertical ++ top_diagonals ++
                      omnidirectional_strides n nested_grid)
  where k = genericLength grid  -- Number of rows
        diagonal = [grid !. i !. i | i <- [0..(n-1)]] 
        horizontal_vertical = concatMap (map (genericTake n)) [grid, transpose grid]
        top_diagonals =
          let flipped_pair i j = (grid !. (i + j) !. j, grid !. j !. (i + j))
              (r, c) = unzip [unzip [flipped_pair i j | j <- [0..(n-1)]] |
                               i <- [1..(k-n)]]
          in r ++ c
        nested_grid = map tail $ tail grid
