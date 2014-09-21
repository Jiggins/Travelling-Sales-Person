module ConvexHull where

import Data.List
import Data.Ord
import Text.Printf

type Point a = (a,a)

-- | Titty operator
-- Not used in this code any more,  I still like it though
(.:) = (.) . (.)

-- | Tuples == numbers, Nice!
instance (Num a, Num b) => Num (a,b) where
    (+) (x1,y1) (x2,y2) = (x1 + x2, y1+y2)
    (*) (x1,y1) (x2,y2) = (x1 * x2, y1*y2)
    abs (a,b) = (abs a, abs b)
    negate (a,b) = (negate a, negate b)
    signum (a,b) = (signum a, signum b)
    fromInteger = undefined

-- | Standard distance between two points
distance :: Floating a => Point a -> Point a -> a
distance (x1, y1) (x2, y2) = sqrt $ a**2 + b**2
    where a = x2 - x1
          b = y2 - y1

toPoint :: [a] -> (a,a)
toPoint (x:y:xs) = (x,y) 

-- * Imported from IO

readMultiNums :: (Num a, Read a) => IO [[a]]
readMultiNums = getContents >>= return . map (map read . words) . lines

-- | "Multiplicity"
--   - Lars 2014
cross :: Num a => Point a -> Point a -> a
cross  (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

-- | Gets a cross product between two points and an origin point,
-- if the answer is: >  0 then its a counter-clockwise turn
--                   == 0 then the three points are colinear
--                   <  0 then its a clockwise turn
--
-- This algorithm keeps turning clockwise until it has hit the starting value
turn :: Num a => Point a -> Point a -> Point a -> a
turn origin a b = (a - origin) `cross` (b - origin)

-- | Check if a turn is clockwise
clockwise :: (Num a, Ord a) => Point a -> Point a -> Point a -> Bool
clockwise origin a b = turn origin a b <= 0

-- | The PFM algorith (Pure fucking magic)
-- Will keep checking points until it finds the most right turn,  then moves
-- on to the next point.  stops when list is empty.
chain :: (Num a, Ord a) => [Point a] -> [Point a] -> [Point a]
chain (a:b:path) (x:xs) = if clockwise b a x
    -- Clockwise, remove most recent turn.
    then chain (b:path) (x:xs)
    -- Counter-clockwise, append to chain.
    else chain (x:a:b:path) (xs)

chain path (x:xs) = chain (x:path) xs
chain path [] = tail path

-- | More fucking magic
convexHull :: (Ord a, Num a) => [Point a] -> [Point a]
convexHull [] = []
convexHull [p] = [p]
convexHull points = lower ++ upper
  where
    sorted = sort points
    lower = chain [] sorted
    upper = chain [] (reverse sorted)

-- | Perimeter of a list of points.
perimeter :: Floating a => [(a, a)] -> a
perimeter (x:xs) = x `distance` (last xs) + 
    (sum . map (\(a,b) -> distance a b) $ pairs (x:xs))

-- | Groups a list into pairs for easy distancing.
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs ([x,y]) = [(x,y)]
pairs (x:y:xs) = (x,y) : pairs (y:xs)