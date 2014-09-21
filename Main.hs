module Main where

import Data.Array (Array, array, Ix)
import Data.List.Split (splitOn)
import System.IO
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Maybe
import Data.List
import Town
import TravellingSalesPerson
import ConvexHull

-- | Reads towns from tab seperated format to the Town type
-- e.g 1    Abbeyfeale  52.386  -9.294
readTown :: String -> Town
readTown = makeTown . splitOn "\t"
  where makeTown town = Town (townId town) (name town) (coords town) False
        townId = (read :: String -> Int) . (!! 0)
        name = (!! 1)
        coords a = apply (read :: String -> Double) ((a !! 2), (a !! 3))

--readTownsArray :: (Enum i, Num i, Ix i) => FilePath -> IO (Array i Town)
--readTownsArray fileName = openFile fileName ReadMode >>= hGetContents >>= 
--    return . array (1,80) . (zip [1..80]) . map readTown . lines


-- | reads from a file into a Vector of Towns
readTownsVector :: FilePath -> IO (Vector Town)
readTownsVector fileName = openFile fileName ReadMode >>= hGetContents 
    >>= return . V.fromList . map readTown . lines

-- | Worst possible way of converting the list of points in the convex hull to
-- a list of town ids.  by searching for each one
-- n^2 for life!
searchTowns :: Vector Town -> Point Double -> Maybe Town
searchTowns vector point = V.find (\x -> point == coords x) vector

main :: IO ()
main = do
    townList <- readTownsVector "Towns.csv" {->>= distanceMatrix-}
    print . intercalate "." . map (show . fromJust . fmap townId) . map (searchTowns townList) . convexHull . V.toList . fmap coords $ townList