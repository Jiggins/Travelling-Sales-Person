module Town where 
    import Data.Array
    import Data.List
    import Data.Ord
    import Haversine

    type TownId = Int


    data Town = Town { townId :: Int
                      ,name  :: String
                      ,coords :: (Double, Double)
                      ,visited :: Bool
                     } 

    instance Show Town where
        show town = intercalate " " [show (townId town)
                                    ,name town
                                    ,show (coords town)]

    {- Path type, implemented for storing a best path and nice printing -}
    data Path = Path {path :: [Int], distance :: Double} deriving (Eq)

    instance Ord Path where
        compare = comparing distance

    instance Show Path where
        show p = concat ["Path:\t\t", (intercalate "." . map show . path) p, 
                         "\nDistance:\t", show (distance p)]

    getTown :: TownId -> Array Int Town -> Town
    getTown town = (! town)

    getCoords :: Ix i => i -> Array i Town -> Coords
    getCoords town = coords . (! town)

    visit :: Town -> Town
    visit town = Town (townId town) (name town) (coords town) True

    wasVisited :: Town -> Bool
    wasVisited = visited