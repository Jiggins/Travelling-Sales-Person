module Haversine where 

	type Coords = (Double, Double)

	haversine :: Coords -> Coords -> Double
	haversine p1 p2 = (2 * radius *) . asin $ sqrt (a + b)
		where dlat  = lat  p2 - lat  p1
		      dlong = long p2 - long p1
		      a = (^2) . sin $ dlat / 2
		      b = cos (lat p1) *
		          cos (lat p2) *
		          sin (dlong / 2) * sin (dlong / 2)

	long :: (Floating a) => (a,a) -> a
	lat  = toRadians . fst
	long = toRadians . snd

	radius :: (Num a) => a
	radius = 6371 -- 6,371  km

	toRadians :: Floating a => a -> a
	toRadians = (*) (pi / 180)