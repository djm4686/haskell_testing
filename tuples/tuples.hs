getFirstMember :: (t, t1) -> t
getFirstMember x = fst x

getSecondMember :: (t, t1) -> t1
getSecondMember x = snd x

listOfPairs :: [t] -> [t1] -> [(t, t1)]
listOfPairs x y = zip x y

rightTrianglesWPerimeter24 :: [(Int, Int, Int)]

rightTrianglesWPerimeter24 = [ (a,b,c) | c <- [1..10],
					 b <- [1..c],
					 a <-[1..b],
					 a^2 + b^2 == c^2,
					 a+b+c == 24]


