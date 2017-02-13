multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z

multTwoNumsW9 = multThree 9
multW18 = multTwoNumsW9 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

inverse :: (Floating a) => a -> a
inverse = (1/)

isCapital :: Char -> Bool
isCapital = (`elem` ['A'..'Z'])

subtract4 = subtract 4

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

haha1 = applyTwice (++ " HAHA") "HEY"
haha1' = "HEY HAHA HAHA"

haha2 = applyTwice ("HAHA " ++) "HEY"
haha2' = "HAHA HAHA HEY"

mult1 = applyTwice (multThree 2 2) 9
mult1' = 144

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipTest1 = zipWith' (zipWith' (*)) [[1, 2, 3], [3, 5, 6], [2, 3, 4]] [[2, 3, 3], [3, 4, 5], [5, 4, 3]]
zipTest1' = [[2, 6, 9], [9, 20, 30], [10, 12, 12]]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where g x y = f y x

fliptest = zipWith' (flip' div) [2, 2..] [10, 8, 6, 4, 2]
fliptest' = [5, 4, 3, 2, 1]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs


maptest = map (+6) [1, 2, 3, 5, 6]
maptest' = [7, 8, 9, 11, 12]

maptest2 = map (++ "BOOOO") ["I'm a ghost. ", "Fear me! "]

duplicate3 = map (replicate 3)

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
	| p x 	= x : filter p xs
	| otherwise = filter p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort (filter' (<=x) xs)
	    biggerSorted = quicksort (filter' (>x) xs)
	in smallerSorted ++ [x] ++ biggerSorted
