lucky :: (Integral a) => a -> [Char]
lucky 7 = "Good jorb"
lucky x = "you unlucky son"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n-1)

charMap :: Char -> String
charMap 'a' = "Albert"
charMap 'b' = "Broseph"
charMap 'c' = "Cecil"
charMap x = "Not on the list br'ah"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

firstTriple :: (a, b, c) -> a
firstTriple (x, _, _) = x

secondTriple :: (a, b, c) -> b
secondTriple (_, y, _) = y

thirdTriple :: (a, b, c) -> c
thirdTriple (_, _, z) = z

patternTest = [ (1, 2), (3, 4), (5, 6)]
patternMatchComp xs = [ a + b | (a, b) <- xs]

patternMatchHead :: [a] -> a
patternMatchHead [] = error "Not empties"
patternMatchHead (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 
 
firstLetterPattern :: String -> String
firstLetterPattern "" = error "No empty strings allowed"
firstLetterPattern all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTellGuard :: (RealFloat a) => a -> String
bmiTellGuard bmi
	| bmi <= 18.5 = "You're underweight"
	| bmi <= 25.0 = "Youre normal"
	| bmi <= 30.0 = "You're overweight"
	| bmi <= 35.0 = "You're obese"
	| otherwise = "You're literally a planet"

calcBmi :: (RealFloat a) => a -> a -> a
calcBmi weight height = weight / height ^ 2

calcAndTellBmi :: (RealFloat a) => a -> a -> String
calcAndTellBmi weight height = bmiTellGuard (calcBmi weight height)

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b = a
	| otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
	| a > b = GT
	| a == b = EQ
	| otherwise  = LT


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= 15.0 = "go away"
	| bmi <= 25.0 = "you suck"
	| bmi <= 30.0 = "i hate you"
	| bmi <= 35.0 = "you fucking ham"
	| otherwise = "planet"
	where bmi = weight / height ^ 2


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort [a | a <- xs, a <= x]
	    biggerSorted = quicksort [a | a <- xs, a > x]
	in smallerSorted ++ [x] ++ biggerSorted
