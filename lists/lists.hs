lostNumbers = [4, 8, 15, 16, 23, 42]
nonLostNumbers = [1, 2, 3, 5, 6, 7]
aString = "A string"
aChar = 'A'
anotherChar = 'C'
anotherString = " and another string"
addLists x y = x ++ y
addAtFront x y = x : y
getElement list index = list !! index

test1 = [3, 2, 1]
test2 = [3, 2, 1, 1]
test3 = [3, 100]
test4 = [2]

test x y = x > y

frontOfList x = head x
popZeroIndex x = tail x
endOfList x = last x
popLastIndex x = init x
listLength x = length x
checkIfEmpty x = null x
getReverse x = reverse x
getFirstX x list = take x list
removeFirstX x list = drop x list
getMax x = maximum x
getMin x = minimum x
getSumOfList x = sum x
getProductOfList x = product x
isXElement x list = x `elem` list

range1 = [1..20]
range2 = ['a'..'z']
range3 = ['K'..'Z']
rangeStep1 = [1,3..20]
rangeStep2 = ['a', 'c' .. 'z']
rangeStep3 = ['d', 'j'..'w']
rangeStep4 = [20,19..1]

lazyEval = take 10 [10, 20 .. 20*1000]
cycleList cutoff list = take cutoff (cycle list)
repeatSingle cutoff single = take cutoff (repeat single)
createsList = repeatSingle 10 "a"
createsString = repeatSingle 10 'a'
replicateSingle num single = replicate num single

listComprehension = [x*2 | x <- [1..10]]  
listComprehensionWPredicate = [x*2 | x <- [1..10], x >= 6]
listComp2 = [x | x <- [50..100], x `mod` 7 == 3]
isPrime :: Int -> Bool
isPrime x = null [y | y <- [2..(x `div` 2)], x `mod` y == 0]

genPrimes n = take n [x | x <- [1,3..], isPrime x]
genPrimesList n list = take n [x | x <- list, isPrime x]

boomBangs xs = [if x < 10 then "tick..." else "BANG!" | x <- xs, odd x]

listMult l1 l2 = [x*y | x <- l1, y <- l2]

length' xs = sum [1 | _ <- xs]

removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

nestedComprehension xxs = [[ x | x <- xs, even x] 
			       | xs <- xxs]


