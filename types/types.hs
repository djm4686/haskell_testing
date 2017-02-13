type1 :: Char
type1 = 'a'

type2 :: [Char]
type2 = "a"

type3 :: Int
type3 = 1

type4 :: Bool
type4 = True

type5 :: (Bool, Int)
type5 = (True, 1)

representAsString :: Show a => a -> [Char]
representAsString x = show x 

reverseInt :: Int -> Int
reverseInt x = getInt (read (reverse (representAsString x)))

readFloat :: [Char] -> Float
readFloat x = read x :: Float


getFloat x = x :: Float
getDouble x = x :: Double
getInt x = x :: Int
getInteger x = x :: Integer  
