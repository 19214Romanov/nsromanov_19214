get :: [a] -> Integer -> a
get [] _ = error "Index too large"
get (x:xs) y  | y == 0 = x
              | otherwise = get xs (y-1)

head' :: [a] -> a
head' [] = error "Empty list"
head' (x:xs) = x;

last' :: [a] -> a
last' [] = error "Empty list"
last' [x] = x
last' (x:xs) = last' xs

tail' :: [a] -> [a]
tail' [] = error "Empty list"
tail' (x:xs) = xs

init' :: [a] -> [a]
init' [] = error "Empty list"
init' [x] = []
init' (x:xs) = x : init' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = append (reverse' xs) x

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

append :: [a] -> a -> [a]
append [] x = [x]
append (y:xs) x = y : append xs x

concat' :: [a] -> [a] -> [a]
concat' x [] = x
concat' x (s:sys) = concat' (append x s) sys

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) | n == 0 = x:xs
               | otherwise = drop' (n-1) xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs) | n /= 0 = x : take' (n-1) xs
               | otherwise = []

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | (length' xs) >= n = (take' n xs, drop' n xs)
              | otherwise = (xs,[])

null' :: [a] -> Bool
null' [] = True
null' (_:_) = False

elem' :: (Eq a) => [a] -> a -> Bool
elem' [] _ = False
elem' (y:xs) x | y == x = True
               | otherwise = elem' xs x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' test (x:xs) | test x == True = x:filter' test xs
                    | otherwise = filter' test xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
