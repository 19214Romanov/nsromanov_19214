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
reverse' xs = reverse'' xs []
         where
         reverse'' :: [a] -> [a] -> [a]
         reverse'' [] xs = xs
         reverse'' (x:xs) ys = reverse'' xs (x : ys)

length' :: [a] -> Integer 
length' xs  = length'' xs 0
         where
         length'' :: [a] -> Integer -> Integer
         length'' [] n  = n
         length'' (x:xs) n = length'' xs (n + 1)

append :: [a] -> a -> [a]
append [] x = [x]
append (y:xs) x = y : append xs x

concat' :: [a] -> [a] -> [a]
concat' x [] = x
concat' x (s:sys) = concat' (append x s) sys

drop' :: Integer -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) | n == 0 = x:xs
               | otherwise = drop' (n-1) xs

take' :: Integer -> [a] -> [a]
take' _ [] = []
take' n (x:xs) | n /= 0 = x : take' (n-1) xs
               | otherwise = []

splitAt' :: Integer -> [a] -> ([a], [a])
splitAt' n xs = (take' n xs, drop' n xs)

null' :: [a] -> Bool
null' [] = True
null' x = False

elem' :: (Eq a) => [a] -> a -> Bool
elem' [] _ = False
elem' (y:xs) x | y == x = True
               | otherwise = elem' xs x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' test (x:xs) | test x = x:filter' test xs
                    | otherwise = filter' test xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
