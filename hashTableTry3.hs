import System.IO
import Data.Char

type Size = Integer
type NowElements = Integer
data HashTable k v = HashTable [[(k,v)]] Size NowElements

instance (Show k, Show v) => Show (HashTable k v) where
     show (HashTable arr size nowelements) = "/*Hash table:" ++ (foldl (\text key -> text ++ "\n" ++ show(key)) "" arr) ++ "\n*/"

--хешом является сумма кодов символов строки
hash :: (Show k) => k -> Size -> Integer
hash key size = (foldl (\x sym -> x + (toInteger.ord) sym) 0 (show key)) `mod` size

defaultHashTable :: Integer -> HashTable k v
--replicate ожидает тип int
defaultHashTable size = HashTable (replicate (fromIntegral size) []) size 0

--конструирует таблицу из списка пар
fromList ::(Show k, Eq k) => [(k,v)] -> HashTable k v
fromList arr = (foldl (\x (k,v) -> insert x k v) (defaultHashTable 2) arr) 

insert :: (Show k, Eq k) => HashTable k v -> k -> v -> HashTable k v
insert (HashTable arr size nowElements) key value = 
             if (nowElements * 2 < size)  then HashTable (a ++ ([(filter (\(k,v) -> k /= key) (arr !! idHash)) ++ [(key, value)]]) ++ b) size (nowElements + 1)
                                                     else insert (rehash (HashTable arr size nowElements)) key value where 
                                                             idHash = fromIntegral (hash (show key) size)
                                                             (a,_:b) = splitAt idHash arr
                                                             --a = fst time'
                                                             --b = drop 1 (snd time')
                                                                                 


rehash :: (Show k, Eq k) => HashTable k v -> HashTable k v
rehash (HashTable elements size nowElements) = if (nowElements * 2 >= size) then (foldl (\arr (k,v) -> (insert arr k v)) newTable (concat elements))
                                                                                 else HashTable elements size nowElements where
                                                                                                         newTable = defaultHashTable (size * 2)

main = do
     handle <- openFile "input.txt" ReadMode
     --lines - структурирует поток ввода из aab\nbaba\nlox к виду ["aab", "baba", "lox"]
     content <- map (\line -> ((words line)!! 0, (words line)!! 1)) <$> lines <$> hGetContents handle
     writeFile "output.txt" (show (fromList content))
     hClose handle

clear :: HashTable k v -> HashTable k v
clear (HashTable _ size _) = defaultHashTable size

erase :: (Show k, Eq k) => HashTable k v -> k -> HashTable k v
erase (HashTable arr size nowElements) key = HashTable (a ++ ([filter (\(k,v) -> k /= key) (arr !! idHash)]) ++ b) size (nowElements - 1) where
                                         idHash = fromIntegral (hash (show key) size)
                                         time' = splitAt idHash arr
                                         a = fst time'
                                         b = drop 1 (snd time')

contains :: (Show k, Eq k) => HashTable k v -> k -> Bool
--fst берет первый элемент из пары, вида (a,b) -> a
contains (HashTable arr size nowElements) key = elem key [k|(k,v) <- arr !! idHash] where
                                                                     idHash = fromIntegral (hash (show key) size)

at :: (Show k, Eq k) => HashTable k v -> k -> Maybe v
at (HashTable arr size nowElements) key = lookup key (arr !! idHash)  where 
                                                     idHash = fromIntegral (hash (show key) size) 
                                                     --snd берет второй элемент из пары, вида (a,b) -> b
													 --lookup ищет в кортеже, где есть пары, вида (a,b), елемент key == a и выводит (Maybe b)

--проверяет, есть ли в нем сейчас элементы (нет = true, да = false)
empty :: (Show k, Eq k) => HashTable k v -> Bool
empty (HashTable _ _ nowElements) = nowElements == 0

size ::(Show k, Eq k) => HashTable k v -> Integer
size (HashTable _ _ nowElements) = nowElements

b = defaultHashTable 2
c = insert b "Nikita" 2001
d = rehash c
e = insert d "Andrew" 1924
f = rehash e
g = insert f "Sergey" 1982
h = rehash g
k = insert h "Senya" 990
l = rehash k
