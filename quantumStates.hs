type Reeall = Integer
type Imagine = Integer
data ComplexNumber a = ComplexNumber a a

instance (Num a, Show a, Ord a, Eq a, Integral a) => Show (ComplexNumber a) where
     show(ComplexNumber 0 0) = show 0
     show(ComplexNumber 0 1) = "i"
     show(ComplexNumber 0 (-1)) = "-i"
     show(ComplexNumber a 0) = show a
     show(ComplexNumber a 1) = show a ++ " + " ++ "i"
     show(ComplexNumber a (-1)) = show a ++ " - " ++ "i"
     show(ComplexNumber 0 b) = show b ++ "i"
     show(ComplexNumber a b) | b > 0 = show a ++ " + " ++ show b ++ "i"
                             | otherwise = show a ++ " - " ++ show (abs b) ++ "i"


instance (Num a, Show a, Ord a, Eq a, Integral a) => Eq (ComplexNumber a) where
     (ComplexNumber a1 b1) == (ComplexNumber a2 b2) = z1 == z2 where
         z1 = sqrt (fromIntegral(a1^2) + fromIntegral(b1^2))
         z2 = sqrt (fromIntegral(a2^2) + fromIntegral(b2^2))

instance (Num a, Show a, Ord a, Eq a, Integral a) => Ord (ComplexNumber a) where
     (ComplexNumber a1 b1) `compare` (ComplexNumber a2 b2) = z1 `compare` z2 where
         z1 = sqrt (fromIntegral(a1^2) + fromIntegral(b1^2))
         z2 = sqrt (fromIntegral(a2^2) + fromIntegral(b2^2))

instance (Num a, Show a, Ord a, Eq a, Integral a) => Num (ComplexNumber a) where
     negate (ComplexNumber a b) = ComplexNumber (-a)(-b)
     (+) (ComplexNumber a1 b1) (ComplexNumber a2 b2) = (ComplexNumber (a1+a2) (b1+b2))
     (*) (ComplexNumber a1 b1) (ComplexNumber a2 b2) = (ComplexNumber (a1*a2-b1*b2) (a1*b2+b1*a2))
     abs (ComplexNumber a b) = ComplexNumber (abs a) (abs b)
     signum (ComplexNumber a b) | a > 0 = 1
                                | a == 0 = 0
                                | otherwise = -1
     fromInteger int = ComplexNumber 0 0

type StrMarker = String
data QuantumState a = ToQuantumState a StrMarker
type Qubit a = [QuantumState a]

instance  (Show a, Ord a, Eq a) => Show (QuantumState a) where
     show (ToQuantumState a b) = "QuantumState " ++ show a ++ " " ++ show b

--не знаю как заставить это работать, но fmap работает и без этого
instance Functor (QuantumState) where
     fmap f (ToQuantumState complex label) = ToQuantumState (f complex) label

toList :: Qubit (ComplexNumber a) -> [ComplexNumber a]
toList x = [complex | (ToQuantumState complex _) <- x]

toLabelList :: Qubit (ComplexNumber a) -> [StrMarker]
toLabelList x = [marker | (ToQuantumState _ marker) <- x]

fromList :: [ComplexNumber a] -> [StrMarker] -> Qubit (ComplexNumber a)
fromList x y = [(ToQuantumState complex txt) | complex <- x, txt <- y]

toPairList :: Qubit (ComplexNumber a) -> [(ComplexNumber a, StrMarker)]
toPairList x = [(complex, marker) | (ToQuantumState complex marker) <- x]

fromPairList :: [(ComplexNumber a, StrMarker)] -> Qubit (ComplexNumber a)
fromPairList x = [(ToQuantumState complex marker) | (complex, marker) <- x]

scalarProduct :: (Num a, Show a, Ord a, Eq a, Integral a) => Qubit (ComplexNumber a) -> Qubit (ComplexNumber a) -> ComplexNumber a
scalarProduct x y = foldr (\a b -> a + b) 0 [(complex1 * complex2)| (ToQuantumState complex1 _) <- x, (ToQuantumState complex2 _) <- y]

entagle :: (Num a, Show a, Ord a, Eq a, Integral a) => Qubit (ComplexNumber a) -> Qubit (ComplexNumber a) -> Qubit (ComplexNumber a)
entagle x y = [(ToQuantumState (complex1 * complex2) (marker1 ++ marker2)) | (ToQuantumState complex1 marker1) <- x, (ToQuantumState complex2 marker2) <- y]

--Examples
x1 = (ComplexNumber 5 1)
x2 = (ComplexNumber (-5) (-1))
x3 = (ComplexNumber 100 100)
y1 = (ToQuantumState x1 "Fish")
y2 = (ToQuantumState x3 "451 of Farengheit")
z1 = [y1, y2]
pl1 = [(x1,"Fish"), (x2, "451 of Farengheit")]
