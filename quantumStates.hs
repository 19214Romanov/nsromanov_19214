type Reeall = Integer
type Imagine = Integer
data ComplexNumber = ComplexNumber Reeall Imagine

instance Show (ComplexNumber) where
     show(ComplexNumber 0 0) = "0"
     show(ComplexNumber 0 1) = "i"
     show(ComplexNumber 0 (-1)) = "-i"
     show(ComplexNumber a 0) = show a
     show(ComplexNumber a 1) = show a ++ " + " ++ "i"
     show(ComplexNumber a (-1)) = show a ++ " - " ++ "i"
     show(ComplexNumber 0 b) = show b ++ "i"
     show(ComplexNumber a b) | b > 0 = show a ++ " + " ++ show b ++ "i"
                             | b < 0 = show a ++ " - " ++ show (abs b) ++ "i"

instance Eq (ComplexNumber) where
     (ComplexNumber a1 b1) == (ComplexNumber a2 b2) = z1 == z2 where
         z1 = sqrt (fromIntegral(a1^2) + fromIntegral(b1^2))
         z2 = sqrt (fromIntegral(a2^2) + fromIntegral(b2^2))

instance Ord (ComplexNumber) where
     (ComplexNumber a1 b1) `compare` (ComplexNumber a2 b2) = z1 `compare` z2 where
         z1 = sqrt (fromIntegral(a1^2) + fromIntegral(b1^2))
         z2 = sqrt (fromIntegral(a2^2) + fromIntegral(b2^2))

type StrMarker = String
data QuantumState = QuantumState ComplexNumber StrMarker
type Qubit = [QuantumState]

instance Show (QuantumState) where
     show (QuantumState a b) = "QuantumState " ++ show a ++ " " ++ show b

toList :: Qubit -> [ComplexNumber]
toList [] = []
toList ((QuantumState x _) : a) = x : toList a

toLabelList :: Qubit -> [StrMarker]
toLabelList [] = []
toLabelList ((QuantumState _ t) : a) = t : toLabelList a

fromList :: [ComplexNumber] -> [StrMarker] -> Qubit
fromList [] _ = []
fromList (x:xs) (t:ts) = (QuantumState x t) : fromList xs ts

toPairList :: Qubit -> [(ComplexNumber, StrMarker)]
toPairList [] = []
toPairList ((QuantumState cn sm):xs) = (cn, sm) : toPairList xs

fromPairList :: [(ComplexNumber, StrMarker)] -> Qubit
fromPairList [] = []
fromPairList (((a),(b)):xs) = (QuantumState a b) : fromPairList xs

--не ясно, что нужно скалярно умножить
--ведь мы имеем два списка, со множеством различных векторов
--если умножать больше двух векторов, то получится смешанное произведение (а это не факт, что получится именно скаляр)
--ЗАДАЧА НЕ ЯСНА
--scalarProduct :: Qubit -> Qubit -> Float
--scalarProduct (((a1),(_)):xs1) (((a2),(_)):xs2) = 

--entagle = осложнить (я понял, что нужно два Кубита слепить в один)
entagle :: Qubit -> Qubit -> Qubit
entagle [] [] = []
entagle [] (y:ys) = y : entagle [] ys
entagle (x:xs) (y:ys) = x : entagle xs (y:ys)

--Examples
x1 = (ComplexNumber 5 1)
x2 = (ComplexNumber (-5) (-1))
x3 = (ComplexNumber 100 100)
y1 = (QuantumState x1 "Fish")
y2 = (QuantumState x2 "451 of Farengheit")
z1 = [y1, y2]
pl1 = [(x1,"Fish"), (x2, "451 of Farengheit")]