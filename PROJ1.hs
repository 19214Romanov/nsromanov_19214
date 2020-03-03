--каждое обращение к модулю должно будет содержать префикс(имя модуля)
import qualified Data.Digest.Pure.MD5 as MD5 --библа, которая генерит hash md-5
import qualified Data.ByteString.Lazy.UTF8 as BLU --библа, которая конвертит String в ByteString

alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

--взять пароль по номеру
findById :: Integer -> String
findById idi = createStr idi 1 ""
               where createStr :: Integer -> Integer -> String -> String
                     createStr idi koef mem | idi >=  0 = createStr (idi - idi `mod` (62^koef) - 1) (koef + 1) ((alphabet !! fromIntegral (idi `mod` (62^koef) `div` (62^(koef - 1)))) : mem)
                                            | otherwise = mem

--перебор всех паролей, пока не найдет нужный
bruteForce :: String -> Integer -> String
bruteForce hash number | hash == (show (MD5.md5 (BLU.fromString (findById number)))) = findById number
                       | number > 931151402 = "Something went wrong =("
                       | otherwise = bruteForce hash (number+1)

main = do
  putStrLn "\nДобро пожаловать, я помогу вам узнать пароль по хешу MD-5.\nВведите ваш хеш:"
  hash <- getLine
  putStrLn "Расшифрован пароль: "
  writeFile "output.txt" (bruteForce hash 0)
