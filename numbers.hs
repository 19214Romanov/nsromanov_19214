--STARTVERSION
toDecimal :: Integer -> String -> String
toDecimal 1 snumber = show(length snumber - 1)
toDecimal base snumber = schet base snumber 0
                             where
                             schet _ [] sum = show sum
                             schet base (s:snumber) sum = schet base snumber (sum + toInteger(findId s st 0) * base^(length snumber))
                             st = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             findId ss (s:st) num | ss == s = num
                                                  | otherwise = findId ss st num+1

fromDecimal :: Integer -> String -> String
fromDecimal 1 snumber = schetOne (read snumber :: Integer) []
                         where
                         schetOne :: Integer -> String -> String
                         schetOne (-1) sum = sum
                         schetOne num sum = schetOne(num - 1) ('1':sum)
fromDecimal toBase snumber = schet toBase snumber ""
                             where
                             schet :: Integer -> String -> String -> String
                             schet _ ['0'] st = st
                             schet toBase snumber st = schet (toBase) (show((read snumber :: Integer) `div` toBase)) ([stt !! ((read snumber :: Int) `mod` fromIntegral toBase)] ++ st)
                             stt = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

convertFromTo :: Integer -> Integer -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
