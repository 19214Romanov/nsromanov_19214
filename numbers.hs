toDecimal :: Integer -> String -> String
toDecimal 1 snumber | isTrue snumber && snumber /= "" = show(length snumber - 1)
                    | snumber == "" = ['0']
                    | otherwise = error "There is no such number in the number system"
                         where
                         isTrue :: String -> Bool
                         isTrue [] = True
                         isTrue (s:snumber) | s == '1' = isTrue snumber
                                            | otherwise = False
toDecimal base snumber = if (isTrue base snumber) then schet base snumber 0 else error "There is no such number in the number system"
                             where
                             schet _ [] sum = show sum
                             schet base (s:snumber) sum = schet base snumber (sum + toInteger(findId s st 0) * base^(length snumber))
                             st = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             findId ss (s:st) num | ss == s = num
                                                  | otherwise = findId ss st num+1
                             isTrue :: Integer -> String -> Bool
                             isTrue _ [] = True
                             isTrue base (s:snumber) | base <= 0 || base > 62 = error "Invalid number system" 
                                                     | findId s st 0 >= base || findId s st 0 < 0 = False
                                                     | otherwise = isTrue base snumber

fromDecimal :: Integer -> String -> String
fromDecimal 1 snumber = if isTrue snumber then schetOne (read snumber :: Integer) [] else error "Invalid number"
                         where
                         schetOne :: Integer -> String -> String
                         schetOne (-1) sum = sum
                         schetOne num sum = schetOne(num - 1) ('1':sum)
                         isTrue snumber | (read snumber :: Integer) < 0 = False
                                        | otherwise = True
fromDecimal toBase snumber = if isTrue toBase snumber then schet toBase snumber "" else error "Invalid number"
                             where
                             schet :: Integer -> String -> String -> String
                             schet _ ['0'] st = if st == [] then ['0'] else st
                             schet toBase snumber st = schet (toBase) (show((read snumber :: Integer) `div` toBase)) ([stt !! ((read snumber :: Int) `mod` fromIntegral toBase)] ++ st)
                             stt = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             isTrue toBase snumber | toBase < 0 || toBase > 62 = error "Invalid number system" 
                                                   | (read snumber :: Integer) < 0 = False
                                                   | otherwise = True

convertFromTo :: Integer -> Integer -> String -> String
convertFromTo fromBase toBase snumber = fromDecimal toBase (toDecimal fromBase snumber)
