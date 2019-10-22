quadrat 0 0 _ = error "Infinity"
quadrat 0 b c = [-c / b]
quadrat a b c | (descr < 0) = error "Can't find real roots"
              | otherwise = [rootOne, rootTwo]
              where descr = b * b - 4 * a * c
                    rootOne = ((-b + sqrt(b * b - 4 * a * c)) / (2 * a))
                    rootTwo = ((-b - sqrt(b * b - 4 * a * c)) / (2 * a))
