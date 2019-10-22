quadrat a b c | (descr < 0) = error "Can't find real roots"
              | (a == 0) && (b == 0) = error "Infinity"
              | (a == 0) && (b /= 0) = [-c / b]
              | ((descr >= 0) && (a /= 0)) = [rootOne, rootTwo]
              | otherwise = error "It's not quadratic equation"
              where descr = b * b - 4 * a * c
                    rootOne = ((-b + sqrt(b * b - 4 * a * c)) / (2 * a))
                    rootTwo = ((-b - sqrt(b * b - 4 * a * c)) / (2 * a))
