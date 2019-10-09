quadrat a b c | (descr a b c < 0) = error "Can't find real roots"
              | (a == 0) && (b == 0) = error "Infinity"
              | (a == 0) && (b /= 0) = [-c / b]
              | ((descr a b c >= 0) && (a /= 0)) = [rootOne a b c, rootTwo a b c]
              | otherwise = error "It's not quadratic equation"
              where descr a b c = b * b - 4 * a * c
                    rootOne a b c = ((-b + sqrt(b * b - 4 * a * c)) / (2 * a))
                    rootTwo a b c = ((-b - sqrt(b * b - 4 * a * c)) / (2 * a))