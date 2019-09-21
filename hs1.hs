roots a b c  | (d > 0) = (x1,x2)
             | (d == 0) = (x1, x2)
			 | (d < 0) = error "No material roots"
  where
    d = b^2 - 4*a*c
    ds = sqrt d
    x1 = (-b + ds) / (2*a)	
    x2 = (-b - ds) / (2*a)   