-- ax2 + bx + c = 0
-- x = (-b +- sqrt(b2-4ac))/2a

oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b/(2.0*a)

twoRoots :: Float -> Float -> Float -> (Float,Float)
twoRoots a b c = (d-e,d+e)
    where d = oneRoot -b/(2.0*a)
          e = sqrt(b*b - 4.0*a*c)/(2.0*a)

roots:: Float -> Float -> Float -> (Int,Float,Float)
roots a b c | b*b - 4.0*a*c < 0 = (0,0,0)
            | b*b - 4.0*a*c == 0 = (1,(oneRoot a b c),0)
            | otherwise     0 = (2,fst raizes, snd raizes)
            where raizes = twoRoots a b c
