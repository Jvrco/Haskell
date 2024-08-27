testaElementos :: (a -> Bool) -> [a] -> Bool
testaElementos _ [] = True
testaElementos x (a:as)
    | not(x a) == False = False
    | otherwise = testaElementos as

testaElementosB :: (a -> Bool) -> [a] -> Bool
testaElementosB f xs = and (map f xs)

testaElementosC :: (a -> Bool) -> [a] -> Bool
testaElementos f = foldr (\x acc -> f x && acc) True
