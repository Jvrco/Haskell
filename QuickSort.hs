quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let menoresOuIguais = [a | a <- xs, a <= x]
        maiores = [a | a <- xs, a > x]
    in quicksort menoresOuIguais ++ [x] ++ quicksort maiores
