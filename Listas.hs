-- Listas em haskell são encadeadas, portanto, a ordem importa
-- [5] == 5:[]
-- (:) :: a -> [a] -> [a]
--[2..7] == [1,2,3,4,5,6,7]
-- função head devolve o primeiro elemento da lista, tail devolve tudo alem da cabeça

length::[t] -> t 
    length [] = 0
    length (a:as) = 1 + length as 

--concatenação
(++) :: [t] -> [t] -> [t]
    [] ++ y =  y 
    (x:xs) ++ y = x : (xs ++ y)

-- Somar uma lista de Inteiros
sumList [Int] -> Int
    sumList as
        | as == [] = 0
        | otherwise = (head as) + sumList(tail as)

-- Expressão Case
firstDigit :: String -> Char
firstDigit st = case (digits st) of
    [] -> '\0'
    (a:as) -> a


-- Compreensão de listas : [x^2| x <-[1..10]] vai retornar o quadrado de todos os numeros de 1 ate 10

