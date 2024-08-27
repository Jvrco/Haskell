
-- a
f:: [Int] -> [Int]
f [] = []
f (a:b:as)
    | a == b = a : f (b:as)
    | otherwise = f (b:as)

-- b

fb :: [Int] -> [Int]
fb xs = [x | (x,y) <- zip xs (tail xs), x == y]

main::IO()
main = do
    let x = [1,1,2,2,3]
    let resultado = f x
    print(resultado)