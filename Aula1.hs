answer:: Int
answer = 42

greater:: Bool
greater = (answer > 71)

yes:: Bool
yes = True

--Variáveis começam com letra minúscula, mas nn necessariamente tem que ter apeans letras minusculas
nO :: Bool
nO = False

square:: Int -> Int
square x = x * x

allEqual :: Int ->Int->Int->Bool
maxi n m p = (n==m) && (m==p)

maxi :: Int -> Int -> Int
maxi n m | n> m   = n  
         | n == m = n 
         | otherwise = m 
-- Letra maiuscula é reservada para tipos e construtores
-- Funções em Haskell tem precedencia a qualquer coisa
vendas:: Int -> Int
vendas n = n 

totalVendas:: Int -> Int
totalVendas n | n == 0 = 0
              | n > 0 = vendas n + totalVendas(n-1)
              | otherwise = 0

sumSquares :: Int -> Int -> Int

sumSquares x y = sqX + sqY
    where sqX = x * x 
          sqY = y * y
