-- Definição das funções
poli :: Integer -> Integer -> Integer -> Integer -> Integer
poli a b c = (\x -> (a * x * x) + (b * x) + c)

listaPoli :: [(Integer, Integer, Integer)] -> [Integer -> Integer]
listaPoli l = [poli x y z | (x, y, z) <- l]

appListaPoli :: [Integer -> Integer] -> [Integer] -> [Integer]
appListaPoli [] [] = []
appListaPoli (a:as) (b:bs) = (a b) : appListaPoli as bs

-- Função main para testar as funções
main :: IO ()
main = do
    -- Testando listaPoli
    let coeficientes = [(1, 2, 3), (2, 3, 4), (3, 4, 5)] -- Lista de tuplas (a, b, c)
    let funcoes = listaPoli coeficientes -- Gera a lista de funções polinomiais
    
    -- Testando appListaPoli
    let valores = [1, 2, 3] -- Lista de valores de x para serem aplicados nas funções
    let resultados = appListaPoli funcoes valores -- Aplica os valores nas funções
    
    -- Exibe os resultados
    putStrLn "Resultados:"
    print resultados
