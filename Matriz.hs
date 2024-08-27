--Dada uma matriz representada por uma lista de listas, defina fun ̧c ̃oes para:
--(a) indicar se a mesma  ́e uma matriz (se todas as linhas tˆem o mesmo tamanho)
--(b) permutar a posi ̧c ̃ao de duas linhas x e y, assumindo que x < y. Dica: pode-se utilizar as fun ̧c ̃oes
--init, take, drop e !! .



-- Função que verifica se uma lista de listas é uma matriz
isMatrix :: [[a]] -> Bool
isMatrix [] = False -- Uma matriz vazia não é considerada válida
isMatrix (x:xs) = all (\row -> length row == length x) xs

-- Função para permutar duas linhas da matriz
permuteLines :: [[a]] -> Int -> Int -> [[a]]
permuteLines matrix i j
  | i == j = matrix  -- Se os índices forem iguais, a matriz não muda
  | otherwise = take i matrix              -- Parte da matriz antes da linha `i`
                ++ [matrix !! j]           -- Substitui a linha `i` pela linha `j`
                ++ take (j - i - 1) (drop (i + 1) matrix) -- Parte da matriz entre as linhas `i` e `j`
                ++ [matrix !! i]           -- Substitui a linha `j` pela linha `i`
                ++ drop (j + 1) matrix     -- Parte da matriz após a linha `j`

main:: IO()
main = do
    let m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    let resultado =  permuteLines m 0 2
    print resultado