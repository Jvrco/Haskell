sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (a:as) = [a:ys|ys <- sublistas as] ++ sublistas as


main :: IO ()
main = do
    let lista = [1, 2, 3]
    let resultado = sublistas lista
    putStrLn "Sublistas geradas:"
    print resultado