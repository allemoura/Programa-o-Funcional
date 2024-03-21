-- Empacote duplicatas consecutivas de elementos da lista em sublistas.
empacotar :: Eq a => [a] -> [[a]]
empacotar [] = []
empacotar [x] = [[x]]
empacotar (x:xs) =
    let (primeiroGrupo, restante) = span (== x) xs
    in (x : primeiroGrupo) : empacotar restante

main :: IO ()
main = do
    let empacotados = empacotar ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
    putStrLn $ show empacotados