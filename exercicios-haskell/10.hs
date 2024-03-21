-- Codificação de comprimento de execução de uma lista.
empacotar :: Eq a => [a] -> [[a]]
empacotar [] = []
empacotar [x] = [[x]]
empacotar (x:xs) =
    let (primeiroGrupo, restante) = span (== x) xs
    in (x : primeiroGrupo) : empacotar restante
    
meuPrimeiro :: [a] -> a
meuPrimeiro [x] = x
meuPrimeiro (x:_) = x

meuComprimento :: [a] -> Int
meuComprimento [] = 0
meuComprimento (_:xs) = 1 + meuComprimento xs

codificar :: Eq a => [a] -> [(Int, a)]
codificar = map (\xs -> (meuComprimento xs, meuPrimeiro xs)) . empacotar

main :: IO ()
main = do
    let codificado = codificar ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
    putStrLn $ show codificado