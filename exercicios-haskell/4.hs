-- Questão: Encontre o número de elementos em uma lista.
meuComprimento :: [a] -> Int
meuComprimento [] = 0
meuComprimento (_:xs) = 1 + meuComprimento xs

main :: IO ()
main = do
    let comprimento = meuComprimento ['a'..'g']
    putStrLn $ "Tamanho da lista = " ++ show comprimento