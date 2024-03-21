-- Questão: Encontre o último elemento de uma lista.
meuUltimo :: [a] -> a
meuUltimo [x] = x
meuUltimo (_:xs) = meuUltimo xs

main :: IO ()
main = do
    let ultimo = meuUltimo ['a'..'c']
    putStrLn $ "ultimo = " ++ show ultimo