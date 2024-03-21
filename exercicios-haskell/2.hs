-- Encontre o penúltimo elemento (ou penúltimo) de uma lista.
meuPenultimo :: [a] -> a
meuPenultimo [x,_] = x
meuPenultimo (_:x) = meuPenultimo x

main :: IO ()
main = do
    let penultimo = meuPenultimo ['a'..'c']
    putStrLn $ "penultimo = " ++ show penultimo