-- Questão: Encontre o K'ésimo elemento de uma lista.
elementAt :: [a] -> Int -> a
elementAt list k = list !! (k-1)

main :: IO ()
main = do
    let index = elementAt ['a'..'g'] 4
    putStrLn $ "K = " ++ show index