-- Eliminar os elementos consecutivos repetidos
eliminarRepitidos :: Eq a => [a] -> [a]
eliminarRepitidos [] = []
eliminarRepitidos [x] = [x]
eliminarRepitidos (x:y:xs) 
    | x == y = eliminarRepitidos(y:xs)
    | otherwise = x : eliminarRepitidos (y:xs)

main :: IO ()
main = do
    let eliminados = eliminarRepitidos "abaabccaadeeee"
    putStrLn $ show eliminados