-- Crie uma definição recursiva para a função length :: [a] -> Integer, 
-- que calcula o comprimento de uma lista. Use encaixe de padrões 
-- para o caso base e o caso recursivo.

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

main :: IO ()
main = do
    let comprimento = length ['a'..'g']
    putStrLn $ "Tamanho da lista = " ++ show comprimento