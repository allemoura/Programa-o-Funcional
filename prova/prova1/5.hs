-- Escreva uma função arePairs :: [Integer] -> [Bool] 
-- que retorna uma lista em que cada elemento é True 
--se o elemento da lista original da mesma posição é par, 
--ou False caso contrário. 
--Exemplo: arePairs [1, -2, 4] = [False, True, True].
arePairs :: [Int] -> [Bool]
arePairs [] = []
arePairs (x:xs) = even x : arePairs xs

main :: IO ()
main = do
    let ePar = arePairs [1, -2, 4]
    putStrLn $ show ePar