-- Descubra se uma lista é um palíndromo.
inverter :: [a] -> [a]
inverter [] = []
inverter (x:xs) = inverter xs ++ [x]

ePalindromo :: Eq a => [a] -> Bool
ePalindromo lista = lista == (inverter lista)


main :: IO ()
main = do
    let palindromo = ePalindromo [ 1 , 2 , 4 , 8 , 16 , 8 , 4 , 2 , 1 ]
    putStrLn $ show palindromo