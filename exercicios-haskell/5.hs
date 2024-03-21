-- Inverter uma lista.
inverter :: [a] -> [a]
inverter [] = []
inverter (x:xs) = inverter xs ++ [x]

main :: IO ()
main = do
    let comprimento = inverter ['a'..'g']
    putStrLn $ show comprimento