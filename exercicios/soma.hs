--Link exercicio: https://www.beecrowd.com.br/judge/pt/problems/view/1001
soma :: Int -> Int -> Int
soma a b = a + b

main :: IO ()
main = do
    input <- getLine
    let valor1 = read input :: Int
    input <- getLine
    let valor2 = read input :: Int
    let resultadoSoma = soma valor1 valor2
    putStrLn $ "X = " ++ show resultadoSoma
