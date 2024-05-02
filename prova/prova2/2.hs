-- Alessandra Maria Ramos Barros de Moura 20200136795
-- Função auxiliar para calcular a média de uma lista de números inteiros
mean :: [Int] -> Float
-- sum xs calcula a soma de todos os elementos da lista xs
-- length xs retorna o comprimento (número de elementos) da lista xs
-- A divisão é realizada convertendo-se a soma e o comprimento para Float 
-- com fromIntegral para obter o resultado da média em ponto flutuante.
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- Função principal que lê do terminal e calcula a média
printMean :: IO ()
printMean = do
    -- Lê o número N do terminal
    nStr <- getLine
    -- converte para número
    let n = read nStr
    -- Cria uma lista com n elementos, 
    -- onde cada elemento é o resultado da função getLine.
    -- E sequence ela combina várias ações de leitura de linha (getLine)
    -- em uma única ação que, quando executada, produz uma 
    -- lista de todas as strings lidas.
    numsStr <- sequence (replicate n getLine)
    -- Aplica a função read a cada elemento de numsStr
    -- convertendo cada string em um número 
    let nums = map read numsStr
    -- Calcula a média dos números inteiros
    let result = mean nums
    -- Escreve o resultado formatado no terminal
    putStr (show result ++ "\n")

-- Executa o programa
main :: IO ()
main = printMean
