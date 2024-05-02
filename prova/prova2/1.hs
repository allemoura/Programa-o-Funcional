
-- ***** QUESTÃO 1 *****
-- Funcao Filter apresentada pela professora em aula e slide
-- Definição da função filter que recebe um predicado p e uma lista xs e retorna uma nova lista
-- contendo apenas os elementos de xs para os quais p retorna True
myFilter :: (a -> Bool) -> [a] -> [a]
-- Caso base: se a lista de entrada for vazia, o resultado também é uma lista vazia
myFilter p [] = []
-- Caso recursivo: se a lista de entrada não for vazia, divide-se a lista em cabeça (x) e cauda (xs)
myFilter p (x:xs)
    -- Se p x for True, adiciona-se x à lista resultante e continua-se a filtrar o restante da lista (xs)  
    | p x       = x : myFilter p xs
    -- Se p x for False, não adiciona-se x à lista resultante, mas continua-se a filtrar o restante da lista (xs)
    | otherwise = myFilter p xs

-- Definicao da funcao
removeAll :: (a -> Bool) -> [a] -> [a]
-- A função myFilter é usada para filtrar os elementos da lista xs
-- A função passada para myFilter é (not . p), que remove os elementos que satisfazem o predicado p
-- filter (not . p) xs
removeAll p xs = myFilter (not . p) xs



-- ***** QUESTÃO 2 ******
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


-- ******* QUESTÃO 3 *******
-- Define um novo tipo de dado chamado Direction, que representa as direções para onde o robô pode se mover.
data Direction = North | South | East | West
    deriving (Eq, Show)

-- Define um novo tipo de dado chamado RobotState, que representa o estado atual do robô, incluindo suas coordenadas e direção.
data RobotState = RobotState { xCoord :: Int, yCoord :: Int, direction :: Direction }
    deriving (Eq, Show)

-- Define um novo tipo de dado chamado Command, que representa os comandos que podem ser dados ao robô.
data Command = Forward Int | Backward Int | TurnLeft | TurnRight
    deriving (Eq, Show)

-- Função para calcular a nova posição do robô
destination :: [Command] -> (Int, Int)
destination commands = calculateDestination commands (RobotState 0 0 North)

-- Função auxiliar para calcular a nova posição do robô a partir de uma lista de comandos e um estado inicial
calculateDestination :: [Command] -> RobotState -> (Int, Int)
calculateDestination [] (RobotState x y _) = (x, y) -- Se não houver mais comandos, retorna as coordenadas atuais
calculateDestination (x:xs) state = calculateDestination xs (moveRobot state x) -- Move o robô com o próximo comando e continua o cálculo

-- Função para mover o robô de acordo com um comando
moveRobot :: RobotState -> Command -> RobotState
moveRobot (RobotState x y dir) (Forward n) = case dir of
    North -> RobotState x (y + n) dir -- Move para frente na direção Norte
    South -> RobotState x (y - n) dir -- Move para frente na direção Sul
    East  -> RobotState (x + n) y dir -- Move para frente na direção Leste
    West  -> RobotState (x - n) y dir -- Move para frente na direção Oeste
moveRobot (RobotState x y dir) (Backward n) = case dir of
    North -> RobotState x (y - n) dir -- Move para trás na direção Norte
    South -> RobotState x (y + n) dir -- Move para trás na direção Sul
    East  -> RobotState (x - n) y dir -- Move para trás na direção Leste
    West  -> RobotState (x + n) y dir -- Move para trás na direção Oeste
moveRobot (RobotState x y dir) TurnLeft = case dir of
    North -> RobotState x y West -- Vira à esquerda quando estiver indo para o Norte
    South -> RobotState x y East -- Vira à esquerda quando estiver indo para o Sul
    East  -> RobotState x y North -- Vira à esquerda quando estiver indo para o Leste
    West  -> RobotState x y South -- Vira à esquerda quando estiver indo para o Oeste
moveRobot (RobotState x y dir) TurnRight = case dir of
    North -> RobotState x y East -- Vira à direita quando estiver indo para o Norte
    South -> RobotState x y West -- Vira à direita quando estiver indo para o Sul
    East  -> RobotState x y South -- Vira à direita quando estiver indo para o Leste
    West  -> RobotState x y North -- Vira à direita quando estiver indo para o Oeste
