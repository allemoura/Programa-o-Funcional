-- Alessandra Maria Ramos Barros de Moura 20200136795
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



main :: IO ()
main = do
    let commands1 = [Forward 2, TurnLeft, TurnLeft, Forward 1]
    putStrLn $ "Teste 1: " ++ show (destination commands1) -- Saída: (0,1)

    let commands2 = [Backward 2, Forward 1]
    putStrLn $ "Teste 2: " ++ show (destination commands2) -- Saída: (0,-1)
