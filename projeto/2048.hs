import Data.List
import System.IO

-- Define os possíveis estados de uma célula no tabuleiro
data Tile = Empty | Tile Int deriving (Eq)

-- Define como exibir uma célula (vazio ou com valor)
instance Show Tile where
    show Empty = "."
    show (Tile n) = show n

-- Define o tipo do tabuleiro
type Board = [[Tile]]

-- Cria o tabuleiro inicial com duas células vazias aleatórias
initialBoard :: Board
initialBoard = insertRandomTile $ insertRandomTile $ replicate 4 (replicate 4 Empty)

-- Atualiza uma lista aplicando várias atualizações em uma única passagem
(//) :: [a] -> [(Int, a)] -> [a]
(//) = foldl (\xs' (i, x) -> take i xs' ++ [x] ++ drop (i+1) xs')

-- Insere um novo tile com valor 2 em uma célula vazia aleatória do tabuleiro
insertRandomTile :: Board -> Board
insertRandomTile board =
    let emptyTiles = [(x, y) | x <- [0..3], y <- [0..3], board !! x !! y == Empty]
    in if null emptyTiles
           then board
           else let (x, y) = emptyTiles !! (length emptyTiles - 1)
                    value = 2
                in board // [(x, board !! x // [(y, Tile value)])]

-- Converte o tabuleiro para uma string formatada para exibição
showBoard :: Board -> String
showBoard board = unlines [concat [show tile ++ " " | tile <- row] | row <- board]

-- Move o tabuleiro na direção especificada ('w', 's', 'a', 'd')
moveBoard :: Board -> Char -> Board
moveBoard board direction
    | direction == 'w' = transpose $ moveLeft $ transpose board
    | direction == 's' = moveDown board
    | direction == 'a' = moveLeft board
    | direction == 'd' = moveRight board
    | otherwise = board

-- Move todas as células para baixo em cada coluna, combinando células iguais
moveDown :: Board -> Board
moveDown board = transpose $ map moveColumnDown $ transpose board

-- Move uma coluna para baixo, combinando células iguais
moveColumnDown :: [Tile] -> [Tile]
moveColumnDown column =
    let nonEmptyTiles = filter (/= Empty) column
        emptySpaces = replicate (4 - length nonEmptyTiles) Empty
        mergedTiles = mergeTiles nonEmptyTiles
    in emptySpaces ++ mergedTiles

-- Move todas as células para a esquerda em cada linha, combinando células iguais
moveLeft :: Board -> Board
moveLeft = map collapseRow
    where collapseRow row = mergeTiles $ filter (/= Empty) row ++ replicate (4 - length (filter (/= Empty) row)) Empty

-- Move todas as células para a direita em cada linha, combinando células iguais
moveRight :: Board -> Board
moveRight = map moveRowRight

-- Move uma linha para a direita, combinando células iguais
moveRowRight :: [Tile] -> [Tile]
moveRowRight row =
    let nonEmptyTiles = filter (/= Empty) row
        emptySpaces = replicate (4 - length nonEmptyTiles) Empty
        mergedTiles = mergeTiles nonEmptyTiles
    in emptySpaces ++ mergedTiles

-- Combina células iguais em uma linha
mergeTiles :: [Tile] -> [Tile]
mergeTiles [] = []
mergeTiles [x] = [x]
mergeTiles (Tile x : Tile y : rest)
    | x == y = Tile (x+y) : mergeTiles rest ++ [Empty]
    | otherwise = Tile x : mergeTiles (Tile y : rest)
mergeTiles (x : rest) = x : mergeTiles rest

-- Verifica se não há mais movimentos possíveis em qualquer direção
gameOver :: Board -> Bool
gameOver board =
    let directions = ['w', 'a', 's', 'd']
    in all (\dir -> moveBoard board dir == board) directions

-- Loop principal do jogo
gameLoop :: Board -> IO ()
gameLoop board = do
    putStrLn $ showBoard board
    if gameOver board
        then do
            putStrLn "Game Over! Digite 'R' para reiniciar o jogo ou qualquer outra tecla para sair."
            choice <- getChar
            if choice == 'R' || choice == 'r'
                then gameLoop initialBoard
                else putStrLn "Goodbye!"
        else do
            putStr "Digite para mover uma peça(wasd): "
            hFlush stdout
            move <- getChar
            putStrLn ""
            let newBoard = moveBoard board move
            if newBoard /= board
                then do
                    let newBoard' = insertRandomTile newBoard
                    gameLoop newBoard'
                else do
                    gameLoop board

-- Função principal que inicia o jogo
main :: IO ()
main = do
    putStrLn "Bem-vindo ao jogo 2048!"
    putStrLn "Deslize as peças para a direita, esquerda, cima ou baixo para combinar peças iguais."
    putStrLn "Cada movimento moverá todas as peças na direção escolhida."
    putStrLn "Quando duas peças com o mesmo número se encontram, elas se fundem em uma única peça com o dobro do valor."
    putStrLn "O objetivo é alcançar uma peça de valor 2048."
    putStrLn ""
    putStrLn "Instruções de movimento:"
    putStrLn "  - Use 'w' para cima"
    putStrLn "  - Use 'a' para esquerda"
    putStrLn "  - Use 's' para baixo"
    putStrLn "  - Use 'd' para direita"
    putStrLn ""
    putStrLn "Pressione qualquer tecla para começar o jogo."
    _ <- getChar
    gameLoop initialBoard
