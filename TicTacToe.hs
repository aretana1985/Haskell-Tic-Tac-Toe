import Data.List(transpose)



type Board= [[Char]]

initialBoard :: Board
initialBoard = replicate 3 $  replicate 3 ' '


--board
displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn $ unlines board

---player input
readMove :: IO (Int, Int)
readMove = do
    putStrLn "Enter row (0-2): "
    row <- readLn
    putStrLn "Enter column (0-2): "
    col <- readLn
    return (row, col)

--update
updateBoard :: Board -> (Int, Int) -> Char -> Board
updateBoard board (row, col) player = 
    take row board ++
    [take col (board !! row) ++ [player] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

--check
isWinning :: Board -> Bool
isWinning b = any lineComplete (rows ++ cols ++ diags)
    where
        rows = b
        cols = transpose b
        diags= [diag b, diag (map reverse b)]
        diag b = [b !! n !! n | n <- [0..2]]
        lineComplete line = all (== 'X') line || all (== 'O') line


isDraw :: Board -> Bool
isDraw b = all(all (/= ' ')) b

--game
main :: IO ()
main = gameLoop initialBoard 'X'

gameLoop :: Board -> Char -> IO ()
gameLoop board player = do
    displayBoard board
    if isWinning board
    then putStrLn $ "Player " ++ show player ++ " wins!"
    else if isDraw board
    then putStrLn "It's a draw!"
    else do
        putStrLn $ "Player " ++ show player ++ " move:"
        move <- readMove
        let newBoard = updateBoard board move player
        gameLoop newBoard (if player == 'X' then 'O' else 'X')
