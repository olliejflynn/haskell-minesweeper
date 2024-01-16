module GameBoard (
    GameBoard (..), Showing (..), Modes (..),
    mine, noMine,
    visible, flagged, hidden,
    inPlay, win, loss,
    onBoard,
    getStatus, updateStatus, getValue,
    createEmptyBoard, plantMines,
    updateState,
    uncover, uncoverAdjacent, placeFlag,
) where
import Data.Vector
import System.Random

-- data types to help improve readability of the code
data State = InPlay | Win | Loss deriving (Eq, Show)
data Showing = Visible | Hidden | Flagged deriving (Eq, Show)
type MinesAdj = Int
data Modes = UnCover | Flag 
data GameBoard = GameBoard {
                    width :: Int,
                    height :: Int,
                    squares :: Vector (Vector MinesAdj),
                    squareStatuses :: Vector (Vector Showing),
                    numMines :: Int,
                    state :: State
                    }


-- Cell types represented by a function name, to be used like constants
mine, noMine :: MinesAdj
mine = -1
noMine = 0

-- Cell showing statues represented by a function name, to be used like constants
visible, hidden, flagged :: Showing
visible = Visible
hidden = Hidden 
flagged = Flagged

-- Game states represented by a function name, to be used like constants
inPlay, win, loss :: State
inPlay = InPlay
win = Win 
loss = Loss 

-- Function to check if given coordinates are on a guven gameboard
onBoard :: GameBoard -> (Int, Int) -> Bool
onBoard gb (x, y) = 
    x >= 0 && x < width gb && y >= 0 && y < height gb

-- Get status of a given square on a given board
getStatus :: GameBoard -> (Int, Int) -> Showing
getStatus gb (x, y) = squareStatuses gb ! x ! y

-- Update status of a given square on a given board
updateStatus :: (Int, Int) -> GameBoard -> Showing -> GameBoard
updateStatus (x, y) gb status = 
    let 
        newRow  = update (squareStatuses gb ! x) (singleton (y, status))
        newStatuses = update (squareStatuses gb) (singleton (x, newRow ))
    in gb {squareStatuses = newStatuses}

-- Get the value of a given square on a given board
getValue ::  GameBoard -> (Int, Int) -> Int
getValue gb (x,y) = 
    if onBoard gb (x,y) then
        squares gb ! x ! y
    else
        -5

-- Create a new board with the given width and height, where every square is empty and hidden
createEmptyBoard :: Int -> Int -> GameBoard
createEmptyBoard width height = GameBoard {
    squares = generate width (\_ -> Data.Vector.replicate height noMine ),
    squareStatuses = generate width (\_ -> Data.Vector.replicate height hidden),
    width = width,
    height = height,
    numMines = 0,
    state = inPlay
}

-- update the value at a given square on a given board to a given value
updateCellValue :: (Int, Int) -> GameBoard -> MinesAdj -> GameBoard
updateCellValue (x, y) gb value = 
    let 
        newRow = update (squares gb ! x) (singleton (y, value))
        newCells = update (squares gb) (singleton (x, newRow))
    in gb {squares = newCells}

-- increment the value of a given square on a given board
incrementSquareValue :: (Int, Int) -> GameBoard -> GameBoard
incrementSquareValue coords gb =
    if onBoard gb coords
    then updateCellValue coords gb $ getValue gb coords + 1
    else gb

-- increment the value of every square that is adjacent to (x, y) if it isn't a mine
incrementAdjacentCells :: (Int, Int) -> GameBoard -> GameBoard
incrementAdjacentCells (x,y) gb = incTheseSquares [(x-1, y-1), (x,y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x,y+1), (x+1, y+1)] gb
 where 
     incTheseSquares [] gb = gb
     incTheseSquares (head:tail) gb = 
         if getValue gb head == mine 
         then
            incTheseSquares tail gb
         else
            incrementSquareValue head $ incTheseSquares tail gb

-- function to plant a mine in a given coordinate on a given board
plantMine :: (Int, Int) -> GameBoard -> GameBoard
plantMine coords gb = 
    incrementAdjacentCells coords gbWithMinePlanted
    where
        gbWithMinePlanted = updateCellValue coords gb mine-- plant a mine at the coords

-- function to check if a mine shouldn't be planted on a given board in a given spot
--  based on if its already a mine or if its adjacent to or is the coordinates of the first square clicked
dontPlantHere :: GameBoard -> (Int, Int) -> (Int, Int) -> Bool
dontPlantHere gb (x, y) (skipX, skipY) =
    if isMine then True else isInArea
    where
        isMine = getValue gb (x, y) == mine
        isInArea = (x >= skipX-1 && x <= skipX+1 && y >= skipY-1 && y <= skipY+1)

-- function to plant a given number of mines on a given board
-- taking coordinates of the first click to make sure no mine is planted there
plantMines :: GameBoard -> Int -> (Int, Int) -> StdGen -> GameBoard
plantMines gb 0 _ _ = gb
plantMines gb numMines coordsToSkip generator =
    let 
        (randomX, generator1) = randomR (0, width gb-1) generator
        (randomY, generator2) = randomR (0, height gb-1) generator1
        in
            if dontPlantHere gb (randomX, randomY) coordsToSkip
                then plantMines gb numMines coordsToSkip generator2
                else 
                    let plantedBoard = plantMine (randomX, randomY) gb  
                    in plantMines plantedBoard (numMines-1) coordsToSkip generator2


-- function to update the game state to win or loss based on uncovered squares
updateState :: GameBoard -> GameBoard
updateState gb = 
    let coordsList = [(columnIndices, rowIndices) | columnIndices <- [0 .. GameBoard.width gb - 1], rowIndices <- [0 .. GameBoard.height gb - 1]]
    in updateStateRecursive gb win coordsList

-- iterate through the board to check the game state, assume win unless mine is uncovered
updateStateRecursive :: GameBoard -> State -> [(Int, Int)] -> GameBoard
updateStateRecursive gb state [] = gb {state = state}
updateStateRecursive gb Win (coords:coordsList)
    | getStatus gb coords == visible && getValue gb coords == mine
        = gb {state = loss}
    | getStatus gb coords /= visible && getValue gb coords /= mine
        = updateStateRecursive gb InPlay coordsList
    | otherwise = 
        updateStateRecursive gb Win coordsList
updateStateRecursive gb InPlay (coords:coordsList) =
   if getStatus gb coords == visible && getValue gb coords == mine
    then gb {state = loss}
    else updateStateRecursive gb InPlay coordsList


-- function to uncover a square on a given board in a given position
uncover :: GameBoard -> (Int, Int) -> GameBoard
uncover gb (x, y) = 
    let squareStatus = getStatus gb (x, y) 
    in if squareStatus == visible || squareStatus == flagged
        then gb
        else
            uncoverAdjacent (x, y) $ updateStatus (x, y) gb hidden


-- function to uncover all adjacent squares to a given square on a given board which arent mines if the squares value is 0
uncoverAdjacent :: (Int, Int) -> GameBoard -> GameBoard
uncoverAdjacent (x, y) gb =
    if onBoard gb (x, y) && (getStatus gb (x, y) == hidden)
    then 
        if getValue gb (x, y) == noMine then
            let updatedBoard = updateStatus (x, y) gb visible
            in  uncoverAdjacent (x-1, y-1) $ uncoverAdjacent (x, y-1) $ uncoverAdjacent (x+1, y-1) $ uncoverAdjacent (x-1, y) $ uncoverAdjacent (x+1, y) $ uncoverAdjacent (x-1, y+1) $ uncoverAdjacent (x, y+1) $ uncoverAdjacent (x+1, y+1) updatedBoard
        else updateStatus (x, y) gb visible
    else gb


-- function to place a flag on a given square on a given board
placeFlag :: GameBoard -> (Int, Int) -> GameBoard
placeFlag gb (x, y) = 
    let squareStatus = getStatus gb (x, y)
    in  if squareStatus == flagged
        then updateStatus (x, y) gb hidden
        else if squareStatus == hidden
            then updateStatus (x, y) gb flagged
            else gb