module AutoPlayer (
    autoMove
) where

import GameBoard
import System.Random
import Data.Maybe


-- main function to play a move based on a given board
autoMove :: GameBoard -> IO GameBoard
autoMove gb = 
    do
        let
            coords = [(rowIndices, columnIndices) | columnIndices <- [0 .. GameBoard.width gb - 1], rowIndices <- [0 .. GameBoard.height gb - 1]]
            firstTry =  recursiveTry gb flagMines coords 
            secondTry =  recursiveTry gb uncoverEmptyAdjacent coords 
            in
            if isJust firstTry
                then return $ fromMaybe gb firstTry
                else if isJust secondTry
                    then return $ fromMaybe gb secondTry
                    else do
                        selectRandom gb <$> newStdGen

-- function to recursively attempt to call a function on a list of given coordinates
recursiveTry :: GameBoard -> (GameBoard -> (Int, Int) -> Maybe GameBoard )->[(Int, Int)] -> Maybe GameBoard
recursiveTry _ _ [] = Nothing
recursiveTry gb f (xy : xys) = 
    let returned = f gb xy in 
    if isJust returned then returned
    else 
        recursiveTry gb f xys



-- function to flag an ajdacent square if definitely a mine
flagMines :: GameBoard -> (Int, Int) -> Maybe GameBoard
flagMines gb (x, y) = 
    let 
        adjacentSquares = getAdjacents gb (x, y)
        numHidden = length $ filter (== hidden) (map (getStatus gb) adjacentSquares)
        numMines = getValue gb (x, y)
        numFlags = length $ filter (== flagged) (map (getStatus gb) adjacentSquares)
    in if numHidden > 0 && numHidden + numFlags == numMines
        then 
            Just $ flagCells gb adjacentSquares
        else 
            Nothing

-- function to flag a list of squares
flagCells :: GameBoard -> [(Int, Int)] -> GameBoard
flagCells gb [] = gb
flagCells gb (xy : xys) = 
    if getStatus gb xy == hidden
        then 
            updateStatus xy gb flagged
        else 
            flagCells gb xys



-- uncover a hidden adjacent square if all adjacent mines have been flagged
uncoverEmptyAdjacent :: GameBoard -> (Int, Int) -> Maybe GameBoard
uncoverEmptyAdjacent gb (x, y) =
    let 
        adjacentSquares = getAdjacents gb (x, y)
        numHidden = length $ filter (== hidden) (map (getStatus gb) adjacentSquares)
        numMines = getValue gb (x, y)
        numFlags = length $ filter (== flagged) (map (getStatus gb) adjacentSquares)
    in 
        if getStatus gb (x,y) == visible && numFlags == numMines && numHidden > 0
        then 
            Just $ uncoverAll gb adjacentSquares
        else 
            Nothing 

-- function to get the coordinates of all the adjacent squares to a given square
getAdjacents :: GameBoard -> (Int, Int) -> [(Int, Int)]
getAdjacents gb (x, y) = 
    let unfiltered = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    in filter (onBoard gb) unfiltered

-- function to uncover the adjacent squares of all squares in a given list of squares
uncoverAll :: GameBoard -> [(Int, Int)] -> GameBoard
uncoverAll gb [] = gb
uncoverAll gb (xy : xys) = 
    if getStatus gb xy == hidden
        then uncoverAdjacent xy gb
        else uncoverAll gb xys



-- function to uncover a random square if all else fails             
selectRandom :: GameBoard -> StdGen -> GameBoard 
selectRandom gb stdGen = 
        let
            (randomX, newGenerator) = randomR (0, width gb-1) stdGen
            (randomY, newerGenerator) = randomR (0, height gb-1) newGenerator
            squareStatus = getStatus gb (randomX, randomY)
        in  
            if squareStatus == hidden 
            then uncoverAdjacent (randomX, randomY) gb
            else selectRandom gb newerGenerator
