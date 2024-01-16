import Graphics.UI.Threepenny as UI
import GameBoard
import AutoPlayer
import Styles
import Data.IORef
import Control.Monad
import System.Random


-- main IO action
main :: IO () 
main = do
    startGUI defaultConfig setup

-- function to help give board checkered appearance
whichColour :: (Int, Int) -> Bool
whichColour (x, y) = 
    if (mod y 2) == 1 then 
        (mod x 2) == 1
    else
        (mod x 2) == 0

-- function to change button appearance to highlight which one is selected
updateButtonStyles :: Element -> Element -> UI ()
updateButtonStyles button1 button2 = do
    set' style styleSelected button1
    set' style styleUnSelected button2

-- function to redraw the canvas
drawCanvas :: Element -> GameBoard -> Double -> Double -> UI ()
drawCanvas canvas gbValue numRows numCols =
    do
        clearCanvas canvas
        -- if the game is still in play draw the board
        -- otherwise draw the board with an overlay displaying whether the game was won or lost
        if state gbValue == inPlay
            then do
                drawSquares canvas gbValue (canvasHeight / numRows) (canvasWidth / numCols)
            else 
                if state gbValue == win then do
                    drawSquares canvas gbValue (canvasHeight / numRows) (canvasWidth / numCols)
                    set' fillStyle (solidColor (RGBA 0 255 0 0.5)) canvas
                    fillRect ((canvasHeight/10),((canvasWidth/10))) ((canvasHeight/10)*8) ((canvasWidth/10)*8) canvas
                    set' fillStyle (htmlColor "white") canvas
                    set' textFont ((show (canvasWidth*0.15)) ++ "px Palatino") canvas
                    fillText "WINNER" (((canvasHeight/10) + ((canvasHeight/10)*4)),((canvasWidth/10) + ((canvasWidth/10)*4))) canvas
                else do

                    drawSquares canvas gbValue (canvasHeight / numRows) (canvasWidth / numCols)
                    set' fillStyle (solidColor (RGBA 255 0 0 0.5)) canvas
                    fillRect ((canvasHeight/10),((canvasWidth/10))) ((canvasHeight/10)*8) ((canvasWidth/10)*8) canvas
                    set' fillStyle (htmlColor "white") canvas
                    set' textFont ((show (canvasWidth*0.1)) ++ "px Palatino") canvas
                    fillText "GAME OVER" (((canvasHeight/10) + ((canvasHeight/10)*4)),((canvasWidth/10) + ((canvasWidth/10)*4))) canvas
        return()

-- function to draw every square on the canvas
drawSquares :: Element -> GameBoard -> Double -> Double -> UI ()
drawSquares canvas gb squareWidth squareHeight = 
    sequence_  $ [drawSquare canvas gb squareWidth squareHeight (columnIndices, rowIndices) | columnIndices <- [0 .. GameBoard.width gb - 1], rowIndices <- [0 .. GameBoard.height gb - 1]]

-- function to draw a given square on a given board
drawSquare :: Element -> GameBoard -> Double -> Double -> (Int, Int) -> UI ()
drawSquare canvas gb squareWidth squareHeight (x, y) = do
    let
        exactX = (fromIntegral x * squareWidth)
        exactY = fromIntegral y * squareHeight
        digitPosX = exactX + squareWidth / 2
        digitPosY = exactY + squareHeight * 0.75
        squareValue = getValue gb (x, y)
        squareStatus = getStatus gb (x, y)
        colourAB = whichColour (x, y)
        in
            if squareStatus == visible then
                if squareValue == mine then
                    drawMine canvas (exactX, exactY) (digitPosX, digitPosY) squareWidth squareHeight
                else 
                    if squareValue == 0 then 
                        drawDigit canvas (exactX, exactY) (digitPosX, digitPosY) squareWidth squareHeight "" colourAB
                    else
                        drawDigit canvas (exactX, exactY) (digitPosX, digitPosY) squareWidth squareHeight (show squareValue) colourAB
            else if squareStatus == flagged then 
                drawFlag canvas (exactX, exactY) (digitPosX, digitPosY) squareWidth squareHeight colourAB
            else
                drawHidden canvas (exactX, exactY) (digitPosX, digitPosY) squareWidth squareHeight colourAB


-- function to draw a given digit on a given square on a given board
drawDigit :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> String -> Bool -> UI()
drawDigit canvas topLeftCoords centerCoords squareWidth squareHeight write colourA = do
    if colourA then do 
        set' fillStyle (htmlColor "#e6c29f") canvas
    else do 
        set' fillStyle (htmlColor "#d7b899") canvas
    fillRect topLeftCoords squareWidth squareHeight canvas
    case write of
        "1" -> set' fillStyle (htmlColor "blue") canvas
        "2" -> set' fillStyle (htmlColor "green") canvas
        "3" -> set' fillStyle (htmlColor "red") canvas
        "4" -> set' fillStyle (htmlColor "purple") canvas
        "5" -> set' fillStyle (htmlColor "black") canvas
        "6" -> set' fillStyle (htmlColor "gray") canvas
        "7" -> set' fillStyle (htmlColor "maroon") canvas
        "8" -> set' fillStyle (htmlColor "turquoise") canvas
        _ -> set' fillStyle (htmlColor "#ffbf80") canvas

    set' textFont ((show (squareWidth*0.75)) ++ "px Palatino") canvas
    fillText write centerCoords canvas

-- function to draw a mine on a given square on a given board
drawMine :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> UI()
drawMine canvas topLeftCoords centerCoords squareWidth squareHeight = do
    set' fillStyle (htmlColor "red") canvas
    fillRect topLeftCoords squareWidth squareHeight canvas
    set' fillStyle (htmlColor "white") canvas
    set' textFont ((show (squareWidth*0.75)) ++ "px Palatino") canvas
    fillText "X" centerCoords canvas

-- function to draw a flag on a given square on a given board
drawFlag :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> Bool -> UI()
drawFlag canvas (x, y) centerCoords squareWidth squareHeight colourA = do
    if colourA then do
        set' fillStyle (htmlColor "#a2d149") canvas
    else do 
        set' fillStyle (htmlColor "#a9d752") canvas
    fillRect (x, y) squareWidth squareHeight canvas
    set' fillStyle (htmlColor "#f23708") canvas
    fillRect ( x+ (squareWidth/10), y + (squareHeight/10)) (squareWidth - 2*(squareWidth/10)) (squareHeight - 5*(squareHeight/10)) canvas
    fillRect ( x+ (squareWidth/10), y + (squareHeight/10)) (squareWidth*0.10) (squareHeight - 2*(squareHeight/10)) canvas


-- function to draw a hidden square on a given square on a given board
drawHidden :: Element -> (Double, Double) -> (Double, Double) -> Double -> Double -> Bool -> UI()
drawHidden canvas topLeftCoords centerCoords squareWidth squareHeight colourA = do
    if colourA then do
        set' fillStyle (htmlColor "#a2d149") canvas
    else do 
        set' fillStyle (htmlColor "#a9d752") canvas
    fillRect topLeftCoords squareWidth squareHeight canvas


-- function to convert canvas coordinates to board coordinates
detectClickedSquare :: Point -> Double -> IO (Int, Int)
detectClickedSquare (x,y) squareSize = return (floor (x / squareSize),
                                        floor (y / squareSize))


-- canvas dimensions represented by functions to improve readability
canvasWidth, canvasHeight:: Double
canvasWidth = 500 :: Double
canvasHeight = 500 :: Double


-- function called to start the program and display three buttons to select difficulty
setup :: Window -> UI()
setup window = do
    return window # set title "Minesweeper"

    -- create the three option buttons
    easy <- button #+ [string "EASY"] 
        # set style styleDifficulty
    medium <- button #+ [string "MEDIUM"]
        # set style styleDifficulty
    hard <- button #+ [string "HARD"]
        # set style styleDifficulty


    getBody window #+
        [element easy, element medium, element hard]

    -- based on when a button is clicked and unclicked take a certain action

    on mousedown easy $ \_ -> do
        set' style styleDifficultySelected easy
        
    on mouseup easy $ \_ -> do
        set' style styleDifficulty easy
        delete easy >> delete medium >> delete hard
        play window 8 8 10

    on mousedown medium $ \_ -> do
        set' style styleDifficultySelected medium
        
    on mouseup medium $ \_ -> do
        set' style styleDifficulty medium
        delete easy >> delete medium >> delete hard
        play window 14 14 40

    on mousedown hard $ \_ -> do
        set' style styleDifficultySelected hard
        
    on mouseup hard $ \_ -> do
        set' style styleDifficulty hard
        delete easy >> delete medium >> delete hard
        play window 20 20 99



-- function to be used to play a game after the difficulty has been selected
play :: Window -> Double -> Double  -> Int -> UI ()
play window numRows numCols numMines = do


    -- create buttons to decide current mode and set both to unpressed
    mineButton <- button #+ [string "Dig"]
        # set style styleUnSelected
    flagButton <- button #+ [string "Flag"]
        # set style styleUnSelected 

    -- create button to perform an automatic move
    autoPlayButton <- button #+ [string "AUTO MOVE"] 
        # set style styleAutoPlay

    -- initialize the canvas element
    canvas <- canvas
        # set UI.height (floor canvasHeight)
        # set UI.width (floor canvasWidth)
        # set style [("border", "solid black 1px"), ("font-weight", "bold")]
        # set textAlign Center 

    -- initialize IORefs to hold a various useful attributes
    stdGen <- liftIO newStdGen
    gb <- liftIO $ newIORef $ createEmptyBoard (floor numRows) (floor numCols)
    mode <- liftIO $ newIORef UnCover 
    optionSelected <- liftIO $ newIORef False 
    pos <- liftIO $ newIORef (0, 0)
    gameStarted <- liftIO $ newIORef False

    
    -- create the canvas element
    gbValue <- liftIO $ readIORef gb
    drawCanvas canvas gbValue numRows numCols

    -- draw canvas and buttons
    getBody window #+
        [element canvas, row [element mineButton, element flagButton, element autoPlayButton]]

    -- change certain parameters based on which button is clicked
    on click mineButton $ \_ -> do
        liftIO $ writeIORef mode UnCover
        liftIO $ writeIORef optionSelected True 
        updateButtonStyles mineButton flagButton

    on click flagButton $ \_ -> do
        liftIO $ writeIORef mode Flag
        liftIO $ writeIORef optionSelected True
        updateButtonStyles flagButton mineButton

    -- update the position IORef if mouse is moved
    on mousemove canvas $ \xy -> do
        liftIO $ writeIORef pos xy

    -- code for when anywhere on the canvas is clicked
    on click canvas $ \_ -> do

        -- read the state of the IORefs
        (x, y) <- liftIO $ readIORef pos
        coords <- liftIO $ detectClickedSquare (x, y) (canvasHeight / numRows) 
        gbValue <- liftIO $ readIORef gb
        optionIsSelected <- liftIO $ readIORef optionSelected
        -- update variable if the game is still in play
        if(state gbValue == inPlay) then do
            -- read the state of the neccessary IORefs
            started <- liftIO $ readIORef gameStarted
            m <- liftIO $ readIORef mode
            gbValue <- liftIO $ readIORef gb

            -- if there is a mode selected i.e. uncover or flag mode then update the canvas
            if (optionIsSelected) then do
                case m of
                    UnCover  -> do
                        -- if its the users first turn, generate a board ensuring there is no mine here or anywhere adjacent
                        if (not started) then do 
                                liftIO $! writeIORef gb (updateState (uncover (plantMines (createEmptyBoard (floor numRows) (floor numCols)) numMines coords stdGen) coords))
                                liftIO $ writeIORef gameStarted True
                        else do
                            liftIO $ writeIORef gb (updateState (uncover gbValue coords))
                    Flag -> do
                        liftIO $ writeIORef gb (placeFlag gbValue coords)

                gbValue <- liftIO $ readIORef gb
                drawCanvas canvas gbValue numRows numCols
            else do
                drawCanvas canvas gbValue numRows numCols
        else do
            drawCanvas canvas gbValue numRows numCols

     -- when the autoplay button is clicked use the autoplayer function to update the board
    on mousedown autoPlayButton $ \_ -> do
        set' style styleSelectAutoPlay autoPlayButton

    on mouseup autoPlayButton $ \_ -> do
        set' style styleAutoPlay autoPlayButton
        gbValue <- liftIO $ readIORef gb
        if(state gbValue == inPlay) then do
            started <- liftIO $ readIORef gameStarted
            if started then do
                gbValue <- liftIO $ readIORef gb
                updatedBoard <- liftIO $ autoMove gbValue

                liftIO $ writeIORef gb updatedBoard

                gbValue <- liftIO $ readIORef gb
                updatedBoard <- liftIO $ return $ updateState gbValue

                liftIO $ writeIORef gb updatedBoard
                gbValue <- liftIO $ readIORef gb
                drawCanvas canvas gbValue numRows numCols
            else do
                return()
        else do 
            return()
