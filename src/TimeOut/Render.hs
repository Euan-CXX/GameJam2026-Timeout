module TimeOut.Render where

import Graphics.Gloss
import Data.Array
import Debug.Trace

import TimeOut.Types
import TimeOut.Logic
import TimeOut.Input

getOffset :: Array (Int, Int) Cell -> (Float, Float)
getOffset arr = 
    let ((minX, minY), (maxX, maxY)) = bounds arr
        offsetX = - (fromIntegral (maxX + minX) / 2.0) * cellSize
        offsetY = - (fromIntegral (maxY + minY) / 2.0) * cellSize
    in (offsetX, offsetY)

render :: World -> Picture
render world = 
    let arr = levelArray (currentLevel world)
        (offsetX, offsetY) = getOffset arr
    in case gameState world of
        Running -> 
            pictures [ renderLevel (currentLevel world), 
                       renderPlayer offsetX offsetY (playerPos (currentLevel world)), 
                       rendergoal offsetX offsetY (goal (currentLevel world)),
                       drawTimer (timer world), 
                       drawLevelHint (levelHint (currentLevel world)) ]
        LevelComplete -> translate 0 0 $ scale 0.4 0.4 $ text "Level Complete!"
        GameWon -> translate 0 0 $ scale 0.4 0.4 $ text "You Won!"

-- world and level rendering
renderLevel :: Level -> Picture
renderLevel level = 
    let arr = levelArray level
        (offsetX, offsetY) = getOffset arr
    in translate offsetX offsetY $ 
       pictures [ renderCell arr (i, j) | (i, j) <- range (bounds arr) ]

renderCell :: Array (Int, Int) Cell -> (Int, Int) -> Picture
renderCell arr (i, j) =
    let cell = arr ! (i, j)
        cellColor = case cell of
            Wall -> black
            Empty -> white
            Box -> orange
            Crystal -> magenta
            (Button _) -> greyN 0.5 
            (Door _) -> greyN 0.5 
        label = case cell of 
            Wall -> "wall"
            Empty -> ""
            Box -> "Box"
            Crystal -> "C"
            (Button id) -> "Btn " ++ show(id)
            (Door id) -> "D " ++ show(id)
        background = color cellColor $ rectangleSolid cellSize cellSize 
        textPicture = color black $ scale 0.07 0.07 $ text label
        centeredText = translate (cellSize / 4) (-cellSize / 4) textPicture
    in translate (fromIntegral i * cellSize) (fromIntegral j * cellSize) $ 
        pictures [background, centeredText] 

renderPlayer :: Float -> Float -> (Int, Int) -> Picture
renderPlayer offsetX offsetY (x, y) = 
    translate offsetX offsetY $ 
    translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ 
    color blue $ rectangleSolid cellSize cellSize

rendergoal :: Float -> Float -> (Int, Int) -> Picture
rendergoal offsetX offsetY (x, y) = 
    translate offsetX offsetY $ 
    translate (fromIntegral x * cellSize) (fromIntegral y * cellSize) $ 
    color green $ rectangleSolid cellSize cellSize

-- timer rendering 
drawTimer :: Timer -> Picture
drawTimer t = 
    let timerText = show (ceiling t :: Int)
        background = color white $ rectangleSolid 120 60
        textPicture = color black $ scale 0.2 0.2 $ text timerText
        centeredText = translate (-20) (-15) textPicture
        timerUI = pictures [background, centeredText]
    in translate 0 250 timerUI -- Moves the whole UI to the top center

drawLevelHint :: String -> Picture
drawLevelHint hint = 
    let
        textPic = color black $ scale 0.12 0.12 $ text hint
        boxWidth = fromIntegral (length hint) * 12.0 + 40.0 
        background = color white $ rectangleSolid boxWidth 40
        centeredText = translate (-(fromIntegral (length hint) * 6.0)) (-6) textPic
    in translate 0 (-250) $ pictures [background, centeredText]