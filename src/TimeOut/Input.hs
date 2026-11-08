module TimeOut.Input where

import Graphics.Gloss.Interface.IO.Interact
import Data.Array
import Data.List
import Debug.Trace

import TimeOut.Dir
import TimeOut.Types
import TimeOut.Logic
import TimeOut.Levels

handleInput :: SpecialKey -> Maybe Dir
handleInput key = isDirKey key 

eventHandler :: Event -> World -> World
eventHandler (EventKey (SpecialKey key) Down _ _) world =
    trace ("eventHandler got: " ++ show key) $
    case handleInput key of
        Just dir -> trace ("handleInput → " ++ show dir) $ movePlayer dir world
        Nothing  -> trace ("handleInput → nothing ") $ world
eventHandler (EventKey (Char 'r') Down _ _) world =
    trace "eventHandler got: r (Reset)" $ 
    smallReset world
eventHandler _ world = world   

movePlayer :: Dir -> World -> World
movePlayer dir world =
    let (dx, dy) = unitVector dir
        (px, py) = playerPos (currentLevel world)
        newPos = (px + dx , py + dy)
        lvl = currentLevel world
        arr = levelArray lvl
    in trace ("withinRange got: " ++ show (withinRange arr newPos) ++ 
    ", newpos is: " ++ show(newPos)) $
        if withinRange arr newPos 
        then dealWithCell arr (px, py) (dx, dy) world
        else world

-- check next position's cell type 
cellType :: Array (Int, Int) Cell -> (Int, Int) -> Maybe Cell
cellType arr pos = Just (arr ! pos) 

-- check next position's in bound or not
withinRange :: Array (Int, Int) Cell -> (Int, Int) -> Bool
withinRange arr pos = 
    trace ("withinRange checking: " ++ show pos) $
    let boundsArr = bounds arr
        (minX, minY) = fst boundsArr
        (maxX, maxY) = snd boundsArr
    in minX <= maxX && minY <= maxY && 
       minX <= fst pos && fst pos <= maxX && 
       minY <= snd pos && snd pos <= maxY

-- check if next position is wall 
checkRangeAndObject :: Array (Int, Int) Cell -> (Int, Int) -> Bool
checkRangeAndObject arr pos = 
    withinRange arr pos && 
    case arr ! pos of
        Empty    -> True
        Button _ -> True  
        _        -> False 

-- deal with diff cell types
dealWithCell :: Array (Int, Int) Cell -> (Int, Int) -> (Int, Int) -> World -> World
dealWithCell arr (px, py) (dx, dy) world = 
    let newPos = (px + dx, py + dy)
    in case cellType arr newPos of 
        Just Wall -> world
        Just Empty -> 
            let currentLvl = currentLevel world
                currentDynamic = dynamicObject currentLvl
                newDynamic = currentDynamic { playerPosPos = newPos }
                newLvl = currentLvl { dynamicObject = newDynamic }
            in checkLevelProgress $ world { currentLevel = newLvl }
        Just Box -> moveBox (px, py) (dx, dy) world
        Just Crystal -> moveCrystal (px, py) (dx, dy) world 
        Just Goal -> 
            let currentLvl = currentLevel world
                currentDynamic = dynamicObject currentLvl
                newDynamic = currentDynamic { playerPosPos = newPos }
                newLvl = currentLvl { dynamicObject = newDynamic }
            in checkLevelProgress $ world { currentLevel = newLvl }
        Just (Button _) -> 
            let currentLvl = currentLevel world
                currentDynamic = dynamicObject currentLvl
                newDynamic = currentDynamic { playerPosPos = newPos }
                newLvl = currentLvl { dynamicObject = newDynamic }
            in checkLevelProgress $ world { currentLevel = newLvl }
        Just (Door id) -> checkButton id newPos world
        Nothing -> error "cellType returned Nothing"

-- move box and also player (now only can push 1 box) (box is static)
moveBox :: (Int, Int) -> (Int, Int) -> World -> World
moveBox (px,py) (dx,dy) world = 
    let boxNewPos = (px + 2 * dx , py + 2 * dy) 
        boxOldPos = (px + dx , py + dy)
        lvl = currentLevel world
        arr = levelArray lvl
    in trace ("withinRange got: " ++ show (withinRange arr boxNewPos) ++ 
    ", boxNewpos is: " ++ show(boxNewPos)) $
        if withinRange arr boxNewPos && 
            withinRange arr boxOldPos && 
            checkRangeAndObject arr boxNewPos
        then 
            let currentLvl = currentLevel world
                currentTerrain = terrain currentLvl
                currentDynamic = dynamicObject currentLvl
                newArray = terrainArrayPos currentTerrain // [(boxNewPos, Box), (boxOldPos, Empty)]
                newTerrain = currentTerrain { terrainArrayPos = newArray }
                newDynamic = currentDynamic { playerPosPos = boxOldPos }
                newLvl = currentLvl { terrain = newTerrain, dynamicObject = newDynamic }
            in checkLevelProgress $ world { currentLevel = newLvl }
        else world

-- move crystal and player (now 1 crystal) (crystal is dynamic)
moveCrystal :: (Int, Int) -> (Int, Int) -> World -> World
moveCrystal (px,py) (dx,dy) world = 
    let crystalNewPos = (px + 2 * dx , py + 2 * dy) 
        crystalOldPos = (px + dx , py + dy)
        lvl = currentLevel world
        arr = levelArray lvl
    in trace ("withinRange got: " ++ show (withinRange arr crystalNewPos) ++ 
    ", crystalNewpos is: " ++ show(crystalNewPos)) $
        if withinRange arr crystalNewPos && 
            withinRange arr crystalOldPos && 
            checkRangeAndObject arr crystalNewPos
        then 
            let currentLvl = currentLevel world
                currentDynamic = dynamicObject currentLvl
                newDynamic = currentDynamic { playerPosPos = crystalOldPos, 
                    objectsPos = updateCrystals (objectsPos currentDynamic) crystalOldPos crystalNewPos }
                newLvl = currentLvl { dynamicObject = newDynamic }
            in checkLevelProgress $ world { currentLevel = newLvl }
        else world
        
updateCrystals :: [((Int, Int), Cell)] -> (Int, Int) -> (Int, Int) -> [((Int, Int), Cell)]
updateCrystals entries crystalOldPos crystalNewPos = 
    map updateEntry entries
  where
    updateEntry (pos, cell)
        | pos == crystalOldPos = (crystalNewPos, cell) 
        | otherwise            = (pos, cell)           

checkButton :: Int -> (Int,Int) -> World -> World
checkButton id newPos world = 
    let lvl = currentLevel world 
        arr = terrainArrayPos (terrain lvl) 
        total = map fst (objectsPos (dynamicObject lvl)) ++ 
            [playerPosPos (dynamicObject lvl)]
    in case findButtonById id arr of 
        Nothing -> world -- Safely do nothing if the button doesn't exist
        Just buttonPos ->
            if isColliding total buttonPos 
            then 
                let currentLvl = currentLevel world
                    currentDynamic = dynamicObject currentLvl
                    newDynamic = currentDynamic { playerPosPos = newPos }
                    newLvl = currentLvl { dynamicObject = newDynamic }
                in checkLevelProgress $ world { currentLevel = newLvl }
        else world

isColliding :: [(Int, Int)] -> (Int, Int) -> Bool
isColliding total buttonPos = 
    any (\(pos) -> pos == buttonPos) total

findButtonById :: Int -> Array (Int, Int) Cell -> Maybe (Int, Int)
findButtonById targetId arr = 
    let match = find (\(pos, cell) -> cell == Button targetId) (assocs arr)
    in fmap fst match

smallReset :: World -> World 
smallReset world = 
    let lvl = currentLevel world
        static = terrain (allLevels !! (currentLevelIndex world))
        dynamic = dynamicObject (allLevels !! (currentLevelIndex world))
        reset = lvl {terrain = static, dynamicObject = dynamic}
    in world { currentLevel = reset, timer = initialTimer }