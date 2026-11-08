module TimeOut.Logic where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

import TimeOut.Types
import TimeOut.Levels
import Debug.Trace

-- updating world and level
initialWorld :: World
initialWorld = World {
    currentLevelIndex = 0,
    currentLevel = allLevels !! 0,
    gameState = Running,
    timer = initialTimer
}

checkLevelProgress :: World -> World
checkLevelProgress world =
    trace ("checkLevelProgress got: " ++ show (gameState world)) $
    let lvl = currentLevel world
    in if playerPos lvl == goal lvl
        then
            let nextIndex = currentLevelIndex world + 1
            in if nextIndex >= length allLevels
                then world { gameState = GameWon }
                else checkLevelProgress world { 
                    currentLevelIndex = nextIndex,
                    currentLevel = allLevels !! nextIndex,
                    gameState = LevelComplete 
                }
        else 
            if (gameState world) == LevelComplete 
                then world { gameState = Running }
                else 
        world

-- timer
-- update timer = 0 update 
update :: Float -> World -> World
update dt world 
    | timer world > 0 = world { timer = timer world - dt } 
    | otherwise = updatePlayerPosition world

-- update timer and update world
-- let terrain reset, leave dynamic alone
updatePlayerPosition :: World -> World
updatePlayerPosition world = 
    let lvl = currentLevel world
        static = terrain (allLevels !! (currentLevelIndex world))
        -- dynamic = dynamicObject lvl
        resetStatic = lvl {terrain = static}
    in world { currentLevel = resetStatic, timer = initialTimer }
