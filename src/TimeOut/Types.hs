module TimeOut.Types where

import Data.Array

data Cell = Wall | Empty | Player | Goal |
     Box | Crystal | Button Int | Door Int deriving (Show, Eq) 

data Level = Level { 
    terrain :: Terrain,
    dynamicObject :: DynamicObject,
    levelHint :: String
} deriving (Show, Eq)

data Terrain = Terrain { 
    terrainArrayPos :: Array (Int, Int) Cell,
    startPos :: (Int, Int),
    goalPos :: (Int, Int)
} deriving (Show, Eq)

data DynamicObject = DynamicObject { 
    objectsPos :: [((Int, Int), Cell)],
    playerPosPos :: (Int, Int)
} deriving (Show, Eq)

-- static + dynamic, dynamic priority higher 
levelArray :: Level -> Array (Int, Int) Cell
levelArray level = terrainArrayPos (terrain level) // objectsPos (dynamicObject level)

start :: Level -> (Int, Int) 
start level = startPos (terrain level)

playerPos :: Level -> (Int, Int)
playerPos level = playerPosPos (dynamicObject level)

goal :: Level -> (Int, Int)
goal level = goalPos (terrain level)

data GameState = Running | LevelComplete | GameWon deriving (Show, Eq)

data World = World {
    currentLevelIndex :: Int,
    currentLevel :: Level,
    gameState :: GameState,
    timer :: Timer
} deriving (Show, Eq)

cellSize :: Float
cellSize = 50.0

type Timer = Float 
initialTimer :: Timer
initialTimer = 5.0

