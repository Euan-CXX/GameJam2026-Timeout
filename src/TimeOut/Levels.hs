module TimeOut.Levels where

import TimeOut.Types
import Data.Array

makeEmptyGrid :: Int -> Array (Int, Int) Cell
makeEmptyGrid size = array ((0,0), (size, size)) [((i,j), Empty) | i <- [0..size], j <- [0..size]]

turnIntoWall :: Array (Int, Int) Cell -> [(Int, Int)] -> Array (Int, Int) Cell
turnIntoWall arr positions = arr // [ (pos, Wall) | pos <- positions ]

turnIntoBox :: Array (Int, Int) Cell -> [(Int, Int)] -> Array (Int, Int) Cell
turnIntoBox arr positions = arr // [ (pos, Box) | pos <- positions ]

turnInto :: Array (Int, Int) Cell -> Cell -> (Int, Int) -> Array (Int, Int) Cell 
turnInto arr cell (x, y) = arr // [((x, y), cell)]

-- turnIntoCrystal :: Array (Int, Int) Cell -> (Int, Int) -> Array (Int, Int) Cell
-- turnIntoCrystal arr (x, y) = arr // [((x, y), Crystal)]

turnIntoButton :: Array (Int, Int) Cell -> (Int, Int) -> Int -> Array (Int, Int) Cell
turnIntoButton arr (x, y) id = arr // [((x, y), Button id)]

turnIntoDoor :: Array (Int, Int) Cell -> (Int, Int) -> Int -> Array (Int, Int) Cell
turnIntoDoor arr (x, y) id = arr // [((x, y), Door id)]

level1 :: Level
level1 = Level {
    terrain = Terrain {
        terrainArrayPos = array ((0,0), (3,0)) 
            [ ((x, y), Empty) | x <- [0..3], y <- [0..0] ],
        startPos = (0, 0),
        goalPos = (3, 0)
    },
    dynamicObject = DynamicObject {
        objectsPos = [],
        playerPosPos = (0, 0)
    },
    levelHint = "use arrow keys to move"
}

level2 :: Level
level2 = Level {
    terrain = Terrain {
        terrainArrayPos = turnIntoWall
            (turnIntoBox 
            (array ((0,0), (3,1)) [ ((x, y), Empty) | x <- [0..3], y <- [0..1] ])
            [(1,0), (1,1)])
            [(3,1)]
        ,
        startPos = (0, 1),
        goalPos = (3, 0)
    },
    dynamicObject = DynamicObject {
        objectsPos = [],
        playerPosPos = (0, 0)
    },
    levelHint = "Boxes resets"
}

level2test :: Level
level2test = Level {
    terrain = Terrain {
        terrainArrayPos = turnIntoBox (array ((0,0), (3,0)) 
            [ ((x, y), Empty) | x <- [0..3], y <- [0..0] ]) [(2,0)]
            ,
        startPos = (0, 0),
        goalPos = (3, 0)
    },
    dynamicObject = DynamicObject {
        objectsPos = [],
        playerPosPos = (0, 0)
    },
    levelHint = "youre stuck?"
}

level3 :: Level
level3 = Level {
    terrain = Terrain {
        terrainArrayPos = array ((0,0), (3,1)) 
            [ ((x, y), Empty) | x <- [0..3], y <- [0..1] ],
        startPos = (0, 1),
        goalPos = (3, 0)
    },
    dynamicObject = DynamicObject {
        objectsPos = [((1,0), Crystal), ((1,1), Crystal)],
        playerPosPos = (0, 0)
    },
    levelHint = "Crystals stays, press r to reset"
}

level4 :: Level
level4 = Level {
    terrain = Terrain {
       terrainArrayPos = array ((0,0), (3,1)) 
            [ ((0, 0), Empty), 
                ((1, 0), Empty), 
                ((2, 0), Door 1), 
                ((3, 0), Empty), 
                ((0, 1), Empty), 
                ((1, 1), Empty), 
                ((2, 1), Button 1), 
                ((3, 1), Wall) 
            ],
        startPos = (0, 0),
        goalPos = (3, 0)
    },
    dynamicObject = DynamicObject {
        objectsPos = [((1,1), Crystal)],
        playerPosPos = (0, 0)
    },
    levelHint = "Buttons and Doors (render tbc)"
}

level5 :: Level
level5 = Level {
    terrain = Terrain {
       terrainArrayPos = array ((0,0), (3,2)) 
            [ ((0, 0), Empty), 
                ((1, 0), Empty), 
                ((2, 0), Wall), 
                ((3, 0), Empty), 
                ((0, 1), Empty), 
                ((1, 1), Empty), 
                ((2, 1), Door 1), 
                ((3, 1), Empty), 
                ((0, 2), Empty), 
                ((1, 2), Box), 
                ((2, 2), Button 1), 
                ((3, 2), Wall)
            ],
        startPos = (0, 0),
        goalPos = (3, 1)
    },
    dynamicObject = DynamicObject {
        objectsPos = [((1,1), Crystal)],
        playerPosPos = (0, 0)
    },
    levelHint = "Only Crystal can press button, try overlapping"
}

allLevels :: [Level]
allLevels = [level1, level2, level2test, level3, level4, level5]