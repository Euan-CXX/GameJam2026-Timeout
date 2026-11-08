module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.Trans.Except

import TimeOut.Types
import TimeOut.Levels
import TimeOut.Render
import TimeOut.Input
import TimeOut.Dir
import TimeOut.Logic

window = InWindow "TimeOut" (800, 600) (100, 100)


-- Main Entry Point
main :: IO ()
main = play
    window                                          -- Window title and size
    black                                           -- Background color
    60                                              -- Frames per second
    initialWorld                                    -- Initial state
    render                                          -- Draw function
    eventHandler                                    -- Event handler
    update                                          -- Update function