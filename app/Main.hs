module Main where

import Lib

main :: IO ()
main = do
    game <- initialize
    mainLoop game
    cleanup game
