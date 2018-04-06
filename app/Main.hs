module Main where

import Life

main :: IO ()
main = do
    file <- getLine
    points <- readFile file
    run (initGame points)
