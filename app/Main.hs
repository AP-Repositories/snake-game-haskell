module Main (main) where

import System.IO (hSetBuffering, stdin, BufferMode (LineBuffering))
import Control.SnakeGame.SnakeGamePrinter (input)
import Data.SnakeGame.SnakeGame (runSnakeGame, getScore)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Data.Time (secondsToNominalDiffTime)
import Text.Printf (printf)

main :: IO ()
main = do 
    hSetBuffering stdin LineBuffering
    putStrLn "Welcome to snake game. Press enter to continue..."
    _          <- getLine
    snakeInput <- input cols rows delay
    game       <- runSnakeGame cols rows foodCount snakeLength snakeInput 
    clearScreen
    setCursorPosition 0 0 
    case game of
        Just g -> do 
            putStrLn "Thanks for playing!" 
            printf "Final score: %d \n" (getScore g)
        _      -> putStrLn "Something went wrong while initializing the game."
  where
    rows        = 20
    cols        = 20
    snakeLength = 10
    foodCount   = 20
    delay       = secondsToNominalDiffTime 0.1
