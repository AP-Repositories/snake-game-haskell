{-# LANGUAGE BlockArguments #-}
module Control.SnakeGame.SnakeGamePrinter
    (input
    ) where
import Control.Monad (forM_, when)
import System.Console.ANSI (setCursorPosition, clearScreen)
import Data.SnakeGame.SnakeGame (getTile, getScore, GameInput, 
    SnakeTile (SnakeEmpty, SnakeFood), SnakeDir (UpD, DownD, RightD, LeftD))
import Data.IORef (newIORef, writeIORef, readIORef)
import System.IO (hReady, stdin, hFlush, stdout, 
    BufferMode (NoBuffering), hSetBuffering)
import Data.Time (NominalDiffTime)
import Data.Functor (($>))
import Data.Function (fix)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Text.Printf (printf)


input :: Int -> Int -> NominalDiffTime -> IO (GameInput IO)
input width height delay = do 
    hSetBuffering stdin NoBuffering
    key'   <- newIORef RightD
    first' <- newIORef True
    pure$ \game -> readIORef first' >>= \first -> if first
        then do 
            writeIORef first' False
            draw game
            fix \loop -> getKey >>= \key -> if isDir key
                then writeIORef key' (toDir key) $> toDir key
                else loop
        else do
            draw game
            child <- forkIO$ fix \loop -> do 
                hReady stdin >>= flip when do
                    key <- getKey
                    when (isDir key) do
                        writeIORef key' (toDir key)
                loop        
            threadDelay (fromEnum delay `div` (1000*1000))
            killThread child
            readIORef key'
  where
    toDir key = case key of
        "\ESC[A" -> UpD
        "\ESC[B" -> DownD
        "\ESC[C" -> RightD
        "\ESC[D" -> LeftD
        _        -> error "impossible"
    isDir key = key `elem` ["\ESC[A","\ESC[B","\ESC[C","\ESC[D"]
    getKey = reverse <$> getKey' ""
      where 
        getKey' chars = do
            char <- getChar
            more <- hReady stdin
            (if more then getKey' else return) (char:chars)
    draw game = do 
        clearScreen
        forM_ [0..height - 1] \y -> do
            setCursorPosition y 0
            putStr$ [0..width - 1] >>= \x -> case getTile game x y of
                SnakeEmpty -> "▁▁"
                SnakeFood  -> "▓▓"
                _          -> "░░"
        setCursorPosition height 0
        printf "SCORE: %d " (getScore game)
        hFlush stdout