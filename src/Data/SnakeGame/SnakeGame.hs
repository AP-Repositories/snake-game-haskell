{-# LANGUAGE TemplateHaskell
    ,LambdaCase
    ,BlockArguments
    ,MagicHash
    ,UnboxedTuples
    ,ScopedTypeVariables
    ,RecordWildCards
    ,TupleSections #-}
module Data.SnakeGame.SnakeGame
    ( runSnakeGame
    , getTile
    , getScore
    , Rows
    , Cols
    , FoodCount
    , SnakeLength
    , GameInput
    , SnakeTile(..)
    , SnakeDir(..)
    ) where
import Control.Lens.TH(makeLenses)
import Control.Lens (use, uses, (%=), (.=), (+=))
import Control.Monad.Trans.State (state, evalStateT, execState, get, modify
    ,execStateT)
import Data.IntMap.Strict (delete, insert, empty, (!?))
import Control.Monad (replicateM_, when)
import Data.Maybe (fromMaybe)
import System.Random (mkStdGen, genWord32)
import Control.Arrow (first)
import Control.Monad.Trans.Class (lift)
import Data.Function (on)
import Data.IntMap (IntMap)

data SnakeDir  = UpD | RightD | DownD | LeftD deriving (Enum, Show)
data SnakeTile = SnakeEmpty | SnakeFood | SnakeUp | SnakeRight | SnakeDown | SnakeLeft deriving (Enum, Show)
data SnakeGame = SnakeGame
    {_rows, _cols, _tailCol, _tailRow, _headCol, _headRow, _score :: Int
    ,_board                                                       :: IntMap SnakeTile
    ,_living                                                      :: Bool} deriving Show

$(makeLenses ''SnakeGame)

type Rows        = Int
type Cols        = Int
type FoodCount   = Int
type SnakeLength = Int
type GameInput m = SnakeGame -> m SnakeDir

getTile :: SnakeGame -> Cols -> Rows -> SnakeTile
getTile SnakeGame{..} col row = if onHead then SnakeDown else tile
  where
    onHead = col == _headCol && row == _headRow
    tile   = fromMaybe SnakeEmpty (_board !? (row*_cols + col))

getScore :: SnakeGame -> FoodCount
getScore SnakeGame{..} = _score

runSnakeGame :: forall m. Monad m => Cols -> Rows -> FoodCount ->
  SnakeLength -> GameInput m -> m (Maybe SnakeGame)
runSnakeGame width height foodCount snakeLength input =
    if isValid then
        Just<$>execStateT gameLoop initial
    else
        pure Nothing
  where
    wrapH x
      | x < 0       = width - 1
      | width <= x  = 0
      | otherwise   = x
    wrapV x
      | x < 0       = height - 1
      | height <= x = 0
      | otherwise   = x
    dec x = x - 1
    inc x = x + 1
    initial = execState (replicateM_ foodCount placeFood) SnakeGame
        {_cols    = width
        ,_rows    = height
        ,_tailCol = tCol
        ,_tailRow = tRow
        ,_headCol = hCol
        ,_headRow = hRow
        ,_score   = 0
        ,_living  = True
        ,_board   = snakeBody}
    gameLoop = use living >>= (`when` do
        gameState <- get
        lift (input gameState) >>= modify . tick
        gameLoop)
    isValid = (snakeLength < width || snakeLength < height) &&
        snakeLength + foodCount < width * height
    (# tCol, tRow, hCol, hRow #) =
        if snakeLength < width then let
                midHeight = height `div` 2
                sideLen   = (width - snakeLength) `div` 2
            in (# sideLen, midHeight, width - sideLen, midHeight #)
        else let
                midDepth = width `div` 2
                sideLen  = (height - snakeLength) `div` 2
            in (# midDepth, sideLen, midDepth, height - sideLen #)
    snakeBody = foldr placeBody id [0..snakeLength] empty
      where
        placeBody = (.). uncurry insert . if snakeLength < width
            then (,SnakeRight) . (tRow*width + tCol +)
            else (,SnakeDown)  . (hCol +) . (* width)
    flat col row = row*width + col
    placeFood = seed >>= evalStateT loop
      where
        seed = mkStdGen . foldr (\x y -> x + y * 31) 0<$>
            sequence
                [use tailCol, use tailRow, use headCol, use headRow
                ,use rows, use cols, use score]
        rand x = state$ first ((`mod` x).fromIntegral).genWord32
        loop = do
            col <- lift (use cols) >>= rand
            row <- lift (use rows) >>= rand
            let coord = flat col row
            tailCoord <- lift (flat<$>use tailCol<*>use tailRow)
            headCoord <- lift (flat<$>use headCol<*>use headRow)
            if on (||) (== coord) tailCoord headCoord then loop else do
                oldTile <- lift$ uses board (fromMaybe SnakeEmpty . (!? coord))
                case oldTile of
                    SnakeEmpty -> lift$ board %= insert coord SnakeFood
                    _          -> loop
    tick dir = execState$ pushHead >>= \case
        SnakeEmpty -> followHead
        SnakeFood  -> do
            placeFood
            score += 1
        _          -> do
            coord <- flat<$>use headCol<*>use headRow
            followHead
            uses board (fromMaybe SnakeEmpty . (!? coord)) >>= \case
                SnakeEmpty -> pure ()
                _          -> living .= False
      where
        followHead = do
            coord <- flat<$>use tailCol<*>use tailRow
            next  <- uses board (fromMaybe SnakeEmpty . (!? coord))
            board %= delete coord
            case next of
                SnakeUp    -> tailRow %= (wrapV . dec)
                SnakeRight -> tailCol %= (wrapH . inc)
                SnakeDown  -> tailRow %= (wrapV . inc)
                SnakeLeft  -> tailCol %= (wrapH . dec)
                _          -> do
                    game     <- get
                    position <- (,)<$>use tailCol<*>use tailRow
                    error$
                        " Expected snake direction body at "++show position++
                        ", found "++show next++" instead. \n"++show game
        pushHead = do
            oldCoord <- flat<$>use headCol<*>use headRow
            board %= insert oldCoord (toEnum (fromEnum SnakeUp + fromEnum dir))
            case dir of
                UpD    -> headRow %= (wrapV . dec)
                RightD -> headCol %= (wrapH . inc)
                DownD  -> headRow %= (wrapV . inc)
                LeftD  -> headCol %= (wrapH . dec)
            newCoord <- flat<$>use headCol<*>use headRow
            uses board (fromMaybe SnakeEmpty . (!? newCoord))