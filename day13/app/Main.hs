{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (HashMap, (!))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.Index as Index
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Debug.Trace

{-|

It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is all
the way on the other end of the ship. Surely, it won't be hard to build your
own - the care package even comes with schematics.

The arcade cabinet runs Intcode software like the game the Elves sent (your
puzzle input). It has a primitive screen capable of drawing square tiles on a
grid. The software draws tiles to the screen with output instructions: every
three output instructions specify the x position (distance from the left), y
position (distance from the top), and tile id. The tile id is interpreted as
follows:

- 0 is an empty tile. No game object appears in this tile.
- 1 is a wall tile. Walls are indestructible barriers.
- 2 is a block tile. Blocks can be broken by the ball.
- 3 is a horizontal paddle tile. The paddle is indestructible.
- 4 is a ball tile. The ball moves diagonally and bounces off objects.

For example, a sequence of output values like 1,2,3,6,5,4 would draw a
horizontal paddle tile (1 tile from the left and 2 tiles from the top) and a
ball tile (6 tiles from the left and 5 tiles from the top).

Start the game. How many block tiles are on the screen when the game exits?

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory =
        HashMap.fromList $
        Index.indexed $ map parseNumber $ T.splitOn "," contents
  putStrLn "Part One"
  let program = buildProgram memory
  let result = runProgram [] program
  let gameState = updateState (reverse $ _output $ result) initialState
  print $ length $ filter (== Block) $ HashMap.elems $ _cells gameState
  printState gameState
  let newProgram = buildProgram (HashMap.insert 0 2 memory)
  let newState = updateState (reverse $ _output $ result) initialState
  putStrLn "Part Two"
  (finalProgram, finalState) <- playGame 0 newProgram newState
  print (_score finalState)

playGame :: Int -> Program -> State -> IO (Program, State)
playGame count program gameState =
  case noBlocksLeft newState of
    True -> pure (newProgram, newState)
    False -> do
      if count `mod` 100 == 0
        then printState newState
        else pure ()
      playGame (count + 1) newProgram newState
  where
    noBlocksLeft s =
      List.null $ filter (\(k, v) -> v == Block) $ HashMap.toList $ _cells s
    newProgram = runProgram [fromIntegral findMove] program
    newState = updateState (reverse $ _output $ newProgram) gameState
    findPosition tile =
      List.find (\(k, v) -> v == tile) $ HashMap.toList (_cells gameState)
    findMove =
      case (findPosition Ball, findPosition Paddle) of
        (Just ((ballX, _), _), Just ((paddleX, _), _)) -> offset ballX paddleX
        _ -> 0
    offset ballX paddleX
      | ballX > paddleX = 1
      | ballX < paddleX = -1
      | otherwise = 0

type Point = (Int, Int)

data State =
  State
    { _cells :: HashMap Point Tile
    , _score :: Int
    }
  deriving (Eq, Show)

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Eq, Show)

prettify :: Tile -> Char
prettify Empty = ' '
prettify Wall = '#'
prettify Block = '='
prettify Paddle = '~'
prettify Ball = '@'

printState :: State -> IO ()
printState state = do
  putStrLn $ "\nScore: " <> show (_score state)
  sequence $ printLines
  pure ()
  where
    cells = _cells state
    points = HashMap.keys cells
    xs = map fst points
    ys = map snd points
    (maxX, minX) = (maximum xs, minimum xs)
    (maxY, minY) = (maximum ys, minimum ys)
    xRange = [minX .. maxX]
    yRange = [minY .. maxY]
    printLines =
      map
        (\y ->
           putStrLn $
           (map
              (\x -> prettify $ HashMap.lookupDefault Empty (x, y) cells)
              xRange))
        yRange

parseTile :: Value -> Tile
parseTile 1 = Wall
parseTile 2 = Block
parseTile 3 = Paddle
parseTile 4 = Ball
parseTile _ = Empty

initialState :: State
initialState = State {_cells = HashMap.empty, _score = 0}

updateState :: [Value] -> State -> State
updateState output state =
  foldl
    (\acc elem ->
       case elem of
         [-1, 0, score] -> acc {_score = fromIntegral score}
         [x, y, value] ->
           acc
             { _cells =
                 HashMap.insert
                   (fromIntegral x, fromIntegral y)
                   (parseTile value)
                   (_cells acc)
             }
         _ -> acc)
    state
    (Split.chunksOf 3 output)

buildProgram :: Memory -> Program
buildProgram memory =
  Program
    { _ip = 0
    , _relativeBase = 0
    , _memory = memory
    , _output = []
    , _returnValue = Nothing
    }

type Value = Integer

type Memory = HashMap Int Value

data Program =
  Program
    { _memory :: Memory
    , _relativeBase :: Int
    , _ip :: Int
    , _output :: [Value]
    , _returnValue :: Maybe Int
    }
  deriving (Show)

addOutput :: Value -> Program -> Program
addOutput value program@(Program {_output = output}) =
  program {_output = value : output}

trace :: Show a => T.Text -> a -> a
trace message value =
  Debug.Trace.trace (T.unpack (message <> " " <> T.pack (show value))) value

data PMode
  = Position
  | Immediate
  | Relative
  deriving (Eq, Show)

data Instruction
  = Add PMode PMode PMode
  | Multiply PMode PMode PMode
  | Halt
  | StoreInput PMode
  | Output PMode
  | JumpIfTrue PMode PMode
  | JumpIfFalse PMode PMode
  | LessThan PMode PMode PMode
  | Equals PMode PMode PMode
  | AdjustRelative PMode
  deriving (Eq, Show)

parseOpcode :: Value -> Instruction
parseOpcode opcode =
  let twoDigitOpcode = opcode `mod` 100
      parseParameterMode 0 = Position
      parseParameterMode 1 = Immediate
      parseParameterMode 2 = Relative
      p1 = parseParameterMode $ opcode `div` 100 `mod` 10
      p2 = parseParameterMode $ opcode `div` 1000 `mod` 10
      p3 = parseParameterMode $ opcode `div` 10000 `mod` 10
   in case twoDigitOpcode of
        1 -> Add p1 p2 p3
        2 -> Multiply p1 p2 p3
        3 -> StoreInput p1
        4 -> Output p1
        5 -> JumpIfTrue p1 p2
        6 -> JumpIfFalse p1 p2
        7 -> LessThan p1 p2 p3
        8 -> Equals p1 p2 p3
        9 -> AdjustRelative p1
        99 -> Halt

runProgramWithOutput :: [Value] -> Program -> IO ()
runProgramWithOutput input program =
  putStrLn $
  List.intercalate "\n" $
  map show $ reverse $ _output $ runProgram input program

runProgram :: [Value] -> Program -> Program
runProgram input program =
  let memory = _memory program
      ip = _ip program
      opcode = memory ! ip
      instruction = parseOpcode opcode
   in case instruction of
        Halt -> program {_returnValue = Just 0}
        Add p1 p2 p3 ->
          runProgram input $
          program
            { _ip = ip + 4
            , _memory = add (p1, ip + 1) (p2, ip + 2) (p3, ip + 3) program
            }
        Multiply p1 p2 p3 ->
          runProgram input $
          program
            { _ip = ip + 4
            , _memory = multiply (p1, ip + 1) (p2, ip + 2) (p3, ip + 3) program
            }
        StoreInput p1 ->
          case input of
            first:rest ->
              runProgram rest $
              program
                {_ip = ip + 2, _memory = writeValue first (p1, ip + 1) program}
            _ -> program
        Output p1 ->
          let value = readValue (p1, ip + 1) program
              newProgram = addOutput value program
           in runProgram input $ newProgram {_ip = ip + 2}
        JumpIfTrue p1 p2 ->
          case readValue (p1, ip + 1) program of
            0 -> runProgram input $ program {_ip = ip + 3}
            _ ->
              runProgram input $
              program {_ip = fromInteger (readValue (p2, ip + 2) program)}
        JumpIfFalse p1 p2 ->
          case readValue (p1, ip + 1) program of
            0 ->
              runProgram input $
              program {_ip = fromInteger (readValue (p2, ip + 2) program)}
            _ -> runProgram input $ program {_ip = ip + 3}
        LessThan p1 p2 p3 ->
          let value1 = readValue (p1, ip + 1) program
              value2 = readValue (p2, ip + 2) program
              result =
                if value1 < value2
                  then 1
                  else 0
           in runProgram input $
              program
                {_ip = ip + 4, _memory = writeValue result (p3, ip + 3) program}
        Equals p1 p2 p3 ->
          let value1 = readValue (p1, ip + 1) program
              value2 = readValue (p2, ip + 2) program
              result =
                if value1 == value2
                  then 1
                  else 0
           in runProgram input $
              program
                {_ip = ip + 4, _memory = writeValue result (p3, ip + 3) program}
        AdjustRelative p1 ->
          let value = fromInteger $ readValue (p1, ip + 1) program
              relativeBase = _relativeBase program
           in runProgram input $
              program {_ip = ip + 2, _relativeBase = relativeBase + value}

getValue :: Int -> Memory -> Value
getValue index memory = HashMap.lookupDefault 0 index memory

readValue :: (PMode, Int) -> Program -> Value
readValue (Immediate, index) (Program {_memory = memory}) =
  getValue index memory
readValue (Position, index) (Program {_memory = memory}) =
  getValue (fromInteger (getValue index memory)) memory
readValue (Relative, index) (Program { _memory = memory
                                     , _relativeBase = relativeBase
                                     }) =
  getValue (relativeBase + fromInteger (getValue index memory)) memory

writeValue :: Value -> (PMode, Int) -> Program -> Memory
writeValue value (Immediate, index) (Program {_memory = memory}) =
  trace "writeValue in Immediate mode" memory
writeValue value (Position, index) (Program {_memory = memory}) =
  HashMap.insert (fromInteger $ getValue index memory) value memory
writeValue value (Relative, index) (Program { _memory = memory
                                            , _relativeBase = relativeBase
                                            }) =
  HashMap.insert
    (relativeBase + fromInteger (getValue index memory))
    value
    memory

add :: (PMode, Int) -> (PMode, Int) -> (PMode, Int) -> Program -> Memory
add first second writeTo program = writeValue result writeTo program
  where
    firstValue = readValue first program
    secondValue = readValue second program
    result = firstValue + secondValue

multiply :: (PMode, Int) -> (PMode, Int) -> (PMode, Int) -> Program -> Memory
multiply first second writeTo program = writeValue result writeTo program
  where
    firstValue = readValue first program
    secondValue = readValue second program
    result = firstValue * secondValue

parseNumber :: T.Text -> Value
parseNumber = read . T.unpack
