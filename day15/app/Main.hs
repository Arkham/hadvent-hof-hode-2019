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

The remote control program executes the following steps in a loop forever:

- Accept a movement command via an input instruction.
- Send the movement command to the repair droid.
- Wait for the repair droid to finish the movement operation.
- Report on the status of the repair droid via an output instruction.
- Only four movement commands are understood: north (1), south (2), west (3),
and east (4). Any other command is invalid. The movements differ in direction,
but not in distance: in a long enough east-west hallway, a series of commands
like 4,4,4,4,3,3,3,3 would leave the repair droid back where it started.

The repair droid can reply with any of the following status codes:

0: The repair droid hit a wall. Its position has not changed.
1: The repair droid has moved one step in the requested direction.
2: The repair droid has moved one step in the requested direction; its new
position is the location of the oxygen system.

You don't know anything about the area around the repair droid, but you can
figure it out by watching the status codes.

For example, we can draw the area using D for the droid, # for walls, . for
locations the droid can traverse, and empty space for unexplored locations.
Then, the initial state looks like this:

      
      
   D  
      
      

N E S W

0 1

0 0 0 1



To make the droid go north, send it 1. If it replies with 0, you know that
location is a wall and that the droid didn't move:

      
   #  
   D  
      
      

To move east, send 4; a reply of 1 means the movement was successful:

      
   #  
   .D 
      
      

Then, perhaps attempts to move north (1), south (2), and east (4) are all met with replies of 0:

      
   ## 
   .D#
    # 
      

Now, you know the repair droid is in a dead end. Backtrack with 3 (which you
already know will get a reply of 1 because you already know that location is
open):

      
   ## 
   D.#
    # 
      

Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of 1, south
again (2) gets a reply of 0, and then west (3) gets a reply of 2:

      
   ## 
  #..#
  D.# 
   #  

Now, because of the reply of 2, you know you've found the oxygen system! In
this example, it was only 2 moves away from the repair droid's starting
position.

What is the fewest number of movement commands required to move the repair
droid from its starting position to the location of the oxygen system?`

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory =
        HashMap.fromList $
        Index.indexed $ map parseNumber $ T.splitOn "," contents
  let program = buildProgram memory
  putStrLn "Part One"
  state <- visitMap (map (\d -> (d, 0)) directions) (buildState (0, 0)) program
  putStrLn "Part Two"
  printState state
  let (minutesElapsed, finalState) = fillWithOxygen state 0
  print minutesElapsed
  printState finalState

fillWithOxygen :: State -> Int -> (Int, State)
fillWithOxygen state count =
  let theMap = _map state
      emptyCells = filter (\(k, v) -> v == Empty) $ HashMap.toList theMap
   in if null emptyCells
        then (count, state)
        else let newOxygenPoints =
                   concatMap
                     (\((x, y), _) ->
                        let anyOxygenNear =
                              any
                                (\dir ->
                                   let (offsetX, offsetY) =
                                         directionToOffset dir
                                       newPoint = (x + offsetX, y + offsetY)
                                    in HashMap.lookup newPoint theMap ==
                                       Just Oxygen)
                                directions
                         in if anyOxygenNear
                              then [(x, y)]
                              else [])
                     emptyCells
                 newMap =
                   foldr
                     (\(x, y) acc -> HashMap.insert (x, y) Oxygen acc)
                     theMap
                     newOxygenPoints
              in fillWithOxygen state {_map = newMap} (count + 1)

buildState :: Point -> State
buildState point =
  State {_map = HashMap.singleton point Empty, _position = point}

visitMap :: [(Direction, Int)] -> State -> Program -> IO State
visitMap [] state program = pure state
visitMap ((direction, distance):rest) state@(State { _map = aMap
                                                   , _position = position
                                                   }) program = do
  let (offsetX, offsetY) = directionToOffset direction
      (x, y) = position
      newPoint = (x + offsetX, y + offsetY)
   in case HashMap.lookup newPoint aMap of
        Nothing ->
          let (droidResponse, newProgram) = peekDirection direction program
              visitAndAdvance kind =
                visitMap (newDirections <> rest) newState newProgram
                where
                  newMap = HashMap.insert newPoint kind aMap
                  newState = state {_map = newMap, _position = newPoint}
                  opposite = oppositeDirection direction
                  newDirections =
                    map (\d -> (d, distance + 1)) $
                    (filter (/= opposite) directions) <> [opposite]
           in case droidResponse of
                FoundWall ->
                  let newMap = HashMap.insert newPoint Wall aMap
                   in visitMap rest (state {_map = newMap}) newProgram
                FoundEmpty -> do
                  visitAndAdvance Empty
                FoundOxygen -> do
                  putStrLn
                    ("Found oxygen at " <>
                     show newPoint <> " with distance " <> show (distance + 1))
                  visitAndAdvance Oxygen
                Unrecognized -> do
                  putStrLn "Unrecognized droid response"
                  pure state
        Just Wall -> visitMap rest state program
        Just Empty ->
          let (droidResponse, newProgram) = peekDirection direction program
           in case droidResponse of
                FoundEmpty ->
                  visitMap rest (state {_position = newPoint}) newProgram
                _ -> do
                  putStrLn "Empty space expected, but missing"
                  pure state

peekDirection :: Direction -> Program -> (DroidResponse, Program)
peekDirection direction program =
  let (output, newProgram) =
        runProgramWithOutput [directionToInput direction] program
   in (parseDroidResponse output, newProgram)

type Point = (Int, Int)

data State =
  State
    { _map :: HashMap Point Tile
    , _position :: Point
    }

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

directions :: [Direction]
directions = [North, East, West, South]

directionToInput :: Direction -> Value
directionToInput direction =
  case direction of
    North -> 1
    South -> 2
    East -> 3
    West -> 4

oppositeDirection :: Direction -> Direction
oppositeDirection direction =
  case direction of
    North -> South
    South -> North
    East -> West
    West -> East

directionToOffset :: Direction -> Point
directionToOffset direction =
  case direction of
    North -> (0, 1)
    South -> (0, -1)
    East -> (1, 0)
    West -> (-1, 0)

data DroidResponse
  = FoundWall
  | FoundEmpty
  | FoundOxygen
  | Unrecognized
  deriving (Eq, Show)

parseDroidResponse :: [Value] -> DroidResponse
parseDroidResponse (0:_) = FoundWall
parseDroidResponse (1:_) = FoundEmpty
parseDroidResponse (2:_) = FoundOxygen
parseDroidResponse _ = Unrecognized

data Tile
  = Empty
  | Wall
  | Oxygen
  deriving (Eq, Show)

prettify :: Maybe Tile -> Char
prettify Nothing = ' '
prettify (Just Empty) = '.'
prettify (Just Wall) = '#'
prettify (Just Oxygen) = 'O'

printState :: State -> IO ()
printState state = do
  sequence $ printLines
  pure ()
  where
    aMap = _map state
    xRange = [-40 .. 40]
    yRange = [-30 .. 30]
    printLines =
      map
        (\y ->
           putStrLn $
           (map
              (\x ->
                 if (x, y) == _position state
                   then '@'
                   else if (x, y) == (0, 0)
                          then 'X'
                          else prettify $ HashMap.lookup (x, y) aMap)
              xRange))
        yRange

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

runProgramWithOutput :: [Value] -> Program -> ([Value], Program)
runProgramWithOutput input program =
  let newProgram = runProgram input program
   in (reverse $ _output newProgram, newProgram {_output = []})

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
