{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (HashMap, (!))

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.Index as Index
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Debug.Trace

{-|

# Part One

Unsure of the state of Santa's ship, you borrowed the tractor beam
technology from Triton. Time to test it out.

When you're safely away from anything else, you activate the tractor beam,
but nothing happens. It's hard to tell whether it's working if there's
nothing to use it on. Fortunately, your ship's drone system can be
configured to deploy a drone to specific coordinates and then check whether
it's being pulled. There's even an Intcode program (your puzzle input) that
gives you access to the drone system.

The program uses two input instructions to request the X and Y position to
which the drone should be deployed. Negative numbers are invalid and will
confuse the drone; all numbers should be zero or positive.

Then, the program will output whether the drone is stationary (0) or being
pulled by something (1). For example, the coordinate X=0, Y=0 is directly
in front of the tractor beam emitter, so the drone control program will
always report 1 at that location.

To better understand the tractor beam, it is important to get a good
picture of the beam itself. For example, suppose you scan the 10x10 grid of
points closest to the emitter:

       X
  0->      9
 0#.........
 |.#........
 v..##......
  ...###....
  ....###...
Y .....####.
  ......####
  ......####
  .......###
 9........##

In this example, the number of points affected by the tractor beam in the
10x10 area closest to the emitter is 27.

However, you'll need to scan a larger area to understand the shape of the
beam. How many points are affected by the tractor beam in the 50x50 area
closest to the emitter? (For each of X and Y, this will be 0 through 49.)

# Part Two

You aren't sure how large Santa's ship is. You aren't even sure if you'll need
to use this thing on Santa's ship, but it doesn't hurt to be prepared. You
figure Santa's ship might fit in a 100x100 square.

The beam gets wider as it travels away from the emitter; you'll need to be a
minimum distance away to fit a square of that size into the beam fully. (Don't
rotate the square; it should be aligned to the same axes as the drone grid.)

For example, suppose you have the following tractor beam readings:

#.......................................
.#......................................
..##....................................
...###..................................
....###.................................
.....####...............................
......#####.............................
......######............................
.......#######..........................
........########........................
.........#########......................
..........#########.....................
...........##########...................
...........############.................
............############................
.............#############..............
..............##############............
...............###############..........
................###############.........
................#################.......
.................########OOOOOOOOOO.....
..................#######OOOOOOOOOO#....
...................######OOOOOOOOOO###..
....................#####OOOOOOOOOO#####
.....................####OOOOOOOOOO#####
.....................####OOOOOOOOOO#####
......................###OOOOOOOOOO#####
.......................##OOOOOOOOOO#####
........................#OOOOOOOOOO#####
.........................OOOOOOOOOO#####
..........................##############
..........................##############
...........................#############
............................############
.............................###########

In this example, the 10x10 square closest to the emitter that fits entirely
within the tractor beam has been marked O. Within it, the point closest to the
emitter (the only highlighted O) is at X=25, Y=20.

Find the 100x100 square closest to the emitter that fits entirely within the
tractor beam; within that square, find the point closest to the emitter. What
value do you get if you take that point's X coordinate, multiply it by 10000,
then add the point's Y coordinate? (In the example above, this would be
250020.)

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory =
        HashMap.fromList $
        Index.indexed $ map parseNumber $ T.splitOn "," contents
  let program = buildProgram memory
  putStrLn "Part One"
  let points = [(x, y) | x <- [0 .. 49], y <- [0 .. 49]]
  let initialState = State {_map = HashMap.empty}
  let state =
        List.foldl'
          (\s (x, y) ->
             let (output, _) =
                   runProgramWithOutput [fromIntegral x, fromIntegral y] program
              in State
                   {_map = HashMap.insert (x, y) (intToTile output) (_map s)})
          initialState
          points
  printState state
  let pulled = filter (\(k, v) -> v == Pulled) $ HashMap.toList (_map state)
  print $ length $ pulled
  putStrLn "Part Two"
  let farPoints = [(x, y) | x <- [0 .. 2000], y <- [1000]]
  print $
    filter (\(_, _, v) -> v == Pulled) $
    List.foldl'
      (\acc (x, y) ->
         let (output, _) =
               runProgramWithOutput [fromIntegral x, fromIntegral y] program
          in (x, y, intToTile output) : acc)
      []
      farPoints

type Point = (Int, Int)

data State =
  State
    { _map :: HashMap Point Tile
    }
  deriving (Show)

data Tile
  = Stationary
  | Pulled
  deriving (Eq, Show)

intToTile :: [Value] -> Tile
intToTile [0] = Stationary
intToTile [1] = Pulled
intToTile other = trace ("other" <> T.pack (show other)) Stationary

prettify :: Tile -> Char
prettify Stationary = '.'
prettify Pulled = '#'

printState :: State -> IO ()
printState state = do
  sequence $ printLines
  pure ()
  where
    theMap = _map state
    points = HashMap.keys theMap
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
              (\x -> prettify $ HashMap.lookupDefault Stationary (x, y) theMap)
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
