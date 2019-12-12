{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (HashMap, (!))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.Index as Index
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Debug.Trace

{-|

You'll need to build a new emergency hull painting robot. The robot needs to be
able to move around on the grid of square panels on the side of your ship,
detect the color of its current panel, and paint its current panel black or
white. (All of the panels are currently black.)

The Intcode program will serve as the brain of the robot. The program uses
input instructions to access the robot's camera, provide 0 if the robot is over
a black panel or 1 if the robot is over a white panel. Then, the program will
output two values:

First, it will output a value indicating the color to paint the panel the robot
is over: 0 means to paint the panel black, and 1 means to paint the panel
white.

Second, it will output a value indicating the direction the robot should turn:
0 means it should turn left 90 degrees, and 1 means it should turn right 90
degrees.

After the robot turns, it should always move forward exactly one panel. The
robot starts facing up.

The robot will continue running for a while like this and halt when it is
finished drawing. Do not restart the Intcode computer inside the robot during
this process.

For example, suppose the robot is about to start running. Drawing black panels
as ., white panels as #, and the robot pointing the direction it is facing (< ^
> v), the initial state and region near the robot looks like this:

.....
.....
..^..
.....
.....

The panel under the robot (not visible here because a ^ is shown instead) is
also black, and so any input instructions at this point should be provided 0.
Suppose the robot eventually outputs 1 (paint white) and then 0 (turn left).
After taking these actions and moving forward one panel, the region now looks
like this:

.....
.....
.<#..
.....
.....

Input instructions should still be provided 0. Next, the robot might output 0
(paint black) and then 0 (turn left):

.....
.....
..#..
.v...
.....

After more outputs (1,0, 1,0):

.....
.....
..^..
.##..
.....

The robot is now back where it started, but because it is now on a white panel,
input instructions should be provided 1. After several more outputs (0,1, 1,0,
1,0), the area looks like this:

.....
..<#.
...#.
.##..
.....

Before you deploy the robot, you should probably have an estimate of the area
it will cover: specifically, you need to know the number of panels it paints at
least once, regardless of color. In the example above, the robot painted 6
panels at least once. (It painted its starting panel twice, but that panel is
still only counted once; it also never painted the panel it ended on.)

Build a new emergency hull painting robot and run the Intcode program on it.
How many panels does it paint at least once?

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory =
        HashMap.fromList $
        Index.indexed $ map parseNumber $ T.splitOn "," contents
  let program = buildProgram memory
  putStrLn "Part One"
  let map = HashMap.empty
  let result = visitAndPaint startingPoint startingDirection map program
  print (length (HashMap.keys result))
  putStrLn "Part Two"
  let map = HashMap.singleton startingPoint White
  printMap $ visitAndPaint startingPoint startingDirection map program

visitAndPaint :: Point -> Direction -> Map -> Program -> Map
visitAndPaint point direction map program =
  let currentColor = HashMap.lookupDefault Black point map
      newProgram = runProgram [(fromIntegral $ fromEnum currentColor)] program
   in case (_returnValue newProgram, _output newProgram) of
        (Nothing, intDirection:intColor:rest) ->
          let newColor = toEnum $ fromIntegral intColor
              newMap = HashMap.insert point newColor map
              directionFn =
                case intDirection of
                  0 -> turnLeft
                  1 -> turnRight
              newDirection = directionFn direction
              newPoint = moveForward newDirection point
           in visitAndPaint newPoint newDirection newMap newProgram
        _ -> map

printMap :: Map -> IO ()
printMap aMap = do
  sequence $ printLines
  pure ()
  where
    points = HashMap.keys aMap
    xs = map fst points
    ys = map snd points
    (maxX, minX) = (maximum xs, minimum xs)
    (maxY, minY) = (maximum ys, minimum ys)
    xRange = [minX .. maxX]
    yRange = reverse [minY .. maxY]
    prettyColor Black = ' '
    prettyColor White = '█'
    printLines =
      map
        (\y ->
           putStrLn $
           (map
              (\x -> prettyColor $ HashMap.lookupDefault Black (x, y) aMap)
              xRange))
        yRange

type Point = (Int, Int)

startingPoint :: Point
startingPoint = (0, 0)

data Direction
  = FacingUp
  | FacingRight
  | FacingDown
  | FacingLeft
  deriving (Eq, Show)

startingDirection :: Direction
startingDirection = FacingUp

turnRight :: Direction -> Direction
turnRight direction =
  case direction of
    FacingUp -> FacingRight
    FacingRight -> FacingDown
    FacingDown -> FacingLeft
    FacingLeft -> FacingUp

turnLeft :: Direction -> Direction
turnLeft direction =
  case direction of
    FacingUp -> FacingLeft
    FacingLeft -> FacingDown
    FacingDown -> FacingRight
    FacingRight -> FacingUp

data Color
  = Black
  | White
  deriving (Enum)

type Map = HashMap Point Color

moveForward :: Direction -> Point -> Point
moveForward direction (x, y) =
  case direction of
    FacingUp -> (x, y + 1)
    FacingRight -> (x + 1, y)
    FacingDown -> (x, y - 1)
    FacingLeft -> (x - 1, y)

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
