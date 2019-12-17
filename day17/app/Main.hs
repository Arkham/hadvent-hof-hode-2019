{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (HashMap, (!))

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.Index as Index
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Debug.Trace

{-|

# Part One

An Intcode program, the Aft Scaffolding Control and Information Interface
(ASCII, your puzzle input), provides access to the cameras and the vacuum
robot. Currently, because the vacuum robot is asleep, you can only access the
cameras.

Running the ASCII program on your Intcode computer will provide the current
view of the scaffolds. This is output, purely coincidentally, as ASCII code: 35
means #, 46 means ., 10 starts a new line of output below the current one, and
so on. (Within a line, characters are drawn left-to-right.)

In the camera output, # represents a scaffold and . represents open space. The
vacuum robot is visible as ^, v, <, or > depending on whether it is facing up,
down, left, or right respectively. When drawn like this, the vacuum robot is
always on a scaffold; if the vacuum robot ever walks off of a scaffold and
begins tumbling through space uncontrollably, it will instead be visible as X.

In general, the scaffold forms a path, but it sometimes loops back onto itself.
  For example, suppose you can see the following view from the cameras:

..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^..

Here, the vacuum robot, ^ is facing up and sitting at one end of the scaffold
near the bottom-right of the image. The scaffold continues up, loops across
itself several times, and ends at the top-left of the image.

The first step is to calibrate the cameras by getting the alignment parameters
of some well-defined points. Locate all scaffold intersections; for each, its
alignment parameter is the distance between its left edge and the left edge of
the view multiplied by the distance between its top edge and the top edge of
the view. Here, the intersections from the above image are marked O:

..#..........
..#..........
##O####...###
#.#...#...#.#
##O###O###O##
..#...#...#..
..#####...^..

For these intersections:

- The top-left intersection is 2 units from the left of the image and 2 units
from the top of the image, so its alignment parameter is 2 * 2 = 4.
- The bottom-left intersection is 2 units from the left and 4 units from the top,
so its alignment parameter is 2 * 4 = 8.
- The bottom-middle intersection is 6 from the left and 4 from the top, so its
alignment parameter is 24.
- The bottom-right intersection's alignment parameter is 40.

To calibrate the cameras, you need the sum of the alignment parameters. In the
above example, this is 76.

Run your ASCII program. What is the sum of the alignment parameters for the
scaffold intersections?

# Part Two

Force the vacuum robot to wake up by changing the value in your ASCII program
at address 0 from 1 to 2. When you do this, you will be automatically prompted
for the new movement rules that the vacuum robot should use. The ASCII program
will use input instructions to receive them, but they need to be provided as
ASCII code; end each line of logic with a single newline, ASCII code 10.

First, you will be prompted for the main movement routine. The main routine may
only call the movement functions: A, B, or C. Supply the movement functions to
use as ASCII text, separating them with commas (,, ASCII code 44), and ending
the list with a newline (ASCII code 10). For example, to call A twice, then
alternate between B and C three times, provide the string A,A,B,C,B,C,B,C and
then a newline.

Then, you will be prompted for each movement function. Movement functions may
use L to turn left, R to turn right, or a number to move forward that many
units. Movement functions may not call other movement functions. Again,
separate the actions with commas and end the list with a newline. For example,
to move forward 10 units, turn left, move forward 8 units, turn right, and
finally move forward 6 units, provide the string 10,L,8,R,6 and then a newline.

Finally, you will be asked whether you want to see a continuous video feed;
provide either y or n and a newline. Enabling the continuous video feed can
help you see what's going on, but it also requires a significant amount of
processing power, and may even cause your Intcode computer to overheat.

Due to the limited amount of memory in the vacuum robot, the ASCII definitions
of the main routine and the movement functions may each contain at most 20
characters, not counting the newline.

For example, consider the following camera feed:

#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......

In order for the vacuum robot to visit every part of the scaffold at least
once, one path it could take is:

R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2

Without the memory limit, you could just supply this whole string to function A
and have the main routine call A once. However, you'll need to split it into
smaller parts.

One approach is:

Main routine: A,B,C,B,A,C
(ASCII input: 65, 44, 66, 44, 67, 44, 66, 44, 65, 44, 67, 10)

Function A:   R,8,R,8
(ASCII input: 82, 44, 56, 44, 82, 44, 56, 10)

Function B:   R,4,R,4,R,8
(ASCII input: 82, 44, 52, 44, 82, 44, 52, 44, 82, 44, 56, 10)

Function C:   L,6,L,2
(ASCII input: 76, 44, 54, 44, 76, 44, 50, 10)

Visually, this would break the desired path into the following parts:

A,        B,            C,        B,            A,        C
R,8,R,8,  R,4,R,4,R,8,  L,6,L,2,  R,4,R,4,R,8,  R,8,R,8,  L,6,L,2

CCCCCCA...BBBBB
C.....A...B...B
C.....A...B...B
......A...B...B
......A...CCC.B
......A.....C.B
^AAAAAAAA...C.B
......A.A...C.B
......AAAAAA#AB
........A...C..
....BBBB#BBBB..
....B...A......
....B...A......
....B...A......
....BBBBA......

Of course, the scaffolding outside your ship is much more complex.

As the vacuum robot finds other robots and notifies them of the impending solar
flare, it also can't help but leave them squeaky clean, collecting any space
dust it finds. Once it finishes the programmed set of movements, assuming it
hasn't drifted off into space, the cleaning robot will return to its docking
station and report the amount of space dust it collected as a large, non-ASCII
value in a single output instruction.

After visiting every part of the scaffold at least once, how much dust does the
vacuum robot report it has collected?

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory =
        HashMap.fromList $
        Index.indexed $ map parseNumber $ T.splitOn "," contents
  let program = buildProgram memory
  putStrLn "Part One"
  let (output, _) = runProgramWithOutput [] program
  let state = buildState output
  printState state
  let intersections = findIntersections state
  print intersections
  print $ sum $ map (uncurry (*)) intersections
  putStrLn "Part Two"
  let path = findPath state
  TI.putStrLn $ prettifyPath path

data Direction
  = ToRight
  | ToLeft
  deriving (Eq, Show)

data Step
  = Turn Direction
  | Forward Int
  deriving (Eq, Show)

prettifyPath :: [Step] -> T.Text
prettifyPath steps =
  T.intercalate "," $
  map
    (\step ->
       case step of
         Turn ToRight -> "R"
         Turn ToLeft -> "L"
         Forward n -> "F" <> T.pack (show n))
    steps

findPath :: State -> [Step]
findPath state = reverse $ findPathWithAcc state []

findPathWithAcc :: State -> [Step] -> [Step]
findPathWithAcc state steps =
  let theMap = _map state
      (pos, facingDir) = _position state
      forward = forwardPoint facingDir pos
      left = leftPoint facingDir pos
      right = rightPoint facingDir pos
      hasScaffold p = HashMap.lookup p theMap == Just Scaffold
      result
        | hasScaffold forward =
          case steps of
            (Forward n:rest) ->
              Right (Forward (n + 1) : rest, (forward, facingDir))
            _ -> Right (Forward 1 : steps, (forward, facingDir))
        | hasScaffold left =
          Right
            (Turn ToLeft : steps, (pos, turnFacingDirection ToLeft facingDir))
        | hasScaffold right =
          Right
            (Turn ToRight : steps, (pos, turnFacingDirection ToRight facingDir))
        | otherwise = Left steps
   in case result of
        Right (newSteps, newPos) ->
          findPathWithAcc (state {_position = newPos}) newSteps
        Left newSteps -> newSteps

findIntersections :: State -> [Point]
findIntersections state =
  let theMap = _map state
      scaffoldCells = filter (\(k, v) -> v == Scaffold) (HashMap.toList theMap)
      allNeighboursScaffolds (x, y) =
        all
          (\(offsetX, offsetY) ->
             HashMap.lookup (x + offsetX, y + offsetY) theMap == Just Scaffold)
          neighbourOffsets
   in map fst $ filter (\(pos, _) -> allNeighboursScaffolds pos) scaffoldCells

neighbourOffsets :: [Point]
neighbourOffsets = [(0, 1), (1, 0), (0, -1), (-1, 0)]

buildState :: [Value] -> State
buildState output =
  buildState'
    output
    (0, 0)
    (State {_map = HashMap.empty, _position = ((0, 0), FacingUp)})
  where
    buildState' [] _ acc = acc
    buildState' (first:rest) (x, y) acc =
      case (Char.chr (fromInteger first)) of
        '>' ->
          buildState' rest (x + 1, y) (acc {_position = ((x, y), FacingRight)})
        '^' ->
          buildState' rest (x + 1, y) (acc {_position = ((x, y), FacingUp)})
        'v' ->
          buildState' rest (x + 1, y) (acc {_position = ((x, y), FacingDown)})
        '<' ->
          buildState' rest (x + 1, y) (acc {_position = ((x, y), FacingLeft)})
        '#' ->
          buildState'
            rest
            (x + 1, y)
            (acc {_map = HashMap.insert (x, y) Scaffold (_map acc)})
        '.' ->
          buildState'
            rest
            (x + 1, y)
            (acc {_map = HashMap.insert (x, y) Empty (_map acc)})
        '\n' -> buildState' rest (0, y + 1) acc
        _ -> trace "unknown output" acc

type Point = (Int, Int)

data State =
  State
    { _map :: HashMap Point Tile
    , _position :: (Point, FacingDirection)
    }
  deriving (Show)

data Tile
  = Empty
  | Scaffold
  deriving (Eq, Show)

data FacingDirection
  = FacingUp
  | FacingDown
  | FacingRight
  | FacingLeft
  deriving (Eq, Show)

forwardPoint :: FacingDirection -> Point -> Point
forwardPoint dir (x, y) =
  case dir of
    FacingUp -> (x, y - 1)
    FacingDown -> (x, y + 1)
    FacingRight -> (x + 1, y)
    FacingLeft -> (x - 1, y)

leftPoint :: FacingDirection -> Point -> Point
leftPoint dir (x, y) =
  case dir of
    FacingUp -> (x - 1, y)
    FacingDown -> (x + 1, y)
    FacingRight -> (x, y - 1)
    FacingLeft -> (x, y + 1)

rightPoint :: FacingDirection -> Point -> Point
rightPoint dir (x, y) =
  case dir of
    FacingUp -> (x + 1, y)
    FacingDown -> (x - 1, y)
    FacingRight -> (x, y + 1)
    FacingLeft -> (x, y - 1)

turnFacingDirection :: Direction -> FacingDirection -> FacingDirection
turnFacingDirection dir facingDir =
  case dir of
    ToRight ->
      case facingDir of
        FacingUp -> FacingRight
        FacingDown -> FacingLeft
        FacingLeft -> FacingUp
        FacingRight -> FacingDown
    ToLeft ->
      case facingDir of
        FacingUp -> FacingLeft
        FacingDown -> FacingRight
        FacingLeft -> FacingDown
        FacingRight -> FacingUp

prettify :: Tile -> Char
prettify Empty = '.'
prettify Scaffold = '#'

prettifyFacingDirection :: FacingDirection -> Char
prettifyFacingDirection FacingUp = '^'
prettifyFacingDirection FacingDown = 'v'
prettifyFacingDirection FacingRight = '>'
prettifyFacingDirection FacingLeft = '<'

printState :: State -> IO ()
printState state = do
  sequence $ printLines
  pure ()
  where
    theMap = _map state
    (pos, dir) = _position state
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
              (\x ->
                 if (x, y) == pos
                   then prettifyFacingDirection dir
                   else prettify $ HashMap.lookupDefault Empty (x, y) theMap)
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
