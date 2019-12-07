{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as Vector
import Data.Vector ((!), (//))
import qualified Debug.Trace

{-|

# Part One

There are five amplifiers connected in series; each one receives an input
signal and produces an output signal. They are connected such that the first
amplifier's output leads to the second amplifier's input, the second
amplifier's output leads to the third amplifier's input, and so on. The first
amplifier's input value is 0, and the last amplifier's output leads to your
ship's thrusters.

    O-------O  O-------O  O-------O  O-------O  O-------O
0 ->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-> (to thrusters)
    O-------O  O-------O  O-------O  O-------O  O-------O

The Elves have sent you some Amplifier Controller Software (your puzzle input),
a program that should run on your existing Intcode computer. Each amplifier
will need to run a copy of the program.

When a copy of the program starts running on an amplifier, it will first use an
input instruction to ask the amplifier for its current phase setting (an
integer from 0 to 4). Each phase setting is used exactly once, but the Elves
can't remember which amplifier needs which phase setting.

The program will then call another input instruction to get the amplifier's
input signal, compute the correct output signal, and supply it back to the
amplifier with an output instruction. (If the amplifier has not yet received an
input signal, it waits until one arrives.)

Your job is to find the largest output signal that can be sent to the thrusters
by trying every possible combination of phase settings on the amplifiers. Make
sure that memory is not shared or reused between copies of the program.

For example, suppose you want to try the phase setting sequence 3,1,2,4,0,
which would mean setting amplifier A to phase setting 3, amplifier B to setting
1, C to 2, D to 4, and E to 0. Then, you could determine the output signal that
gets sent from amplifier E to the thrusters with the following steps:

- Start the copy of the amplifier controller software that will run on
amplifier A. At its first input instruction, provide it the amplifier's phase
setting, 3. At its second input instruction, provide it the input signal, 0.
After some calculations, it will use an output instruction to indicate the
amplifier's output signal.

- Start the software for amplifier B. Provide it the phase setting (1) and then
whatever output signal was produced from amplifier A. It will then produce a
new output signal destined for amplifier C.

- Start the software for amplifier C, provide the phase setting (2) and the
value from amplifier B, then collect its output signal.

- Run amplifier D's software, provide the phase setting (4) and input value,
and collect its output signal.

- Run amplifier E's software, provide the phase setting (0) and input value,
and collect its output signal.

The final output signal from amplifier E would be sent to the thrusters.
However, this phase setting sequence may not have been the best one; another
sequence might have sent a higher signal to the thrusters.

Here is an example program:

Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):

3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0

Try every combination of phase settings on the amplifiers. What is the highest
signal that can be sent to the thrusters?

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory = Vector.fromList $ map parseNumber $ T.splitOn "," contents
  let availableSettings = List.permutations [0 .. 4]
  let values =
        sequence $ map (calculateSetting (Just 0) memory) availableSettings
  case values of
    Just v -> print (maximum v)
    Nothing -> putStrLn "Something went wrong"

calculateSetting :: Maybe Int -> Memory -> [Int] -> Maybe Int
calculateSetting inputSignal memory settings =
  foldl
    (\output elem ->
       output >>=
       (\value ->
          let (Program _ returnValue) =
                runProgram [elem, value] (Program memory Nothing)
           in returnValue))
    inputSignal
    settings

type Memory = Vector.Vector Int

data Program =
  Program Memory (Maybe Int)

type Input = [Int]

trace :: Show a => T.Text -> a -> a
trace message value =
  Debug.Trace.trace (T.unpack (message <> " " <> T.pack (show value))) value

data PMode
  = Position
  | Immediate
  deriving (Eq, Show)

data Instruction
  = Add PMode PMode PMode
  | Multiply PMode PMode PMode
  | Halt
  | Input
  | Output
  | JumpIfTrue PMode PMode
  | JumpIfFalse PMode PMode
  | LessThan PMode PMode PMode
  | Equals PMode PMode PMode
  deriving (Eq, Show)

runProgram :: Input -> Program -> Program
runProgram = runProgram' 0

parseOpcode :: Int -> Instruction
parseOpcode opcode =
  let twoDigitOpcode = opcode `mod` 100
      parseParameterMode 0 = Position
      parseParameterMode _ = Immediate
      p1 = parseParameterMode $ opcode `div` 100 `mod` 10
      p2 = parseParameterMode $ opcode `div` 1000 `mod` 10
      p3 = parseParameterMode $ opcode `div` 10000 `mod` 10
   in case twoDigitOpcode of
        1 -> Add p1 p2 p3
        2 -> Multiply p1 p2 p3
        3 -> Input
        4 -> Output
        5 -> JumpIfTrue p1 p2
        6 -> JumpIfFalse p1 p2
        7 -> LessThan p1 p2 p3
        8 -> Equals p1 p2 p3
        _ -> Halt

runProgram' :: Int -> Input -> Program -> Program
runProgram' ip input program@(Program memory returnValue) =
  let opcode = memory ! ip
      instruction = parseOpcode opcode
   in case instruction of
        Halt -> program
        Add p1 p2 _p3 ->
          runProgram'
            (ip + 4)
            input
            (Program (add (p1, ip + 1) (p2, ip + 2) (ip + 3) memory) returnValue)
        Multiply p1 p2 _p3 ->
          runProgram'
            (ip + 4)
            input
            (Program
               (multiply (p1, ip + 1) (p2, ip + 2) (ip + 3) memory)
               returnValue)
        Input ->
          case input of
            first:rest ->
              runProgram'
                (ip + 2)
                rest
                (Program (writeValue first (ip + 1) memory) returnValue)
            _ -> program
        Output ->
          let value = readValue (Position, ip + 1) memory
           in runProgram' (ip + 2) input (Program memory (Just value))
        JumpIfTrue p1 p2 ->
          case readValue (p1, ip + 1) memory of
            0 -> runProgram' (ip + 3) input program
            _ -> runProgram' (readValue (p2, ip + 2) memory) input program
        JumpIfFalse p1 p2 ->
          case readValue (p1, ip + 1) memory of
            0 -> runProgram' (readValue (p2, ip + 2) memory) input program
            _ -> runProgram' (ip + 3) input program
        LessThan p1 p2 _p3 ->
          let value1 = readValue (p1, ip + 1) memory
              value2 = readValue (p2, ip + 2) memory
              result =
                if value1 < value2
                  then 1
                  else 0
           in runProgram'
                (ip + 4)
                input
                (Program (writeValue result (ip + 3) memory) returnValue)
        Equals p1 p2 _p3 ->
          let value1 = readValue (p1, ip + 1) memory
              value2 = readValue (p2, ip + 2) memory
              result =
                if value1 == value2
                  then 1
                  else 0
           in runProgram'
                (ip + 4)
                input
                (Program (writeValue result (ip + 3) memory) returnValue)

readValue :: (PMode, Int) -> Memory -> Int
readValue (Immediate, index) memory = memory ! index
readValue (Position, index) memory = memory ! (memory ! index)

writeValue :: Int -> Int -> Memory -> Memory
writeValue value index memory = memory // [(memory ! index, value)]

add :: (PMode, Int) -> (PMode, Int) -> Int -> Memory -> Memory
add first second resultIndex memory = writeValue result resultIndex memory
  where
    firstValue = readValue first memory
    secondValue = readValue second memory
    result = firstValue + secondValue

multiply :: (PMode, Int) -> (PMode, Int) -> Int -> Memory -> Memory
multiply first second resultIndex memory = writeValue result resultIndex memory
  where
    firstValue = readValue first memory
    secondValue = readValue second memory
    result = firstValue * secondValue

parseNumber :: T.Text -> Int
parseNumber = read . T.unpack
