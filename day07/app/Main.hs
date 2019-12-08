{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as Vector
import Data.Vector ((!), (//))
import qualified Debug.Trace
import qualified Safe

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

# Part Two

Most of the amplifiers are connected as they were before; amplifier A's output
is connected to amplifier B's input, and so on. However, the output from
amplifier E is now connected into amplifier A's input. This creates the
feedback loop: the signal will be sent through the amplifiers many times.

In feedback loop mode, the amplifiers need totally different phase settings:
integers from 5 to 9, again each used exactly once. These settings will cause
the Amplifier Controller Software to repeatedly take input and produce output
many times before halting. Provide each amplifier its phase setting at its
first input instruction; all further input/output instructions are for signals.

Don't restart the Amplifier Controller Software on any amplifier during this
process. Each one should continue receiving and sending signals until it halts.

All signals sent or received in this process will be between pairs of
amplifiers except the very first signal and the very last signal. To start the
process, a 0 signal is sent to amplifier A's input exactly once.

Eventually, the software on the amplifiers will halt after they have processed
the final loop. When this happens, the last output signal from amplifier E is
sent to the thrusters. Your job is to find the largest output signal that can
be sent to the thrusters using the new phase settings and feedback loop
arrangement.

Here are some example programs:

Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):

3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory = Vector.fromList $ map parseNumber $ T.splitOn "," contents
  putStrLn "Part One"
  testThusters [0] memory (List.permutations [0 .. 4]) linearSetting
  putStrLn "Part Two"
  testThusters [0] memory (List.permutations [5 .. 9]) cyclicSetting

testThusters :: [Value] -> Memory -> [[Integer]] -> SettingCalculator -> IO ()
testThusters input memory settings calculator =
  case sequence $ map (calculator input memory) settings of
    Just v -> print (maximum v)
    Nothing -> putStrLn "Something went wrong"

buildProgram :: Memory -> Program
buildProgram memory =
  Program {_ip = 0, _memory = memory, _output = [], _returnValue = Nothing}

type SettingCalculator = [Value] -> Memory -> [Integer] -> Maybe Value

linearSetting :: [Value] -> Memory -> [Integer] -> Maybe Value
linearSetting inputSignal memory settings =
  Safe.headMay $
  foldl
    (\input elem ->
       case input of
         [value] ->
           let newProgram = runProgram [elem, value] (buildProgram memory)
            in _output newProgram
         _ -> [])
    inputSignal
    settings

cyclicSetting :: [Value] -> Memory -> [Integer] -> Maybe Value
cyclicSetting input memory settings = runThis input amps
  where
    amps = map (\n -> (buildProgram memory, n, False)) settings
    runThis values list =
      case runThis' values list of
        (Nothing, output, newAmps) -> runThis output (reverse newAmps)
        (Just _, output, _) -> Safe.headMay output
    runThis' values list =
      foldl
        (\(returnValue, output, acc) (program, setting, alreadyInitialized) ->
           case output of
             value:rest ->
               let newInput =
                     (if alreadyInitialized
                        then [value]
                        else [setting, value])
                   newProgram = runProgram newInput program
                   newOutput = _output newProgram
                   newReturnValue = returnValue `orElse` _returnValue newProgram
                in ( newReturnValue
                   , newOutput
                   , (newProgram, setting, True) : acc)
             _ -> (returnValue, output, acc))
        (Nothing, values, [])
        list

type Value = Integer

type Memory = Vector.Vector Value

data Program =
  Program
    { _memory :: Memory
    , _ip :: Int
    , _output :: [Value]
    , _returnValue :: Maybe Int
    }
  deriving (Show)

setMemory :: Memory -> Program -> Program
setMemory memory program = program {_memory = memory}

addOutput :: Value -> Program -> Program
addOutput value program@(Program {_output = output}) =
  program {_output = value : output}

setReturnValue :: Int -> Program -> Program
setReturnValue value program = program {_returnValue = Just value}

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

parseOpcode :: Value -> Instruction
parseOpcode opcode =
  let twoDigitOpcode = opcode `mod` 100
      parseParameterMode 0 = Position
      parseParameterMode 1 = Immediate
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
        99 -> Halt

runProgram :: [Value] -> Program -> Program
runProgram input program =
  let memory = _memory program
      ip = _ip program
      opcode = memory ! ip
      instruction = parseOpcode opcode
   in case instruction of
        Halt -> setReturnValue 0 program
        Add p1 p2 _p3 ->
          runProgram input $
          program
            { _ip = ip + 4
            , _memory = add (p1, ip + 1) (p2, ip + 2) (ip + 3) memory
            }
        Multiply p1 p2 _p3 ->
          runProgram input $
          program
            { _ip = ip + 4
            , _memory = multiply (p1, ip + 1) (p2, ip + 2) (ip + 3) memory
            }
        Input ->
          case input of
            first:rest ->
              runProgram rest $
              program {_ip = ip + 2, _memory = writeValue first (ip + 1) memory}
            _ -> program
        Output ->
          let value = readValue (Position, ip + 1) memory
              newProgram = addOutput value program
           in runProgram input $ newProgram {_ip = ip + 2}
        JumpIfTrue p1 p2 ->
          case readValue (p1, ip + 1) memory of
            0 -> runProgram input $ program {_ip = ip + 3}
            _ ->
              runProgram input $
              program {_ip = fromInteger (readValue (p2, ip + 2) memory)}
        JumpIfFalse p1 p2 ->
          case readValue (p1, ip + 1) memory of
            0 ->
              runProgram input $
              program {_ip = fromInteger (readValue (p2, ip + 2) memory)}
            _ -> runProgram input $ program {_ip = ip + 3}
        LessThan p1 p2 _p3 ->
          let value1 = readValue (p1, ip + 1) memory
              value2 = readValue (p2, ip + 2) memory
              result =
                if value1 < value2
                  then 1
                  else 0
           in runProgram input $
              program
                {_ip = ip + 4, _memory = writeValue result (ip + 3) memory}
        Equals p1 p2 _p3 ->
          let value1 = readValue (p1, ip + 1) memory
              value2 = readValue (p2, ip + 2) memory
              result =
                if value1 == value2
                  then 1
                  else 0
           in runProgram input $
              program
                {_ip = ip + 4, _memory = writeValue result (ip + 3) memory}

readValue :: (PMode, Int) -> Memory -> Value
readValue (Immediate, index) memory = memory ! index
readValue (Position, index) memory = memory ! (fromInteger (memory ! index))

writeValue :: Value -> Int -> Memory -> Memory
writeValue value index memory =
  memory // [(fromInteger (memory ! index), value)]

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

parseNumber :: T.Text -> Value
parseNumber = read . T.unpack

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y =
  case x of
    Just _ -> x
    Nothing -> y
