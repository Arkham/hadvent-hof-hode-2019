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

# Part One

Your existing Intcode computer is missing one key feature: it needs support for
parameters in relative mode.

Parameters in mode 2, relative mode, behave very similarly to parameters in
position mode: the parameter is interpreted as a position. Like position mode,
parameters in relative mode can be read from or written to.

The important difference is that relative mode parameters don't count from
address 0. Instead, they count from a value called the relative base. The
relative base starts at 0.

The address a relative mode parameter refers to is itself plus the current
relative base. When the relative base is 0, relative mode parameters and
position mode parameters with the same value refer to the same address.

For example, given a relative base of 50, a relative mode parameter of -7
refers to memory address 50 + -7 = 43.

The relative base is modified with the relative base offset instruction:

Opcode 9 adjusts the relative base by the value of its only parameter. The
relative base increases (or decreases, if the value is negative) by the value
of the parameter.

For example, if the relative base is 2000, then after the instruction 109,19,
the relative base would be 2019. If the next instruction were 204,-34, then the
value at address 1985 would be output.

Your Intcode computer will also need a few other capabilities:

The computer's available memory should be much larger than the initial program.
Memory beyond the initial program starts with the value 0 and can be read or
written like any other memory. (It is invalid to try to access memory at a
negative address, though.)

The computer should have support for large numbers. Some instructions near the
beginning of the BOOST program will verify this capability.

Here are some example programs that use these features:

- 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 takes no input and produces a copy of itself as output.
- 1102,34915192,34915192,7,4,7,99,0 should output a 16-digit number.
- 104,1125899906842624,99 should output the large number in the middle.

The BOOST program will ask for a single input; run it in test mode by providing
it the value 1. It will perform a series of checks on each opcode, output any
opcodes (and the associated parameter modes) that seem to be functioning
incorrectly, and finally output a BOOST keycode.

Once your Intcode computer is fully functional, the BOOST program should report
no malfunctioning opcodes when run in test mode; it should only output a single
value, the BOOST keycode. What BOOST keycode does it produce?

# Part Two

The program runs in sensor boost mode by providing the input instruction the
value 2. Once run, it will boost the sensors automatically, but it might take a
few seconds to complete the operation on slower hardware. In sensor boost mode,
the program will output a single value: the coordinates of the distress signal.

Run the BOOST program in sensor boost mode. What are the coordinates of the
distress signal?

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let memory =
        HashMap.fromList $
        Index.indexed $ map parseNumber $ T.splitOn "," contents
  putStrLn "Part One"
  runProgramWithOutput [1] (buildProgram memory)
  putStrLn "\nPart Two"
  runProgramWithOutput [2] (buildProgram memory)

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
