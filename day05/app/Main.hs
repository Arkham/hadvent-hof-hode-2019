{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as Vector
import Data.Vector ((!), (//))
import qualified Debug.Trace

{-|

First, you'll need to add two new instructions:

- Opcode 3 takes a single integer as input and saves it to the position given
by its only parameter. For example, the instruction 3,50 would take an input
value and store it at address 50.

- Opcode 4 outputs the value of its only parameter. For example, the
instruction 4,50 would output the value at address 50.

Programs that use these instructions will come with documentation that explains
what should be connected to the input and output. The program 3,0,4,0,99
outputs whatever it gets as input, then halts.

Second, you'll need to add support for parameter modes:

Each parameter of an instruction is handled based on its parameter mode. Right
now, your ship computer already understands parameter mode 0, position mode,
which causes the parameter to be interpreted as a position, if the parameter is
50, its value is the value stored at address 50 in memory. Until now, all
parameters have been in position mode.

Now, your ship computer will also need to handle parameters in mode 1,
immediate mode. In immediate mode, a parameter is interpreted as a value, if
the parameter is 50, its value is simply 50.

Parameter modes are stored in the same value as the instruction's opcode. The
opcode is a two-digit number based only on the ones and tens digit of the
value, that is, the opcode is the rightmost two digits of the first value in an
instruction. Parameter modes are single digits, one per parameter, read
right-to-left from the opcode: the first parameter's mode is in the hundreds
digit, the second parameter's mode is in the thousands digit, the third
parameter's mode is in the ten-thousands digit, and so on. Any missing modes
are 0.

For example, consider the program 1002,4,3,4,33.

The first instruction, 1002,4,3,4, is a multiply instruction, the rightmost two
digits of the first value, 02, indicate opcode 2, multiplication. Then, going
right to left, the parameter modes are 0 (hundreds digit), 1 (thousands digit),
and 0 (ten-thousands digit, not present and therefore zero):

ABCDE
 1002

DE - two-digit opcode,      02 == opcode 2
 C - mode of 1st parameter,  0 == position mode
 B - mode of 2nd parameter,  1 == immediate mode
 A - mode of 3rd parameter,  0 == position mode,
                                  omitted due to being a leading zero

This instruction multiplies its first two parameters. The first parameter, 4 in
position mode, works like it did before - its value is the value stored at
address 4 (33). The second parameter, 3 in immediate mode, simply has value 3.
The result of this operation, 33 * 3 = 99, is written according to the third
parameter, 4 in position mode, which also works like it did before: 99 is
written to address 4.

Parameters that an instruction writes to will never be in immediate mode.

Finally, some notes:

It is important to remember that the instruction pointer should increase by the
number of values in the instruction after the instruction finishes. Because of
the new instructions, this amount is no longer always 4.

Integers can be negative: 1101,100,-1,4,0 is a valid program (find 100 + -1,
store the result in position 4).

The TEST diagnostic program will start by requesting from the user the ID of
the system to test by running an input instruction - provide it 1, the ID for
the ship's air conditioner unit.

It will then perform a series of diagnostic tests confirming that various parts
of the Intcode computer, like parameter modes, function correctly. For each
test, it will run an output instruction indicating how far the result of the
test was from the expected value, where 0 means the test was successful.
Non-zero outputs mean that a function is not working correctly; check the
instructions that were run before the output instruction to see which one
failed.

Finally, the program will output a diagnostic code and immediately halt. This
final output isn't an error; an output followed immediately by a halt means the
program finished. If all outputs were zero except the diagnostic code, the
diagnostic program ran successfully.

After providing 1 to the only input instruction and passing all the tests, what
diagnostic code does the program produce?

# Part Two

Your computer is only missing a few opcodes:

- Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the
instruction pointer to the value from the second parameter. Otherwise, it does
nothing.

- Opcode 6 is jump-if-false: if the first parameter is zero, it sets the
instruction pointer to the value from the second parameter. Otherwise, it does
nothing.

- Opcode 7 is less than: if the first parameter is less than the second
parameter, it stores 1 in the position given by the third parameter. Otherwise,
it stores 0.

- Opcode 8 is equals: if the first parameter is equal to the second parameter,
it stores 1 in the position given by the third parameter. Otherwise, it stores
0.

Normally, after an instruction is finished, the instruction pointer increases
by the number of values in that instruction. However, if the instruction
modifies the instruction pointer, that value is used and the instruction
pointer is not automatically increased.

-}
main :: IO Program
main = do
  contents <- TI.readFile "input.txt"
  let program = Vector.fromList $ map parseNumber $ T.splitOn "," contents
  putStrLn "Part One:"
  _ <- runProgram 1 program
  putStrLn "\nPart Two:"
  runProgram 5 program

type Program = Vector.Vector Int

trace :: Show a => T.Text -> a -> a
trace message value =
  Debug.Trace.trace (T.unpack (message <> " " <> T.pack (show value))) value

data PMode
  = Position
  | Immediate
  deriving (Eq, Show)

data Instruction
  = Add PMode
        PMode
        PMode
  | Multiply PMode
             PMode
             PMode
  | Halt
  | Input
  | Output
  | JumpIfTrue PMode
               PMode
  | JumpIfFalse PMode
                PMode
  | LessThan PMode
             PMode
             PMode
  | Equals PMode
           PMode
           PMode
  deriving (Eq, Show)

runProgram :: Int -> Program -> IO Program
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

runProgram' :: Int -> Int -> Program -> IO Program
runProgram' ip input program =
  let opcode = program ! ip
      instruction = parseOpcode opcode
   in case instruction of
        Halt -> pure program
        Add p1 p2 _p3 ->
          runProgram'
            (ip + 4)
            input
            (add (p1, ip + 1) (p2, ip + 2) (ip + 3) program)
        Multiply p1 p2 _p3 ->
          runProgram'
            (ip + 4)
            input
            (multiply (p1, ip + 1) (p2, ip + 2) (ip + 3) program)
        Input -> runProgram' (ip + 2) input (writeValue input (ip + 1) program)
        Output -> do
          let value = readValue (Position, ip + 1) program
          print value
          runProgram' (ip + 2) input program
        JumpIfTrue p1 p2 ->
          case readValue (p1, ip + 1) program of
            0 -> runProgram' (ip + 3) input program
            _ -> runProgram' (readValue (p2, ip + 2) program) input program
        JumpIfFalse p1 p2 ->
          case readValue (p1, ip + 1) program of
            0 -> runProgram' (readValue (p2, ip + 2) program) input program
            _ -> runProgram' (ip + 3) input program
        LessThan p1 p2 _p3 ->
          let value1 = readValue (p1, ip + 1) program
              value2 = readValue (p2, ip + 2) program
              result =
                if value1 < value2
                  then 1
                  else 0
           in runProgram' (ip + 4) input (writeValue result (ip + 3) program)
        Equals p1 p2 _p3 ->
          let value1 = readValue (p1, ip + 1) program
              value2 = readValue (p2, ip + 2) program
              result =
                if value1 == value2
                  then 1
                  else 0
           in runProgram' (ip + 4) input (writeValue result (ip + 3) program)

readValue :: (PMode, Int) -> Program -> Int
readValue (Immediate, index) program = program ! index
readValue (Position, index) program = program ! (program ! index)

writeValue :: Int -> Int -> Program -> Program
writeValue value index program = program // [(program ! index, value)]

add :: (PMode, Int) -> (PMode, Int) -> Int -> Program -> Program
add first second resultIndex program = writeValue result resultIndex program
  where
    firstValue = readValue first program
    secondValue = readValue second program
    result = firstValue + secondValue

multiply :: (PMode, Int) -> (PMode, Int) -> Int -> Program -> Program
multiply first second resultIndex program =
  writeValue result resultIndex program
  where
    firstValue = readValue first program
    secondValue = readValue second program
    result = firstValue * secondValue

parseNumber :: T.Text -> Int
parseNumber = read . T.unpack
