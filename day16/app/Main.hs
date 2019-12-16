{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Debug.Trace
import Text.Read (readMaybe)

{-|

As input, FFT takes a list of numbers. In the signal you received (your puzzle
input), each number is a single digit: data like 15243 represents the sequence
1, 5, 2, 4, 3.

FFT operates in repeated phases. In each phase, a new list is constructed with
the same length as the input list. This new list is also used as the input for
the next phase.

Each element in the new list is built by multiplying every value in the input
list by a value in a repeating pattern and then adding up the results. So, if
the input list were 9, 8, 7, 6, 5 and the pattern for a given element were 1,
2, 3, the result would be 9*1 + 8*2 + 7*3 + 6*1 + 5*2 (with each input element
on the left and each value in the repeating pattern on the right of each
multiplication). Then, only the ones digit is kept: 38 becomes 8, -17 becomes
7, and so on.

While each element in the output array uses all of the same input array
elements, the actual repeating pattern to use depends on which output element
is being calculated. The base pattern is 0, 1, 0, -1. Then, repeat each value
in the pattern a number of times equal to the position in the output list being
considered. Repeat once for the first element, twice for the second element,
three times for the third element, and so on. So, if the third element of the
output list is being calculated, repeating the values would produce: 0, 0, 0,
1, 1, 1, 0, 0, 0, -1, -1, -1.

When applying the pattern, skip the very first value exactly once. (In other
words, offset the whole pattern left by one.) So, for the second element of the
output list, the actual pattern used would be: 0, 1, 1, 0, 0, -1, -1, 0, 0, 1,
1, 0, 0, -1, -1, ....

After using this process to calculate each element of the output list, the
phase is complete, and the output list of this phase is used as the new input
list for the next phase, if any.

Given the input signal 12345678, below are four phases of FFT. Within each
phase, each output digit is calculated on a single line with the result at the
far right; each multiplication operation shows the input digit on the left and
the pattern value on the right:

Input signal: 12345678

1*1  + 2*0  + 3*-1 + 4*0  + 5*1  + 6*0  + 7*-1 + 8*0  = 4
1*0  + 2*1  + 3*1  + 4*0  + 5*0  + 6*-1 + 7*-1 + 8*0  = 8
1*0  + 2*0  + 3*1  + 4*1  + 5*1  + 6*0  + 7*0  + 8*0  = 2
1*0  + 2*0  + 3*0  + 4*1  + 5*1  + 6*1  + 7*1  + 8*0  = 2
1*0  + 2*0  + 3*0  + 4*0  + 5*1  + 6*1  + 7*1  + 8*1  = 6
1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*1  + 7*1  + 8*1  = 1
1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*1  + 8*1  = 5
1*0  + 2*0  + 3*0  + 4*0  + 5*0  + 6*0  + 7*0  + 8*1  = 8

After 1 phase: 48226158

4*1  + 8*0  + 2*-1 + 2*0  + 6*1  + 1*0  + 5*-1 + 8*0  = 3
4*0  + 8*1  + 2*1  + 2*0  + 6*0  + 1*-1 + 5*-1 + 8*0  = 4
4*0  + 8*0  + 2*1  + 2*1  + 6*1  + 1*0  + 5*0  + 8*0  = 0
4*0  + 8*0  + 2*0  + 2*1  + 6*1  + 1*1  + 5*1  + 8*0  = 4
4*0  + 8*0  + 2*0  + 2*0  + 6*1  + 1*1  + 5*1  + 8*1  = 0
4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*1  + 5*1  + 8*1  = 4
4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*0  + 5*1  + 8*1  = 3
4*0  + 8*0  + 2*0  + 2*0  + 6*0  + 1*0  + 5*0  + 8*1  = 8

-}
main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  putStrLn "Part One:"
  input <- TI.readFile "input.txt"
  let inputDigits = Maybe.catMaybes $ map parseNumber $ T.unpack input
  let result = runPhases 100 inputDigits
  putStrLn $ List.intercalate "" $ map show $ take 8 result

{-|

The offset usually falls over the second part of our number.

If we look at how the pattern is distributed in the second half of our number
we will see that it's like this:

1  0  -1 0  1  0  -1 0
0  1  1  0  0  -1 -1 0
0  0  1  1  1  0  0  0
0  0  0  1  1  1  1  0
0  0  0  0  1  1  1  1
0  0  0  0  0  1  1  1
0  0  0  0  0  0  1  1
0  0  0  0  0  0  0  1

First of all previous numbers (from the first half of the number) don't
influence the second half of the number. Also there is no real variation
of indexes, but it's a descending chain of ones.

So we can just ignore all numbers before the offset. For the numbers after the
offset we can see that they are just a positive sum of all following numbers.
So if we start from the last number and go backwards it's enough to sum all the
numbers one by one to obtain the new number.

-}
part2 :: IO ()
part2 = do
  putStrLn "Part Two:"
  input <- TI.readFile "input.txt"
  let inputDigits = Maybe.catMaybes $ map parseNumber $ T.unpack input
  let offset = (read $ concat $ map show $ take 7 inputDigits) :: Int
  let digits = drop offset $ concat $ replicate 10000 inputDigits
  let result = fastPhases 100 digits
  putStrLn $ List.intercalate "" $ map show $ take 8 result

parseNumber :: Char -> Maybe Int
parseNumber a = readMaybe [a]

trace :: Show a => T.Text -> a -> a
trace message value =
  Debug.Trace.trace (T.unpack (message <> " " <> T.pack (show value))) value

runPhases :: Int -> [Int] -> [Int]
runPhases 0 digits = digits
runPhases n digits = runPhases (n - 1) result
  where
    result =
      map
        (\index -> lastDigit $ sum $ zipWith (*) digits (buildPattern index))
        [1 .. (length digits)]

buildPattern :: Int -> [Int]
buildPattern n = drop 1 $ cycle $ concatMap (replicate n) [0, 1, 0, -1]

lastDigit :: Int -> Int
lastDigit n = abs n `mod` 10

fastPhases :: Int -> [Int] -> [Int]
fastPhases 0 digits = digits
fastPhases n digits = fastPhases (n - 1) result
  where
    (_, result) =
      foldr
        (\el (count, acc) ->
           let newCount = el + count
            in (newCount, lastDigit newCount : acc))
        (0, [])
        digits
