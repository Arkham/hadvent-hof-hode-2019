module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-|
Fuel required to launch a given module is based on its mass. Specifically, to
find the fuel required for a module, take its mass, divide by three, round
down, and subtract 2.

For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.

For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel
required is also 2.
-}
simpleGetFuel :: Integer -> Integer
simpleGetFuel = subtract2 . roundDown . divideBy3
  where
    divideBy3 n = (fromInteger n :: Double) / 3
    roundDown = floor
    subtract2 n = n - 2

{-|
Fuel itself requires fuel just like a module - take its mass, divide by three,
round down, and subtract 2. However, that fuel also requires fuel, and that
fuel requires fuel, and so on. Any mass that would require negative fuel should
instead be treated as if it requires zero fuel; the remaining mass, if any, is
instead handled by wishing really hard, which has no mass and is outside the
scope of this calculation.

So, for each module mass, calculate its fuel and add it to the total. Then,
treat the fuel amount you just calculated as the input mass and repeat the
process, continuing until a fuel requirement is zero or negative. For example:

A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2
divided by 3 and rounded down is 0, which would call for a negative fuel), so
the total fuel required is still just 2.

At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216
more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires 21
fuel, which requires 5 fuel, which requires no further fuel. So, the total fuel
required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
-}
getFuel :: Integer -> Integer
getFuel mass = getFuel' mass 0
  where
    getFuel' m acc
      | fuel > 0 = getFuel' fuel (acc + fuel)
      | otherwise = acc
      where
        fuel = simpleGetFuel m

main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let numbers = map parseLine (T.lines contents)
  print (sum (map getFuel numbers))

parseLine :: T.Text -> Integer
parseLine = read . T.unpack
