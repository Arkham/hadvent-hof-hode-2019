{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-|

# Part One

The wires twist and turn, but the two wires occasionally cross paths. To fix
the circuit, you need to find the intersection point closest to the central
port. Because the wires are on a grid, use the Manhattan distance for this
measurement. While the wires do technically cross right at the central port
where they both start, this point does not count, nor does a wire count as
crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the
central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........

Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4,
and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

These wires cross at two locations (marked X), but the lower-left one is closer
to the central port: its distance is 3 + 3 = 6.

# Part Two

It turns out that this circuit is very timing-sensitive; you actually need to
minimize the signal delay.

To do this, calculate the number of steps each wire takes to reach each
intersection; choose the intersection where the sum of both wires' steps is
lowest. If a wire visits a position on the grid multiple times, use the steps
value from the first time it visits that position when calculating the total
value of a specific intersection.

The number of steps a wire takes is the total number of grid squares the wire
has entered to get to that location, including the intersection being
considered. Again consider the example from above:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

In the above example, the intersection closest to the central port is reached
after 8+5+5+2 = 20 steps by the first wire and 7+6+4+3 = 20 steps by the second
wire for a total of 20+20 = 40 steps.

However, the top-right intersection is better: the first wire takes only 8+5+2
= 15 and the second wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  -- let contents = "U7,R6,D4,L4\nR8,U5,L5,D3"
  let wires = map (buildWire . buildWireDescription) (T.lines contents)
  let intersections = findIntersections wires
  let distances = map manhattanDistance intersections
  print (minimum distances)
  let intersectionsWithSteps = findIntersectionsWithSteps wires
  print (minimum intersectionsWithSteps)

data Movement
  = GoUp
  | GoRight
  | GoLeft
  | GoDown
  deriving (Eq, Show, Enum)

type WireDescription = [(Movement, Int)]

buildWireDescription :: T.Text -> WireDescription
buildWireDescription input =
  concatMap
    (\command ->
       case T.unpack command of
         'U':rest -> [(GoUp, read rest :: Int)]
         'R':rest -> [(GoRight, read rest :: Int)]
         'D':rest -> [(GoDown, read rest :: Int)]
         'L':rest -> [(GoLeft, read rest :: Int)]
         _ -> [])
    (T.splitOn "," input)

type Point = (Int, Int)

type PointWithDistance = (Point, Int)

type Wire = HM.HashMap Point Int

origin :: PointWithDistance
origin = ((0, 0), 0)

buildPath ::
     (Movement, Int)
  -> PointWithDistance
  -> ([PointWithDistance], PointWithDistance)
buildPath (movement, distance) start@((startX, startY), startDistance)
  | distance <= 0 = ([], start)
  | otherwise = (points, end)
  where
    (movX, movY) =
      case movement of
        GoUp -> (0, 1)
        GoRight -> (1, 0)
        GoDown -> (0, -1)
        GoLeft -> (-1, 0)
    movePoint amount =
      ((startX + movX * amount, startY + movY * amount), startDistance + amount)
    points = [movePoint x | x <- [1 .. distance]]
    end = movePoint distance

buildWire :: WireDescription -> Wire
buildWire description = buildWire' description origin []
  where
    buildWire' desc start acc =
      case desc of
        [] -> HM.unions acc
        first:rest -> buildWire' rest end (HM.fromList points : acc)
          where (points, end) = buildPath first start

findIntersections :: [Wire] -> [Point]
findIntersections wires = HM.keys $ foldl1 HM.intersection wires

findIntersectionsWithSteps :: [Wire] -> HM.HashMap Point Int
findIntersectionsWithSteps = foldl1 (HM.intersectionWith (+))

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = abs x + abs y
