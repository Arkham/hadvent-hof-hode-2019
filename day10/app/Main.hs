{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import qualified Debug.Trace

{-|

The map indicates whether each position is empty (.) or contains an asteroid
(#). The asteroids are much smaller than they appear on the map, and every
asteroid is exactly in the center of its marked position. The asteroids can be
described with X,Y coordinates where X is the distance from the left edge and Y
is the distance from the top edge (so the top-left corner is 0,0 and the
position immediately to its right is 1,0).

Your job is to figure out which asteroid would be the best place to build a new
monitoring station. A monitoring station can detect any asteroid to which it
has direct line of sight - that is, there cannot be another asteroid exactly
between them. This line of sight can be at any angle, not just lines aligned to
the grid or diagonally. The best location is the asteroid that can detect the
largest number of other asteroids.

For example, consider the following map:

.#..#
.....
#####
....#
...##

The best location for a new monitoring station on this map is the highlighted
asteroid at 3,4 because it can detect 8 asteroids, more than any other
location. (The only asteroid it cannot detect is the one at 1,0; its view of
this asteroid is blocked by the asteroid at 2,2.) All other asteroids are worse
locations; they can detect 7 or fewer other asteroids. Here is the number of
other asteroids a monitoring station on each asteroid could detect:

.7..7
.....
67775
....7
...87

Here is an asteroid (#) and some examples of the ways its line of sight might
be blocked. If there were another asteroid at the location of a capital letter,
the locations marked with the corresponding lowercase letter would be blocked
and could not be detected:

..........
...A......
...B..a...
.EDCG....a
..F.c.b...
.....c....
..efd.c.gb
.......c..
....f...c.
...e..d..c

Here are some larger examples:

Best is 5,8 with 33 other asteroids detected:

......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####

Best is 1,2 with 35 other asteroids detected:

#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case buildMap contents of
    Right aMap -> do
      let asteroids =
            concatMap
              (\(x, y, v) ->
                 if v == Asteroid
                   then [(x, y)]
                   else [])
              (toCoordList aMap)
      print (length (asteroids))
      let counted =
            map
              (\a ->
                 let grouped = groupByAngle a asteroids
                  in ( length grouped
                     , (List.take 1 .
                        List.drop 199 . concat . List.transpose . reverse)
                         grouped
                     , a))
              asteroids
      print . show . head . reverse . List.sort $ counted
    Left err -> putStrLn err

data Cell
  = Empty
  | Asteroid
  deriving (Eq, Show)

data AsteroidMap =
  AsteroidMap Int Int (Vector (Vector Cell))

buildMap :: T.Text -> Either String AsteroidMap
buildMap text =
  let result = map (map parseChar . T.unpack) $ T.lines text
   in case result of
        [] -> Left "Empty rows"
        first:rest ->
          case first of
            [] -> Left "Empty cols"
            other ->
              Right $
              AsteroidMap
                (length result)
                (length first)
                (Vector.fromList (map Vector.fromList result))

toCoordList :: AsteroidMap -> [(Int, Int, Cell)]
toCoordList (AsteroidMap rows cols cells) =
  let allCoordinates = [(x, y) | x <- [0 .. (rows - 1)], y <- [0 .. (cols - 1)]]
   in map (\(x, y) -> (x, y, cells ! y ! x)) allCoordinates

findAngle :: (Int, Int) -> (Int, Int) -> Double
findAngle (x1, y1) (x2, y2) =
  atan2 (fromIntegral (x1 - x2)) ((fromIntegral (y1 - y2)))

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x1, y1) (x2, y2) =
  sqrt ((fromIntegral (x1 - x2)) ^ 2 + (fromIntegral (y1 - y2)) ^ 2)

groupByAngle :: (Int, Int) -> [(Int, Int)] -> [[(Double, Double, (Int, Int))]]
groupByAngle self points =
  List.groupBy (\(a, _, _) (b, _, _) -> a == b) $
  List.sort $
  map (\point -> (findAngle point self, distance point self, point)) $
  filter (/= self) points

trace :: Show a => T.Text -> a -> a
trace message value =
  Debug.Trace.trace (T.unpack (message <> " " <> T.pack (show value))) value

parseChar :: Char -> Cell
parseChar '#' = Asteroid
parseChar _ = Empty
