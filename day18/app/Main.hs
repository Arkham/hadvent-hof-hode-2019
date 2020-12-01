{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.List.Index as Index
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Debug.Trace
import Text.Read (readMaybe)

{-|

A scan of the local area reveals only one interesting feature: a massive
underground vault. You generate a map of the tunnels (your puzzle input). The
tunnels are too narrow to move diagonally.

Only one entrance (marked @) is present among the open passages (marked .) and
stone walls (#), but you also detect an assortment of keys (shown as lowercase
letters) and doors (shown as uppercase letters). Keys of a given letter open
the door of the same letter: a opens A, b opens B, and so on. You aren't sure
which key you need to disable the tractor beam, so you'll need to collect all
of them.

For example, suppose you have the following map:

#########
#b.A.@.a#
#########

Starting from the entrance (@), you can only access a large door (A) and a key
(a). Moving toward the door doesn't help you, but you can move 2 steps to
collect the key, unlocking A in the process:

#########
#b.....@#
#########

Then, you can move 6 steps to collect the only other key, b:

#########
#@......#
#########

So, collecting every key took a total of 8 steps.

Here is a larger example:

########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################

The only reasonable move is to take key a and unlock door A:

########################
#f.D.E.e.C.b.....@.B.c.#
######################.#
#d.....................#
########################

Then, do the same with key b:

########################
#f.D.E.e.C.@.........c.#
######################.#
#d.....................#
########################

...and the same with key c:

########################
#f.D.E.e.............@.#
######################.#
#d.....................#
########################

Now, you have a choice between keys d and e. While key e is closer, collecting
it now would be slower in the long run than collecting key d first, so that's
the best choice:

########################
#f...E.e...............#
######################.#
#@.....................#
########################

Finally, collect key e to unlock door E, then collect key f, taking a grand total of 86 steps.

How many steps is the shortest path that collects all of the keys?

-}
main :: IO ()
main = do
  let contents = T.intercalate "\n" [ "#########", "#b.A.@.a#", "#########" ]
  let map = parseMap contents
  printMap map
  let yourPos = fst $ head $ filter (\(k, v) -> v == You) $ HashMap.toList map
  let keysWithDistance = findKeysWithDistance yourPos map

type Point = (Int, Int)

data Cell
  = Wall
  | Empty
  | Door Char
  | Key Char
  | You
  deriving (Eq, Show)

type Map =
  HashMap Point Cell

parseMap :: T.Text -> Map
parseMap input =
  let lines = T.lines input
      indexedLines = Index.indexed lines
  in List.foldl' (\acc (index, row) -> parseRow index row acc) HashMap.empty indexedLines
 where
   parseRow y row theMap =
     List.foldl'
       (\acc (x, el) ->
         HashMap.insert (x, y) (parseCell el) acc)
       theMap
       (Index.indexed $ T.unpack row)

parseCell :: Char -> Cell
parseCell '#' = Wall
parseCell '.' = Empty
parseCell '@' = You
parseCell c
  | Char.isUpper c = Door c
  | Char.isLower c = Key (Char.toUpper c)
  | otherwise = trace "unknown cell" Empty

prettifyCell :: Cell -> Char
prettifyCell Wall = '#'
prettifyCell Empty = '.'
prettifyCell You = '@'
prettifyCell (Door c) = c
prettifyCell (Key c) = Char.toLower c

printMap :: Map -> IO ()
printMap theMap = do
  sequence $ printLines
  pure ()
  where
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
              (\x -> prettifyCell $ HashMap.lookupDefault Empty (x, y) theMap)
              xRange))
        yRange

trace :: Show a => T.Text -> a -> a
trace message value =
  Debug.Trace.trace (T.unpack (message <> " " <> T.pack (show value))) value

findKeysWithDistance :: Map -> Point -> [(Char, Int)]
findKeysWithDistance theMap (x, y) =
  let distances = HashMap.singleton (x, y) 0
   in findKeysWithDistance' m (x, y) distances []
  where findKeysWithDistance' m distances acc =
    let outermostPoints = List.sortHashMap.toList distances

neighbourOffsets :: [Point]
neighbourOffsets =
  [ (0, 1)
  , (1, 0)
  , (0, -1)
  , (-1, 0)
  ]
