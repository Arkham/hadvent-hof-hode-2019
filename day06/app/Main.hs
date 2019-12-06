{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-|

# Part One

Before you use your map data to plot a course, you need to make sure it wasn't
corrupted during the download. To verify maps, the Universal Orbit Map facility
uses orbit count checksums - the total number of direct orbits (like the one
shown above) and indirect orbits.

Whenever A orbits B and B orbits C, then A indirectly orbits C. This chain can
be any number of objects long: if A orbits B, B orbits C, and C orbits D, then
A indirectly orbits D.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L

Visually, the above map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I

In this visual representation, when two objects are connected by a line, the
one on the right directly orbits the one on the left.

Here, we can count the total number of orbits as follows:

- D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
- L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
- COM orbits nothing.

The total number of direct and indirect orbits in this example is 42.

# Part Two

Now, you just need to figure out how many orbital transfers you (YOU) need to
take to get to Santa (SAN).

You start at the object YOU are orbiting; your destination is the object SAN is
orbiting. An orbital transfer lets you move from any object to an object
orbiting or orbited by that object.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN

Visually, the above map of orbits looks like this:

                          YOU
                         /
        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I - SAN

In this example, YOU are in orbit around K, and SAN is in orbit around I. To
move from K to I, a minimum of 4 orbital transfers are required:

K to J
J to E
E to D
D to I

Afterward, the map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \
                I - SAN
                 \
                  YOU

What is the minimum number of orbital transfers required to move from the
object YOU are orbiting to the object SAN is orbiting? (Between the objects
they are orbiting - not between YOU and SAN.)

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  let orbits = foldr parseOrbit [] (T.lines contents)
  let distances = distanceFrom (Node "COM") orbits
  print $ sum $ map length $ HashMap.elems distances
  case ( HashMap.lookup (Node "YOU") distances
       , HashMap.lookup (Node "SAN") distances) of
    (Just youHops, Just santaHops) ->
      print $ findMinimumOrbitalTransfers youHops santaHops
    _ -> putStrLn "Could not find you or santa!"

newtype Node =
  Node T.Text
  deriving (Eq, Ord, Show, Hashable)

{-|

The thing that an orbiting body orbits is called that orbiting body's
primary. Quoth Wikipedia: A natural satellite, or moon, is a celestial
body that orbits another body, e.g. a planet, which is called its
primary.

In our case an Orbit is a pair of nodes (Satellite, Primary).

Why representing the orbit in this way? First of all it reads better:
- Moon orbits around Earth

Then it also makes the data representation cleaner since a node can only orbit
around a single node. Consider this simple example:

A -> B
A -> C
B -> D
D -> E

If we represented this 'forward' we would have to deal with nested lists:

A -> [ B, C ]
B -> [ D ]
D -> [ E ]

But representing it 'backwards' makes it so each link is 1-to-1:

B -> A
C -> A
D -> B
E -> D

Tada!

-}
type Orbit = (Node, Node)

parseOrbit :: T.Text -> [Orbit] -> [Orbit]
parseOrbit current acc =
  case T.splitOn ")" current of
    [primary, satellite] -> (Node satellite, Node primary) : acc
    _ -> acc

{-|
We represent the distance as a list of hops. With this graph

A -> B
A -> C
B -> D
D -> E

we would have something like this

{
  B -> [ A ],
  C -> [ A ],
  D -> [ B, A ],
  E -> [ D, B, A],
}
-}
type Distance = HashMap Node [Node]

distanceFrom :: Node -> [Orbit] -> Distance
distanceFrom source orbits = distanceFrom' orbits initial
  where
    initial = HashMap.singleton source []
    distanceFrom' [] acc = acc
    distanceFrom' ((satellite, primary):rest) acc =
      case (HashMap.lookup satellite acc, HashMap.lookup primary acc) of
        (Just _, _) -> distanceFrom' rest acc
        (Nothing, Just v) ->
          distanceFrom' rest (HashMap.insert satellite (primary : v) acc)
        (Nothing, Nothing) -> distanceFrom' (rest <> [(satellite, primary)]) acc

{-|
In this case we just need to find the first node that is not part of both hopping
history. Then we can sum the length of the diverging paths to get the distance.
-}
findMinimumOrbitalTransfers :: [Node] -> [Node] -> Int
findMinimumOrbitalTransfers firstHops secondHops =
  doEet (reverse firstHops) (reverse secondHops)
  where
    doEet (x:xs) (y:ys) =
      if x == y
        then doEet xs ys
        else length (x : xs) + length (y : ys)
    doEet first second = length first + length second
