{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Debug.Trace

{-|

You ask the nanofactory to produce a list of the reactions it can perform that
are relevant to this process (your puzzle input). Every reaction turns some
quantities of specific input chemicals into some quantity of an output
chemical. Almost every chemical is produced by exactly one reaction; the only
exception, ORE, is the raw material input to the entire process and is not
produced by a reaction.

You just need to know how much ORE you'll need to collect before you can
produce one unit of FUEL.

Each reaction gives specific quantities for its inputs and output; reactions
cannot be partially run, so only whole integer multiples of these quantities
can be used. (It's okay to have leftover chemicals when you're done, though.)
For example, the reaction 1 A, 2 B, 3 C => 2 D means that exactly 2 units of
chemical D can be produced by consuming exactly 1 A, 2 B and 3 C. You can run
the full reaction as many times as necessary; for example, you could produce 10
D by consuming 5 A, 10 B, and 15 C.

Suppose your nanofactory produces the following list of reactions:

10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL

The first two reactions use only ORE as inputs; they indicate that you can
produce as much of chemical A as you want (in increments of 10 units, each 10
costing 10 ORE) and as much of chemical B as you want (each costing 1 ORE). To
produce 1 FUEL, a total of 31 ORE is required: 1 ORE to produce 1 B, then 30
more ORE to produce the 7 + 7 + 7 + 7 = 28 A (with 2 extra A wasted) required
in the reactions to convert the B into C, C into D, D into E, and finally E
into FUEL. (30 A is produced because its reaction requires that it is created
in increments of 10.)

Or, suppose you have the following list of reactions:

9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL

The above list of reactions requires 165 ORE to produce 1 FUEL:

Consume 45 ORE to produce 10 A.
Consume 64 ORE to produce 24 B.
Consume 56 ORE to produce 40 C.
Consume 6 A, 8 B to produce 2 AB.
Consume 15 B, 21 C to produce 3 BC.
Consume 16 C, 4 A to produce 4 CA.
Consume 2 AB, 3 BC, 4 CA to produce 1 FUEL.

Here are some larger examples:

13312 ORE for 1 FUEL:

157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT

180697 ORE for 1 FUEL:

2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF

2210736 ORE for 1 FUEL:

171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX

Given the list of reactions in your puzzle input, what is the minimum amount of
ORE required to produce exactly 1 FUEL?

-}
main :: IO ()
main = do
  contents <- TI.readFile "input.txt"
  case parseReactions contents of
    Right reactions -> do
      let dependencies = toDependencies reactions
      -- print $ show $ dependencies
      let (cost, remainder) = findCost dependencies
      putStrLn "Part One"
      print cost
      putStrLn "Part Two"
      let oreFound = 1000000000000 :: Double
      case (cost, HashMap.lookup "ORE" remainder) of
        (n, Nothing) -> print $ floor (oreFound / fromIntegral n)
        (n, Just v) -> print $ floor (oreFound / fromIntegral n - v)
    Left err -> TI.putStrLn err

type MaterialId = T.Text

type MaterialQuantity = (MaterialId, Int)

data Reaction =
  Reaction [MaterialQuantity] MaterialQuantity
  deriving (Eq, Show)

data Dependency =
  Dependency
    { _minAmount :: Int
    , _deps :: [(Int, MaterialQuantity)]
    , _leafDistance :: Int
    }
  deriving (Eq, Show)

type Dependencies = HashMap MaterialId Dependency

toDependencies :: [Reaction] -> Dependencies
toDependencies reactions =
  toDependencies'
    reactions
    (HashMap.singleton
       "ORE"
       (Dependency {_minAmount = 1, _deps = [], _leafDistance = 0}))

toDependencies' :: [Reaction] -> Dependencies -> Dependencies
toDependencies' [] current = current
toDependencies' (first@(Reaction inputs (outputId, outputAmount)):rest) current =
  case sequence $
       map
         (\(id, quantity) ->
            fmap
              (\el -> (_leafDistance el, (id, quantity)))
              (HashMap.lookup id current))
         inputs of
    Nothing -> toDependencies' (rest <> [first]) current
    Just inputs ->
      let updated =
            HashMap.insert
              outputId
              (Dependency
                 { _minAmount = outputAmount
                 , _deps = inputs
                 , _leafDistance = (maximum (map fst inputs)) + 1
                 })
              current
       in toDependencies' rest updated

findCost :: Dependencies -> (Int, HashMap MaterialId Double)
findCost current =
  case fmap _deps $ HashMap.lookup "FUEL" current of
    Nothing -> (0, initialRemainder)
    Just fuelDeps -> sortAndfindCost initialRemainder fuelDeps
  where
    initialRemainder = HashMap.empty
    sortAndfindCost r deps = findCost' r $ dedupe $ reverse $ List.sort deps
    findCost' r [(_, ("ORE", n))] = (n, r)
    findCost' r (first@(_, (id, quantity)):rest) =
      case HashMap.lookup id current of
        Nothing -> findCost' r rest
        Just dep ->
          let minAmount = _minAmount dep
              (factor, remainder) =
                case quantity `divMod` minAmount of
                  (x, 0) -> (x, 0)
                  (x, y) -> (x + 1, minAmount - y)
              factorDeps =
                map
                  (\(p, (depId, depAmount)) -> (p, (depId, depAmount * factor)))
                  (_deps dep)
              newR =
                case (HashMap.lookup id r, remainder) of
                  (Nothing, 0) -> r
                  (m, _) ->
                    foldl
                      (\acc (_, (depId, depAmount)) ->
                         let change =
                               (fromIntegral remainder + Maybe.fromMaybe 0 m) *
                               (fromIntegral depAmount) /
                               (fromIntegral minAmount)
                          in HashMap.alter
                               (\existingValue ->
                                  case existingValue of
                                    Just v -> Just (v + change)
                                    Nothing -> Just change)
                               depId
                               acc)
                      (HashMap.delete id r)
                      (_deps dep)
           in sortAndfindCost newR $ factorDeps <> rest
    dedupe input@((p1, (i1, q1)):(_, (i2, q2)):rest)
      | i1 == i2 = dedupe ((p1, (i1, q1 + q2)) : rest)
      | otherwise = input
    dedupe other = other

parseReactions :: T.Text -> Either T.Text [Reaction]
parseReactions input =
  sequence $
  map
    (\line ->
       case T.splitOn " => " line of
         [inputs, output] ->
           case ( (sequence $ map parseMaterialQuantity (T.splitOn ", " inputs))
                , (parseMaterialQuantity output)) of
             (Right is, Right o) -> Right (Reaction is o)
             _ -> Left "Could not parse inputs or output"
         _ -> Left "Could not parse reaction")
    (T.lines input)

parseMaterialQuantity :: T.Text -> Either T.Text MaterialQuantity
parseMaterialQuantity input =
  case T.splitOn " " input of
    [quantity, material] -> Right (material, parseInt quantity)
    _ -> Left "Could not parse material"

parseInt :: T.Text -> Int
parseInt = read . T.unpack

trace :: Show a => T.Text -> a -> a
trace message value =
  Debug.Trace.trace (T.unpack (message <> " " <> T.pack (show value))) value
