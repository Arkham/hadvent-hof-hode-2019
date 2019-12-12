module Main where

type Point = (Int, Int, Int)

type Speed = (Int, Int, Int)

type Planet = (Point, Speed)

initial :: [Planet]
initial =
  let initialSpeed = (0, 0, 0)
   in [ ((17, -12, 13), initialSpeed)
      , ((2, 1, 1), initialSpeed)
      , ((-1, -17, 7), initialSpeed)
      , ((12, -14, 18), initialSpeed)
      ]

main :: IO ()
main = do
  let planets = simulate 0 initial
  print $ show planets
  let newPlanets = simulate 1000 initial
  print $ totalEnergy newPlanets
  let xs = map (\((x, _, _), (vX, _, _)) -> (x, vX)) planets
  let ys = map (\((_, y, _), (_, vY, _)) -> (y, vY)) planets
  let zs = map (\((_, _, z), (_, _, vZ)) -> (z, vZ)) planets
  let xRepeat = findRepeat 0 xs xs
  let yRepeat = findRepeat 0 ys ys
  let zRepeat = findRepeat 0 zs zs
  print $ lcm (lcm xRepeat yRepeat) zRepeat
  where
    findRepeat :: Int -> [(Int, Int)] -> [(Int, Int)] -> Int
    findRepeat n axis initial =
      let newAxis = simulateAxis axis
       in if newAxis == initial
            then n + 1
            else findRepeat (n + 1) newAxis initial

totalEnergy :: [Planet] -> Int
totalEnergy planets =
  sum $ map (\p -> potentialEnergy p * kineticEnergy p) planets

potentialEnergy :: Planet -> Int
potentialEnergy ((x, y, z), _) = abs x + abs y + abs z

kineticEnergy :: Planet -> Int
kineticEnergy (_, (vX, vY, vZ)) = abs vX + abs vY + abs vZ

simulate :: Int -> [Planet] -> [Planet]
simulate times planets =
  case times of
    0 -> planets
    other -> simulate (times - 1) newPlanets
  where
    findDiffs ((x, y, z), _) =
      foldl
        (\(dX, dY, dZ) ((pX, pY, pZ), _) ->
           ( dX +
             (if pX > x
                then 1
                else 0) +
             (if pX < x
                then -1
                else 0)
           , dY +
             (if pY > y
                then 1
                else 0) +
             (if pY < y
                then -1
                else 0)
           , dX +
             (if pZ > z
                then 1
                else 0) +
             (if pZ < z
                then -1
                else 0)))
        (0, 0, 0)
        planets
    newPlanets =
      map
        (\point@((x, y, z), (sX, sY, sZ)) ->
           let (dX, dY, dZ) = findDiffs point
               (sX', sY', sZ') = (sX + dX, sY + dY, sZ + dZ)
            in ((x + sX', y + sY', z + sZ'), (sX', sY', sZ')))
        planets

simulateAxis :: [(Int, Int)] -> [(Int, Int)]
simulateAxis axis =
  map
    (\point@(pos, velocity) ->
       let d = findDiff point
        in (pos + velocity + d, velocity + d))
    axis
  where
    findDiff (pos, _) =
      foldl
        (\d (pointPos, _) ->
           (d +
            (if pointPos > pos
               then 1
               else 0) +
            (if pointPos < pos
               then -1
               else 0)))
        0
        axis
