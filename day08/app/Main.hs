{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-|

# Part One

Each image actually consists of a series of identically-sized layers that are
filled in this way. So, the first digit corresponds to the top-left pixel of
the first layer, the second digit corresponds to the pixel to the right of that
on the same layer, and so on until the last digit, which corresponds to the
bottom-right pixel of the last layer.

For example, given an image 3 pixels wide and 2 pixels tall, the image data
123456789012 corresponds to the following image layers:

Layer 1: 123
         456

Layer 2: 789
         012

The image you received is 25 pixels wide and 6 pixels tall.

To make sure the image wasn't corrupted during transmission, the Elves would
like you to find the layer that contains the fewest 0 digits. On that layer,
what is the number of 1 digits multiplied by the number of 2 digits?

# Part Two

Now you're ready to decode the image. The image is rendered by stacking the
layers and aligning the pixels with the same positions in each layer. The
digits indicate the color of the corresponding pixel: 0 is black, 1 is white,
and 2 is transparent.

The layers are rendered with the first layer in front and the last layer in
back. So, if a given position has a transparent pixel in the first and second
layers, a black pixel in the third layer, and a white pixel in the fourth
layer, the final image would have a black pixel at that position.

For example, given an image 2 pixels wide and 2 pixels tall, the image data
0222112222120000 corresponds to the following image layers:

Layer 1: 02
         22

Layer 2: 11
         22

Layer 3: 22
         12

Layer 4: 00
         00

Then, the full image can be found by determining the top visible pixel in each position:

- The top-left pixel is black because the top layer is 0.
- The top-right pixel is white because the top layer is 2 (transparent), but the second layer is 1.
- The bottom-left pixel is white because the top two layers are 2, but the third layer is 1.
- The bottom-right pixel is black because the only visible pixel in that position is 0 (from layer 4).

So, the final image looks like this:

01
10

What message is produced after decoding your image?

-}
main = do
  contents <- TI.readFile "input.txt"
  let dimensions = (25, 6)
  let layers = getLayers dimensions contents
  let result =
        minimum $
        map
          (\layer ->
             ( digitsInLayer 0 layer
             , digitsInLayer 1 layer * digitsInLayer 2 layer))
          layers
  putStrLn "Part One"
  print result
  let visible = findVisible dimensions layers
  putStrLn "\nPart Two"
  showLayer visible

data Layer =
  Layer
    { _width :: Int
    , _height :: Int
    , _cells :: [Int]
    }

getLayers :: (Int, Int) -> T.Text -> [Layer]
getLayers (width, height) input =
  map (Layer width height) $
  Split.chunksOf (width * height) $
  map ((flip (-) 48) . fromEnum) (T.unpack input)

digitsInLayer :: Int -> Layer -> Int
digitsInLayer n (Layer {_cells = cells}) = length $ filter (== n) cells

parseInt :: Char -> Int
parseInt char = read [char]

findVisible :: (Int, Int) -> [Layer] -> Layer
findVisible (width, height) layers =
  Layer
    { _width = width
    , _height = height
    , _cells = map getVisiblePixel $ List.transpose (map _cells layers)
    }
  where
    getVisiblePixel pixels =
      Maybe.fromMaybe transparent $ List.find (/= transparent) pixels

transparent :: Int
transparent = 2

showLayer :: Layer -> IO ()
showLayer (Layer {_width = width, _cells = cells}) = do
  sequence $ map putStrLn $ Split.chunksOf width str
  pure ()
  where
    str = List.intercalate "" $ map toSym cells
    toSym 0 = " "
    toSym 1 = "█"
    toSym 2 = "~"
