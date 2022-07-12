module Main where

import Perlin
import Codec.Picture

main :: IO ()
main = writePng "image.png" $ generatePerlinImage 500 500 20 20
