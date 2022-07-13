module Perlin ( generatePerlinImage
              , interpolate ) where

import Data.Bits
import Codec.Picture

type Vector2D = (Double, Double)

interpolate :: Double -> Double -> Double -> Double
interpolate a0 a1 w 
  = (a1 - a0) * ((w * (w * 6.0 - 15.0) + 10.0) * w * w * w) + a0

randomGradientVector :: Int -> Int -> Vector2D
randomGradientVector ix iy = (sin random, cos random)
  where
    ix' = fromIntegral ix :: Word
    iy' = fromIntegral iy :: Word
    w = finiteBitSize ix'
    s = w `div` 2
    a = ix' * 3284157443
    a' = (a `shiftL` s) .|. (a `shiftR` (w - s))
    b = 1911520717 * (iy' ^ a')
    b' = (b `shiftL` s) .|. (b `shiftR` (w - s))
    random = fromIntegral $ 2048419325 * (a' ^ b')

dotProduct :: Vector2D -> Vector2D -> Double
dotProduct (ax, ay) (bx, by) = ax * bx + ay * by

dotGridGradient :: Int -> Int -> Double -> Double -> Double
dotGridGradient ix iy x y = gradient `dotProduct` displacement
  where
    gradient     = randomGradientVector ix iy
    displacement = (x - fromIntegral ix, y - fromIntegral iy)

perlinNoiseAtPoint :: Double -> Double -> Double
perlinNoiseAtPoint x y = value
  where
    x0 = floor x
    y0 = floor y
    x1 = x0 + 1
    y1 = y0 + 1
    -- Interpolation weights
    sx = x - fromIntegral x0
    sy = y - fromIntegral y0
    -- Interpolating between gridpoint gradients
    n0 = dotGridGradient x0 y0 x y
    n1 = dotGridGradient x1 y0 x y
    i0 = interpolate n0 n1 sx

    m0 = dotGridGradient x0 y1 x y
    m1 = dotGridGradient x1 y1 x y
    i1 = interpolate m0 m1 sx

    value = interpolate i0 i1 sy

sigmoid :: Double -> Double -> Double
sigmoid a x = 1.0 / (1.0 + exp (negate a * x))

generatePerlinImage :: Int -> Int -> Int -> Int -> Double -> Int -> Image Pixel8
generatePerlinImage pixelsW pixelsH gridW gridH sharpness seed
  = generateImage generatePerlinPixel pixelsW pixelsH
  where
    generatePerlinPixel :: Int -> Int -> Pixel8
    generatePerlinPixel pixelX pixelY
      = floor $ 255 * sigmoid sharpness noise
      where
        gridX = (fromIntegral pixelX / fromIntegral pixelsW) * fromIntegral gridW + fromIntegral seed
        gridY = (fromIntegral pixelY / fromIntegral pixelsH) * fromIntegral gridW + fromIntegral seed
        noise = perlinNoiseAtPoint gridX gridY
