module Perlin where

import Data.Bits
import Codec.Picture

type Vector2D = (Double, Double)

interpolate :: Double -> Double -> Double -> Double
interpolate a0 a1 w = (a1 - a0) * w + a0

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
    i0 = interpolate n1 n0 sx

    m0 = dotGridGradient x0 y1 x y
    m1 = dotGridGradient x1 y1 x y
    i1 = interpolate m1 m0 sx

    value = interpolate i0 i1 sy

generatePerlinImage :: Int -> Int -> Int -> Int -> Image Pixel8
generatePerlinImage pixelsW pixelsH gridW gridH 
  = generateImage generatePerlinPixel pixelsW pixelsH
  where
    generatePerlinPixel :: Int -> Int -> Pixel8
    generatePerlinPixel pixelX pixelY
      = floor $ 255 * (noise + 1) / 2 
      where
        gridX = (fromIntegral pixelX / fromIntegral pixelsW) * fromIntegral gridW
        gridY = (fromIntegral pixelY / fromIntegral pixelsH) * fromIntegral gridW
        noise = perlinNoiseAtPoint gridX gridY


-- type Grid a = Vector (Vector a)
--
-- perlinPixels :: Int -> Int -> Int -> Int -> Grid Double
-- perlinPixels pixelsW pixelsH gridW gridH
--   = V.generate pixelsH genRow
--   where
--     genRow :: Int -> Vector Double
--     genRow row
--       = V.generate pixelsW (\c -> perlinNoiseAtPoint (col c) row')
--       where
--         row' = (fromIntegral row / fromIntegral pixelsH) * fromIntegral gridH
--         col  = \c -> (fromIntegral c / fromIntegral pixelsW) * fromIntegral gridW
