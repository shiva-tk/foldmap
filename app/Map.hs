module Map (shade) where

import Codec.Picture

shade :: Image Pixel8 -> Image Pixel8 -> Image Pixel8 -> Image PixelRGB8
shade depth temp precip = generateImage pixelShader width height
  where width       = minimum [imageWidth  depth, imageWidth  temp, imageWidth  precip]
        height      = minimum [imageHeight depth, imageHeight temp, imageHeight precip]
        pixelShader :: Int -> Int -> PixelRGB8 
        pixelShader x y 
          | depthPixel < waterLevel = shadeWaterPixel   waterShades depthPixel
          | otherwise               = shadeTerrainPixel terrainShades tempPixel precipPixel
          where depthPixel  = pixelAt depth  x y
                tempPixel   = pixelAt temp   x y
                precipPixel = pixelAt precip x y

shadeWaterPixel :: [PixelRGB8] -> Pixel8 -> PixelRGB8
shadeWaterPixel colours depth 
  | depth < waterLevel = colours !! fromIntegral index
  | otherwise          = PixelRGB8 depth depth depth
  where depthRange = waterLevel `div` fromIntegral (length colours)
        index      = depth      `div` depthRange

shadeTerrainPixel :: [[PixelRGB8]] -> Pixel8 -> Pixel8 -> PixelRGB8
shadeTerrainPixel colours temp precip
  | temp < maxPixel8 
    = if precip < maxPixel8
        then colours !! fromIntegral tempIndex !! fromIntegral precipIndex
        else colours !! fromIntegral tempIndex !! (noPrecips - 1)
  | otherwise 
    = if precip < maxPixel8
        then colours !! (noTemps - 1)          !! fromIntegral precipIndex
        else colours !! (noTemps - 1)          !! (noPrecips - 1)
  where tempRange   = maxPixel8 `div` fromIntegral noTemps
        precipRange = maxPixel8 `div` fromIntegral noPrecips
        tempIndex   = temp      `div` tempRange
        precipIndex = precip    `div` precipRange
        noTemps     = length colours
        noPrecips   = length $ head colours

-- Constants
maxPixel8 :: Pixel8
maxPixel8 = 255

water0, water1, water2, water3, water4 :: PixelRGB8 
water0 = PixelRGB8 0   13  40
water1 = PixelRGB8 13  23  56
water2 = PixelRGB8 13  29  68
water3 = PixelRGB8 13  34  83
water4 = PixelRGB8 23  39  90

waterShades :: [PixelRGB8]
waterShades =  [water0, water1, water2, water3, water4]

waterLevel :: Pixel8
waterLevel =  100

-- Naming convention: temperaturePrecipitaionLevel
coldLow, coldMid, coldHigh :: PixelRGB8
coldLow  = PixelRGB8 79  83  80
coldMid  = PixelRGB8 29  74  79
coldHigh = PixelRGB8 73  90  124

warmLow, warmMid, warmHigh :: PixelRGB8
warmLow  = PixelRGB8 42  79  74
warmMid  = PixelRGB8 20  73  77
warmHigh = PixelRGB8 15  69  79

hotLow, hotMid, hotHigh :: PixelRGB8
hotLow  = PixelRGB8 86  96  89
hotMid  = PixelRGB8 80  93  85
hotHigh = PixelRGB8 43  93  82

cold, warm, hot :: [PixelRGB8]
cold = [coldLow, coldMid, coldHigh]
warm = [warmLow, warmMid, warmHigh]
hot  = [hotLow,  hotMid,  hotHigh ]

terrainShades :: [[PixelRGB8]]
terrainShades =  [cold, warm, hot]
