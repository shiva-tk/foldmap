module Shader where

import Perlin (interpolate)

import Codec.Picture

class Shader s where
  shadePixel :: s -> Pixel8 ->PixelRGB8
  shade      :: s -> Image Pixel8 -> Image PixelRGB8
  shade a    =  pixelMap (shadePixel a)

data GradientShader
  = GradientShader { start         :: PixelRGB8
                   , end           :: PixelRGB8
                   , gradientStart :: Pixel8
                   , gradientEnd   :: Pixel8
                   }

gradientShadePixel :: GradientShader -> Pixel8 -> PixelRGB8
gradientShadePixel gr p
  | inRange   = PixelRGB8 r g b
  | otherwise = PixelRGB8 p p p
  where PixelRGB8 sr sg sb = start gr
        PixelRGB8 er eg eb = end   gr
        inRange            = gradientStart gr <= p && p <= gradientEnd gr
        gradientLength     = gradientEnd gr - gradientStart gr
        w                  = fromIntegral (p - gradientStart gr) / fromIntegral gradientLength
        r                  = round $ interpolate (fromIntegral sr) (fromIntegral er) w
        g                  = round $ interpolate (fromIntegral sg) (fromIntegral eg) w
        b                  = round $ interpolate (fromIntegral sb) (fromIntegral eb) w

instance Shader GradientShader where
  shadePixel = gradientShadePixel

data BasicTerrainShader
  = BasicTerrainShader { waterShader :: GradientShader
                       , sandShader  :: GradientShader
                       , landShader  :: GradientShader
                       , waterLevel  :: Pixel8
                       , sandLevel   :: Pixel8
                       }

basicTerrainShadePixel :: BasicTerrainShader -> Pixel8 -> PixelRGB8
basicTerrainShadePixel bs p
  | isWater        = shadePixel (waterShader bs) p
  | isSand         = shadePixel (sandShader  bs) p
  | otherwise      = shadePixel (landShader  bs) p
  where isWater    = p <=waterLevel bs
        isSand     = p > waterLevel bs && p <=sandLevel bs

instance Shader BasicTerrainShader where
  shadePixel = basicTerrainShadePixel

-- Preset shaders and colours
black, white            :: PixelRGB8
sandLow, sandHigh       :: PixelRGB8
waterDeep, waterShallow :: PixelRGB8
landLow, landHigh       :: PixelRGB8
black        = PixelRGB8 0   0   0
white        = PixelRGB8 255 255 255
sandLow      = PixelRGB8 248 255 162
sandHigh     = PixelRGB8 255 211 26
waterDeep    = PixelRGB8 47  0   255
waterShallow = PixelRGB8 137 229 255
landLow      = PixelRGB8 37  255 26
landHigh     = PixelRGB8 162 255 206

sand, water, land :: GradientShader
sand  = GradientShader { start         = sandLow
                       , end           = sandHigh
                       , gradientStart = defaultWaterLevel + 1
                       , gradientEnd   = defaultSandLevel
                       }
water = GradientShader { start         = waterDeep
                       , end           = waterShallow
                       , gradientStart = 0
                       , gradientEnd   = defaultWaterLevel
                       }
land  = GradientShader { start         = landLow
                       , end           = landHigh
                       , gradientStart = defaultSandLevel + 1
                       , gradientEnd   = 255
                       }

defaultWaterLevel, defaultSandLevel :: Pixel8
defaultWaterLevel = 122
defaultSandLevel  = 130

terrain :: BasicTerrainShader
terrain =  BasicTerrainShader { waterShader = water
                              , sandShader  = sand
                              , landShader  = land
                              , waterLevel  = defaultWaterLevel
                              , sandLevel   = defaultSandLevel
                              }
