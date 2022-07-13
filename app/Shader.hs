module Shader where

import Codec.Picture

class Shader a where
  shade :: a -> Image Pixel8 -> Image PixelRGB8

data GradientShader
  = GradientShader { start :: PixelRGB8
                   , end   :: PixelRGB8
                   }

gradientShadePixel :: GradientShader -> Pixel8 -> PixelRGB8
gradientShadePixel gr p = PixelRGB8 r g b
  where PixelRGB8 sr sg sb = start gr
        PixelRGB8 er eg eb = end   gr
        maxPixel8          = 255.0
        w                  = fromIntegral p / maxPixel8
        r                  = floor (fromIntegral (er - sr) * w) + sr
        g                  = floor (fromIntegral (eg - sg) * w) + sg
        b                  = floor (fromIntegral (eb - sb) * w) + sb

instance Shader GradientShader where
  shade s i = pixelMap (gradientShadePixel s) i
