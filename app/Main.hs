module Main where

import Perlin

import System.Environment
import Codec.Picture
import System.Random

-- Default constants
maxRandomSeed     = 10000
defaultWidth      = 1000
defaultHeight     = 1000
defaultNoise      = 3
defaultSharpness  = 4.0

data Settings 
  = Settings { width     :: Int
             , height    :: Int
             , fileName  :: String 
             , noise     :: Int
             , sharpness :: Double
             , seed      :: Int
             }

parseArgs :: [String] -> IO Settings
parseArgs [f] = do
  r <- randomRIO (0, maxRandomSeed)
  print r
  return Settings { width     = defaultWidth
                  , height    = defaultHeight
                  , fileName  = f
                  , noise     = defaultNoise
                  , sharpness = defaultSharpness
                  , seed      = r
                  }
parseArgs ("-d" : w : h : args) = do
  parseArgs args >>= \s' -> return Settings { width     = read w
                                            , height    = read h
                                            , fileName  = fileName s'
                                            , noise     = noise s'
                                            , sharpness = sharpness s'
                                            , seed      = seed s'
                                            }
parseArgs ("-n" : n : args) = do
  parseArgs args >>= \s' -> return Settings { width     = width s'
                                            , height    = height s'
                                            , fileName  = fileName s'
                                            , noise     = read n
                                            , sharpness = sharpness s'
                                            , seed      = seed s'
                                            }
parseArgs ("-s" : s : args) = do
  parseArgs args >>= \s' -> return Settings { width     = width s'
                                            , height    = height s'
                                            , fileName  = fileName s'
                                            , noise     = noise s'
                                            , sharpness = read s
                                            , seed      = seed s'
                                            }
parseArgs ("-seed" : s : args) = do
  parseArgs args >>= \s' -> return Settings { width     = width s'
                                            , height    = height s'
                                            , fileName  = fileName s'
                                            , noise     = noise s'
                                            , sharpness = sharpness s'
                                            , seed      = read s
                                            }
parseArgs _ 
  = error "Bad arguements. Use -h for help."

main :: IO ()
main = do
  args <- getArgs
  s <- parseArgs args
  writePng 
    (fileName s) $ 
    generatePerlinImage (width s) (height s) (noise s) (noise s) (sharpness s) (seed s)
