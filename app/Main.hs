module Main where

import Perlin
import Shader

import Codec.Picture
import System.Environment
import System.Exit
import System.Random

-- Default constants
maxRandomSeed     = 10000
defaultWidth      = 1000
defaultHeight     = 1000
defaultNoise      = 3
defaultSharpness  = 10.0

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
  return Settings { width     = defaultWidth
                  , height    = defaultHeight
                  , fileName  = f
                  , noise     = defaultNoise
                  , sharpness = defaultSharpness
                  , seed      = r
                  }
-- Dimensions: -d
parseArgs ("-d" : w : h : args) = do
  s' <- parseArgs args
  return s' {width = read w, height = read h}
-- Noise:      -n
parseArgs ("-n" : n : args) = do
  s' <- parseArgs args
  return s' {noise = read n}
-- Sharpness:  -s
parseArgs ("-s" : s : args) = do
  s' <- parseArgs args
  return s' {sharpness = read s}
-- Seed:       -seed
parseArgs ("-seed" : s : args) = do
  s' <- parseArgs args
  return s' {seed = read s}
parseArgs _ 
  = print "Bad arguements. Use -h for help." >> exitFailure

main :: IO ()
main = do
  args   <- getArgs
  s      <- parseArgs args
  writePng 
    (fileName s)  $ 
    shade terrain $
    generatePerlinImage (width s) (height s) (noise s) (noise s) (sharpness s) (seed s)
