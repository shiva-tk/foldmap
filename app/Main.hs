module Main where

import Perlin

import System.Environment
import Codec.Picture

data Settings 
  = Settings { width     :: Int
             , height    :: Int
             , fileName  :: String 
             , noise     :: Int
             , sharpness :: Double
             }

parseArgs :: [String] -> Settings
parseArgs [f] 
  = Settings { width     = 500
             , height    = 500
             , fileName  = f
             , noise     = 5
             , sharpness = 2.0
             }

parseArgs ("-d" : w : h : args) 
  = Settings { width     = read w
             , height    = read h
             , fileName  = fileName s'
             , noise     = noise s'
             , sharpness = sharpness s'
             }
  where s' = parseArgs args

parseArgs ("-n" : n : args) 
  = Settings { width     = width s'
             , height    = height s'
             , fileName  = fileName s'
             , noise     = read n
             , sharpness = sharpness s'
             }
  where s' = parseArgs args

parseArgs ("-s" : s : args) 
  = Settings { width     = width s'
             , height    = height s'
             , fileName  = fileName s'
             , noise     = noise s'
             , sharpness = read s
             }
  where s' = parseArgs args

parseArgs _ 
  = error "Bad arguements. Use -h for help."

main :: IO ()
main = do
  args <- getArgs
  let s = parseArgs args
  writePng 
    (fileName s) $ 
    generatePerlinImage (width s) (height s) (noise s) (noise s) (sharpness s)
