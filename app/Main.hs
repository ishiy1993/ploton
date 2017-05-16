module Main where

import Config
import Utils

main :: IO ()
main = do
    cfg <- getConfig
    let code = toCode cfg
    plot code
    putStrLn $ output cfg
