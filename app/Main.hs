module Main where

import Control.Monad (when)
import System.IO

import Config
import Generator
import Utils

main :: IO ()
main = do
    cfg <- getConfig
    let code = genCode cfg
    when (verbose cfg) $ putErr ("Code: " ++ code)
    plot code
    putStrLn $ output cfg

putErr :: String -> IO ()
putErr = hPutStrLn stderr
