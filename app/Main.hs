module Main where

import Control.Monad (when)
import System.IO ( hPutStrLn, stderr )

import Config ( getConfig, Config(verbose, output, term) )
import Generator ( genCode )
import Utils ( plot )


main :: IO ()
main = do
    cfg <- getConfig
    let code = genCode cfg
    when (verbose cfg) $ putErr ("Code: " ++ code)
    plot code
    putStrLn $ output cfg ++ "." ++ term cfg

putErr :: String -> IO ()
putErr = hPutStrLn stderr
