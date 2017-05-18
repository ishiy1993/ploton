module Utils where

import Control.Exception (onException)
import Data.Char (intToDigit)
import System.Exit (die)
import System.Process (callCommand)

replace :: Int -> String -> String -> String
replace i str "" = ""
replace i str ('#':c:res)
    | c == intToDigit i = str ++ replace i str res
    | otherwise = ['#',c] ++ replace i str res
replace i str (s:ss) = s:replace i str ss

plot :: String -> IO ()
plot code = do
    let command = "gnuplot -e '" ++ code ++ "'"
    callCommand command
        `onException` die (unlines ["Error:", code])
