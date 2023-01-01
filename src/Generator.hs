{-# LANGUAGE TypeApplications #-}
module Generator where

import           Control.Monad
import           Control.Monad.Trans.Writer
import           Data.List                  (foldl', isPrefixOf, zip4)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (isJust)

import           Config
import           Utils

genCode :: Config -> String
genCode cfg = execWriter $ do
  case calcSize (term cfg) <$> multi cfg of
    Nothing -> set $ "term " ++ term cfg
    Just s  -> set $ "term " ++ term cfg ++ " size " ++ s
  set $ "output " ++ dquote (output cfg ++ "." ++ term cfg)
  set "tmargin 3"
  set "bmargin 4"
  set "lmargin 5"
  set "rmargin 2"
  set "key at screen 1,0.1 opaque"
  setMaybe "xrange" (range <$> xrange cfg)
  setMaybe "yrange" (range <$> yrange cfg)
  setMaybe "format x" (dquote <$> xformat cfg)
  setMaybe "format y" (dquote <$> yformat cfg)
  set $ "style data " ++ style cfg
  when (logx cfg) $ set "logscale x"
  when (logy cfg) $ set "logscale y"
  when (splot cfg) $ do
    set "pm3d map"
    set $ "palette " ++ colorToCode (color cfg)
  mapM_ set $ setting cfg
  let withTitle = maybe id (\t -> (++ (" title " ++ dquote t))) (multiTitle cfg)
  setMaybe "multiplot layout" (withTitle <$> multi cfg)
  let body = makeScripts (splot cfg) (script cfg) (dataFiles cfg)
      ts = split $ title cfg
      xl = split $ xlabel cfg
      yl = split $ ylabel cfg
  forM_ (zip4 ts xl yl body) $ \(t, x, y, b) -> do
    setMaybe "title" (dquote <$> t)
    setMaybe "xlabel" (dquote <$> x)
    setMaybe "ylabel" (dquote <$> y)
    tell' b

calcSize :: String -> String -> String
calcSize term xy | "png" `isPrefixOf` term = show (pngUnit*y) ++ "," ++ show (pngUnit*x)
                 | "pdf" `isPrefixOf` term = show (4*y) ++ "in," ++ show (3*x) ++ "in"
                 | otherwise = show (svgUnit*y) ++ "," ++ show (svgUnit*x)
  where (x',y') = drop 1 <$> break (==',') xy
        x = read @Int x'
        y = read @Int y'
        pngUnit = 512
        svgUnit = 350

range :: String -> String
range s | null s || head s == '[' && last s == ']' = s
        | otherwise = "["++s++"]"

colorToCode :: String -> String
colorToCode "jet" =
    "define (0\"#000090\",1\"#000fff\",2\"#0090ff\",3\"#0fffee\",4\"#90ff70\",5\"#ffee00\",6\"#ff7000\",7\"#ee0000\",8\"#7f0000\")"
colorToCode "light" =
    "define (1\"#0000ff\",2\"#0080ff\",3\"#00ffff\",4\"#00ff80\",5\"#00ff00\",6\"#80ff00\",7\"#ffff00\",8\"#ff8000\",9\"#ff0000\")"
colorToCode "nizi" = "rgb 33,13,10"
colorToCode _ = ""

makeScripts :: Bool -> String -> [String] -> [String]
makeScripts b s fs = map build $ splitOn ";" $ replaceFilename s fs
  where build = if b then ("splot " ++) else ("plot " ++)

replaceFilename :: String -> [String] -> String
replaceFilename script files = snd $ foldl' build (1, script) files
  where build (i, scr) file = (i+1, replace i (dquote file) scr)

set :: String -> Writer String ()
set code = tell $ "set " ++ code ++ ";"

setMaybe :: String -> Maybe String -> Writer String ()
setMaybe attr Nothing  = return ()
setMaybe attr (Just s) = set $ attr ++ " " ++ s

tell' :: String -> Writer String ()
tell' str = tell $ str ++ ";"

split :: Maybe String -> [Maybe String]
split Nothing   = repeat Nothing
split (Just xs) = [Just x | x <- splitOn ";" xs] ++ repeat Nothing

dquote :: String -> String
dquote s = "\"" ++ s ++ "\""
