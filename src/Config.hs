module Config where

import Data.Monoid ((<>))
import Data.List (intercalate, foldl')
import Data.Version (showVersion)
import Options.Applicative

import Utils

import Paths_ploton (version)

data Config = Config
    { script    :: !String
    , dataFiles :: ![String]
    , term    :: !String
    , output    :: !String
    , set       :: ![String]
    , xlabel    :: !(Maybe String)
    , ylabel    :: !(Maybe String)
    , xrange    :: !(Maybe String)
    , yrange    :: !(Maybe String)
    , xformat   :: !(Maybe String)
    , yformat   :: !(Maybe String)
    , title     :: !(Maybe String)
    , style     :: !String
    , logx      :: !Bool
    , logy      :: !Bool
    , splot     :: !Bool
    } deriving Show

getConfig :: IO Config
getConfig = execParser optsParser

optsParser :: ParserInfo Config
optsParser = info (versionInfo <*> helper <*> programOptions)
                  (fullDesc <> header "ploton")
    where
        versionInfo = abortOption (InfoMsg (showVersion version)) $
            long "version" <> help "Show version" <> hidden
        maybeStrOption mv lg =
            (\s -> if null s then Nothing else Just s) <$>
                strOption (metavar mv <> long lg <> value "" <> help mv)
        programOptions =
            Config <$> strArgument (metavar "SCRIPT" <> help "plot script")
                   <*> some (strArgument $ metavar "FILES"
                                         <> help "data ifles")
                   <*> strOption (metavar "FORMAT"
                                 <> long "term"
                                 <> value "pdf"
                                 <> help "output format"
                                 )
                   <*> strOption (metavar "OUTPUT"
                                 <> long "output"
                                 <> value "plot_result.pdf"
                                 <> help "output file name"
                                 )
                   <*> many (strOption (metavar "set ..."
                                       <> long "set"
                                       <> help "set options"
                                       ))
                   <*> maybeStrOption "xlabel" "xl"
                   <*> maybeStrOption "ylabel" "yl"
                   <*> maybeStrOption "xrange" "xr"
                   <*> maybeStrOption "yrange" "yr"
                   <*> maybeStrOption "xformat" "xf"
                   <*> maybeStrOption "yformat" "yf"
                   <*> maybeStrOption "title" "title"
                   <*> strOption (metavar "style"
                                 <> long "style"
                                 <> value "linespoints"
                                 <> help "data style"
                                 )
                   <*> switch (long "logx" <> help "logx")
                   <*> switch (long "logy" <> help "logy")
                   <*> switch (short '3' <> long "splot" <> help "Use splot")

toCode :: Config -> String
toCode cfg = intercalate ";" codes
    where maybeToCode attr = maybe [] (\s->[unwords ["set", attr, s]])
          range s | null s || head s == '[' && last s == ']' = s
                  | otherwise = "["++s++"]"
          ploting = snd $ foldl' build (1, script cfg) $ dataFiles cfg
              where build (i, scr) file = (i+1, replace i (show file) scr)
          codes = 
            [ "set term " ++ term cfg
            , "set output " ++ show (output cfg)]
            ++ maybeToCode "xlabel" (show <$> xlabel cfg)
            ++ maybeToCode "ylabel" (show <$> ylabel cfg)
            ++ maybeToCode "xrange" (range <$> xrange cfg)
            ++ maybeToCode "yrange" (range <$> yrange cfg)
            ++ maybeToCode "format x" (show <$> xformat cfg)
            ++ maybeToCode "format y" (show <$> yformat cfg)
            ++ maybeToCode "title" (show <$> title cfg)
            ++ ["set style data " ++ style cfg]
            ++ ["set logscale x" | logx cfg]
            ++ ["set logscale y" | logy cfg]
            ++ ["set pm3d map" | splot cfg]
            ++ ["set " ++ opt | opt <- set cfg]
            ++ ["splot " ++ ploting | splot cfg]
            ++ ["plot " ++ ploting | not (splot cfg)]
