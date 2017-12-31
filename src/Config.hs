module Config where

import Data.Monoid ((<>))
import Data.List (intercalate, foldl')
import Data.Version (showVersion)
import Options.Applicative hiding (style)

import Utils

import Paths_ploton (version)

data Config = Config
    { script    :: !String
    , dataFiles :: ![String]
    , term      :: !String
    , output    :: !String
    , setting   :: ![String]
    , xlabel    :: !(Maybe String)
    , ylabel    :: !(Maybe String)
    , xrange    :: !(Maybe String)
    , yrange    :: !(Maybe String)
    , xformat   :: !(Maybe String)
    , yformat   :: !(Maybe String)
    , title     :: !(Maybe String)
    , multi     :: !(Maybe String)
    , multiTitle :: !(Maybe String)
    , style     :: !String
    , color     :: !String
    , logx      :: !Bool
    , logy      :: !Bool
    , splot     :: !Bool
    , verbose   :: !Bool
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
                   <*> maybeStrOption "multi" "multi"
                   <*> maybeStrOption "multi title" "multi-title"
                   <*> strOption (metavar "style"
                                 <> long "style"
                                 <> value "linespoints"
                                 <> help "data style"
                                 )
                   <*> strOption (metavar "color map"
                                 <> long "color"
                                 <> value ""
                                 <> help "pm3d map color"
                                 )
                   <*> switch (long "logx" <> help "logx")
                   <*> switch (long "logy" <> help "logy")
                   <*> switch (short '3' <> long "splot" <> help "Use splot")
                   <*> switch (long "verbose" <> help "output gnuplot script")
