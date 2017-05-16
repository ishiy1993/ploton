module Config where

import Data.Monoid ((<>))
import Data.List (intercalate, foldl')
import Options.Applicative

import Utils

data Config = Config
    { script    :: !String
    , dataFiles :: ![String]
    , format    :: !String
    , output    :: !String
    , xlabel    :: !(Maybe String)
    , ylabel    :: !(Maybe String)
    , xrange    :: !(Maybe String)
    , yrange    :: !(Maybe String)
    , title     :: !(Maybe String)
    , logx      :: !Bool
    , logy      :: !Bool
    } deriving Show

getConfig :: IO Config
getConfig = execParser optsParser

optsParser :: ParserInfo Config
optsParser = info (helper <*> programOptions)
                  (fullDesc <> header "ploton")
    where
        maybeStrOption mv lg =
            (\s -> if null s then Nothing else Just s) <$>
                strOption (metavar mv <> long lg <> value "" <> help mv)
        programOptions =
            Config <$> strArgument (metavar "SCRIPT" <> help "plot script")
                   <*> some (strArgument $ metavar "FILES"
                                         <> help "data ifles")
                   <*> strOption (metavar "FORMAT"
                                 <> long "fmt"
                                 <> value "pdf"
                                 <> help "output format"
                                 )
                   <*> strOption (metavar "OUTPUT"
                                 <> long "output"
                                 <> value "plot_result.pdf"
                                 <> help "output file name"
                                 )
                   <*> maybeStrOption "xlabel" "xl"
                   <*> maybeStrOption "ylabel" "yl"
                   <*> maybeStrOption "xrange" "xr"
                   <*> maybeStrOption "yrange" "yr"
                   <*> maybeStrOption "title" "title"
                   <*> switch (help "logx")
                   <*> switch (help "logy")

toCode :: Config -> String
toCode cfg = intercalate ";" $ [ "set term " ++ format cfg
                               , "set output " ++ show (output cfg)]
                             ++ maybeToCode "xlabel" (xlabel cfg)
                             ++ maybeToCode "ylabel" (ylabel cfg)
                             ++ maybeToCode "xrange" (xrange cfg)
                             ++ maybeToCode "yrange" (yrange cfg)
                             ++ if logx cfg then ["set logscale x"] else []
                             ++ if logy cfg then ["set logscale y"] else []
                             ++ ["plot " ++ ploting]
    where maybeToCode attr = maybe [] (\s->[unwords ["set", attr, show s]])
          ploting = snd $ foldl' build (1, script cfg) $ dataFiles cfg
              where build (i, scr) file = (i+1, replace i (show file) scr)
