module SrcSpec (main, spec) where

import           Test.Hspec

import           Config
import           Generator  (genCode)
import           Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "replace \"#1 is #2\"" $ do
        it "replace only $1" $
            replace 1 "Haskell" "#1 is #2" `shouldBe` "Haskell is #2"
        it "replace both $1 and $2" $
            replace 1 "Haskell" (replace 2 "greate" "#1 is #2")
                `shouldBe` "Haskell is greate"
    describe "genCode" $ do
        it "simple version" $ do
            let cfg = Config { script = "#1 u 1:2"
                             , dataFiles = ["sample.dat"]
                             , term = "pdf"
                             , output = "plot_result"
                             , setting = []
                             , xlabel = Nothing
                             , ylabel = Nothing
                             , xrange = Nothing
                             , yrange = Nothing
                             , xformat = Nothing
                             , yformat = Nothing
                             , title = Nothing
                             , multi = Nothing
                             , multiTitle = Nothing
                             , style = "linespoints"
                             , color = ""
                             , logx = False
                             , logy = False
                             , splot = False
                             , verbose = False
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set tmargin 3;"
                             ,"set bmargin 4;"
                             ,"set lmargin 5;"
                             ,"set rmargin 2;"
                             ,"set key at screen 1,0.1 opaque;"
                             ,"set style data linespoints;"
                             ,"plot \"sample.dat\" u 1:2;"]
            genCode cfg `shouldBe` res
        it "full version" $ do
            let cfg = Config { script = "#1 u 1:2, #1 u 1:4, #2 u 1:2"
                             , dataFiles = ["sample1.dat", "sample2.dat"]
                             , term = "pdf"
                             , output = "plot_result"
                             , setting = []
                             , xlabel = Just "x"
                             , ylabel = Just "x^2"
                             , xrange = Just "[1:10]"
                             , yrange = Just "10:100"
                             , xformat = Just "%E"
                             , yformat = Just "%.1E"
                             , title = Just "sample"
                             , multi = Nothing
                             , multiTitle = Nothing
                             , style = "linespoints"
                             , color = ""
                             , logx = True
                             , logy = True
                             , splot = False
                             , verbose = False
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set tmargin 3;"
                             ,"set bmargin 4;"
                             ,"set lmargin 5;"
                             ,"set rmargin 2;"
                             ,"set key at screen 1,0.1 opaque;"
                             ,"set xrange [1:10];"
                             ,"set yrange [10:100];"
                             ,"set format x \"%E\";"
                             ,"set format y \"%.1E\";"
                             ,"set style data linespoints;"
                             ,"set logscale x;"
                             ,"set logscale y;"
                             ,"set title \"sample\";"
                             ,"set xlabel \"x\";"
                             ,"set ylabel \"x^2\";"
                             ,"plot \"sample1.dat\" u 1:2, \"sample1.dat\" u 1:4, \"sample2.dat\" u 1:2;"]
            genCode cfg `shouldBe` res
        it "3d simple version" $ do
            let cfg = Config { script = "#1 with pm3d"
                             , dataFiles = ["sample.dat"]
                             , term = "pdf"
                             , output = "plot_result"
                             , setting = []
                             , xlabel = Nothing
                             , ylabel = Nothing
                             , xrange = Nothing
                             , yrange = Nothing
                             , xformat = Nothing
                             , yformat = Nothing
                             , title = Nothing
                             , multi = Nothing
                             , multiTitle = Nothing
                             , style = "linespoints"
                             , color = ""
                             , logx = False
                             , logy = False
                             , splot = True
                             , verbose = False
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set tmargin 3;"
                             ,"set bmargin 4;"
                             ,"set lmargin 5;"
                             ,"set rmargin 2;"
                             ,"set key at screen 1,0.1 opaque;"
                             ,"set style data linespoints;"
                             ,"set pm3d map;"
                             ,"set palette ;"
                             ,"splot \"sample.dat\" with pm3d;"]
            genCode cfg `shouldBe` res
        it "multi 2d version" $ do
            let cfg = Config { script = "#1 u 1:2;#1 u 1:3"
                             , dataFiles = ["sample.dat"]
                             , term = "pdf"
                             , output = "plot_result"
                             , setting = []
                             , xlabel = Nothing
                             , ylabel = Nothing
                             , xrange = Nothing
                             , yrange = Nothing
                             , xformat = Nothing
                             , yformat = Nothing
                             , title = Nothing
                             , multi = Just "1,2"
                             , multiTitle = Nothing
                             , style = "linespoints"
                             , color = ""
                             , logx = False
                             , logy = False
                             , splot = False
                             , verbose = False
                             }
                res = concat ["set term pdf size 8in,3in;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set tmargin 3;"
                             ,"set bmargin 4;"
                             ,"set lmargin 5;"
                             ,"set rmargin 2;"
                             ,"set key at screen 1,0.1 opaque;"
                             ,"set style data linespoints;"
                             ,"set multiplot layout 1,2;"
                             ,"plot \"sample.dat\" u 1:2;"
                             ,"plot \"sample.dat\" u 1:3;"
                             ]
            genCode cfg `shouldBe` res
        it "multi 3d version" $ do
            let cfg = Config { script = "#1 u 1:2:3;#1 u 1:2:4;#2 u 1:2:3"
                             , dataFiles = ["sample1.dat", "sample2.dat"]
                             , term = "pdf"
                             , output = "plot_result"
                             , setting = []
                             , xlabel = Just "x"
                             , ylabel = Just "{/Symbol r};u;v"
                             , xrange = Nothing
                             , yrange = Nothing
                             , xformat = Nothing
                             , yformat = Nothing
                             , title = Just "r;u;v"
                             , multi = Just "1,3"
                             , multiTitle = Just "sample"
                             , style = "linespoints"
                             , color = ""
                             , logx = False
                             , logy = False
                             , splot = True
                             , verbose = False
                             }
                res = concat ["set term pdf size 12in,3in;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set tmargin 3;"
                             ,"set bmargin 4;"
                             ,"set lmargin 5;"
                             ,"set rmargin 2;"
                             ,"set key at screen 1,0.1 opaque;"
                             ,"set style data linespoints;"
                             ,"set pm3d map;"
                             ,"set palette ;"
                             ,"set multiplot layout 1,3 title \"sample\";"
                             ,"set title \"r\";"
                             ,"set xlabel \"x\";"
                             ,"set ylabel \"{/Symbol r}\";"
                             ,"splot \"sample1.dat\" u 1:2:3;"
                             ,"set title \"u\";"
                             ,"set ylabel \"u\";"
                             ,"splot \"sample1.dat\" u 1:2:4;"
                             ,"set title \"v\";"
                             ,"set ylabel \"v\";"
                             ,"splot \"sample2.dat\" u 1:2:3;"
                             ]
            genCode cfg `shouldBe` res
