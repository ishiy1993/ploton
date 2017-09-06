module SrcSpec (main, spec) where

import Test.Hspec

import Config
import Utils

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
    describe "toCode" $ do
        it "simple version" $ do
            let cfg = Config { script = "#1 u 1:2"
                             , dataFiles = ["sample.dat"]
                             , term = "pdf"
                             , output = "plot_result.pdf"
                             , set = []
                             , xlabel = Nothing
                             , ylabel = Nothing
                             , xrange = Nothing
                             , yrange = Nothing
                             , xformat = Nothing
                             , yformat = Nothing
                             , title = Nothing
                             , style = "linespoints"
                             , color = ""
                             , logx = False
                             , logy = False
                             , splot = False
                             , verbose = False
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set style data linespoints;"
                             ,"plot \"sample.dat\" u 1:2"]
            toCode cfg `shouldBe` res
        it "full version" $ do
            let cfg = Config { script = "#1 u 1:2, #1 u 1:4, #2 u 1:2"
                             , dataFiles = ["sample1.dat", "sample2.dat"]
                             , term = "pdf"
                             , output = "plot_result.pdf"
                             , set = []
                             , xlabel = Just "x"
                             , ylabel = Just "x^2"
                             , xrange = Just "[1:10]"
                             , yrange = Just "10:100"
                             , xformat = Just "%E"
                             , yformat = Just "%.1E"
                             , title = Just "sample"
                             , style = "linespoints"
                             , color = ""
                             , logx = True
                             , logy = True
                             , splot = False
                             , verbose = False
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set xlabel \"x\";"
                             ,"set ylabel \"x^2\";"
                             ,"set xrange [1:10];"
                             ,"set yrange [10:100];"
                             ,"set format x \"%E\";"
                             ,"set format y \"%.1E\";"
                             ,"set title \"sample\";"
                             ,"set style data linespoints;"
                             ,"set logscale x;"
                             ,"set logscale y;"
                             ,"plot \"sample1.dat\" u 1:2, \"sample1.dat\" u 1:4, \"sample2.dat\" u 1:2"]
            toCode cfg `shouldBe` res
        it "3d simple version" $ do
            let cfg = Config { script = "#1 with pm3d"
                             , dataFiles = ["sample.dat"]
                             , term = "pdf"
                             , output = "plot_result.pdf"
                             , set = []
                             , xlabel = Nothing
                             , ylabel = Nothing
                             , xrange = Nothing
                             , yrange = Nothing
                             , xformat = Nothing
                             , yformat = Nothing
                             , title = Nothing
                             , style = "linespoints"
                             , color = ""
                             , logx = False
                             , logy = False
                             , splot = True
                             , verbose = False
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set style data linespoints;"
                             ,"set pm3d map;"
                             ,"set palette ;"
                             ,"splot \"sample.dat\" with pm3d"]
            toCode cfg `shouldBe` res
