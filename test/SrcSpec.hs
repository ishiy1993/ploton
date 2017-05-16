module SrcSpec (main, spec) where

import Test.Hspec

import Config
import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "replace \"$1 is $2\"" $ do
        it "replace only $1" $
            replace 1 "Haskell" "$1 is $2" `shouldBe` "Haskell is $2"
        it "replace both $1 and $2" $
            replace 1 "Haskell" (replace 2 "greate" "$1 is $2")
                `shouldBe` "Haskell is greate"
    describe "toCode" $ do
        it "simple version" $ do
            let cfg = Config { script = "$1 u 1:2"
                             , dataFiles = ["sample.dat"]
                             , format = "pdf"
                             , output = "plot_result.pdf"
                             , xlabel = Nothing
                             , ylabel = Nothing
                             , xrange = Nothing
                             , yrange = Nothing
                             , title = Nothing
                             , logx = False
                             , logy = False
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"plot \"sample.dat\" u 1:2"]
            toCode cfg `shouldBe` res
        it "full version" $ do
            let cfg = Config { script = "$1 u 1:2, $1 u 1:4, $2 u 1:2"
                             , dataFiles = ["sample1.dat", "sample2.dat"]
                             , format = "pdf"
                             , output = "plot_result.pdf"
                             , xlabel = Just "x"
                             , ylabel = Just "x^2"
                             , xrange = Just "[1:10]"
                             , yrange = Just "10:100"
                             , title = Just "sample"
                             , logx = True
                             , logy = True
                             }
                res = concat ["set term pdf;"
                             ,"set output \"plot_result.pdf\";"
                             ,"set xlabel \"x\";"
                             ,"set ylabel \"x^2\";"
                             ,"set xrange [1:10];"
                             ,"set yrange [10:100];"
                             ,"set title \"sample\";"
                             ,"set logscale x;"
                             ,"set logscale y;"
                             ,"plot \"sample1.dat\" u 1:2, \"sample1.dat\" u 1:4, \"sample2.dat\" u 1:2"]
            toCode cfg `shouldBe` res
