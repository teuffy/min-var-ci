module Data.Conduit.AnalysisSpec where

import Test.Hspec
import Data.Conduit
import qualified Data.Conduit.Analysis as CA
import qualified Control.Lens as L
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.List
import Data.Ord (comparing)

main :: IO ()
main = hspec spec

data Person = Person
    { _personName    :: String
    , _personAge     :: Int
    , _personCountry :: String
    }

age :: L.Lens' Person Int
age = L.lens _personAge $ \p a -> p { _personAge = a }

country :: L.Lens' Person String
country = L.lens _personCountry $ \p a -> p { _personCountry = a }

spec :: Spec
spec = describe "Data.Conduit.Analysis" $ do
    describe "groupBy" $ do
        let input =
                [ Person "US1" 10 "USA"
                , Person "IS1" 10 "Israel"
                , Person "US2" 20 "USA"
                , Person "IS2" 30 "Israel"
                ]
            myAverage _ country' = do
                stats <- CA.stats age
                case CA.statsMean $ fmap fromIntegral stats of
                    Nothing -> return ()
                    Just avg -> yield (country', avg)
            sink = CL.fold (\m (c, a) -> HM.insert c a m) HM.empty
            expected = HM.fromList
                [ ("USA", 15 :: Double)
                , ("Israel", 20)
                ]
        it "presorted" $ do
            let input' = Data.List.sortBy (comparing _personCountry) input
                src = mapM_ yield input'
            res <- runResourceT $ src $$ CA.groupBy CA.PreSorted country myAverage =$ sink
            res `shouldBe` expected
        it "sort then group" $ do
            let src = mapM_ yield input
            res <- runResourceT $ src $$ CA.groupBy CA.SortThenGroup country myAverage =$ sink
            res `shouldBe` expected
        it "coroutines" $ do
            let src = mapM_ yield input
            res <- runResourceT $ src $$ CA.groupBy CA.Coroutines country myAverage =$ sink
            res `shouldBe` expected
            {-
        it "threads" $ do
            let src = mapM_ yield input
            res <- runResourceT $ src $$ CA.groupBy CA.Threads country myAverage =$ sink
            res `shouldBe` expected
            -}
        it "smoothDeltaAbnormalities" $ do
            let src = mapM_ yield [1.0, 1.2, 1.0, 1.3, 1.4, 85, 1.3, 1.7, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3]
            res <- runResourceT $ src $$ CA.smoothDeltaAbnormalities id =$ CL.consume
            res `shouldBe` [1.0, 1.2, 1.0, 1.3, 1.4, 1.35, 1.3, 1.7, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3]