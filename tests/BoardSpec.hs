module BoardSpec (spec) where

import           Board
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "board" $ do
        describe "get" $ do
            context "when getting a slot where board does not have a value" $ do
                it "returns Nothing" $ do
                    get (fromList [Just 1, Nothing, Nothing]) 2 `shouldBe` Nothing
            context "when getting a slot where board has a value" $ do
                it "returns Just the value" $ do
                    get (fromList [Just 1, Nothing, Nothing]) 1 `shouldBe` Just 1
        describe "add" $ do
            context "when there is no space to add" $ do
                it "returns an error message" $ do
                    add 1 (fromList [Just 1, Just 1, Just 1]) `shouldBe` Left "Board is full"
            context "when there is space to add in the beginning" $ do
                it "returns a new board with the element added" $ do
                    add 1 (fromList [Nothing, Just 1, Just 1]) `shouldBe` Right (fromList [Just 1, Just 1, Just 1])
            context "when there is space to add in the middle" $ do
                it "returns a new board with the element added" $ do
                    add 1 (fromList [Just 1, Nothing, Just 1]) `shouldBe` Right (fromList [Just 1, Just 1, Just 1])
