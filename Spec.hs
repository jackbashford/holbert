import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "dummy test" $ do
    it "1+1=2" $ do
      1 + 1 `shouldBe` 2
