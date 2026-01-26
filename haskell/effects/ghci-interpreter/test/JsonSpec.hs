-- | Tests for JSON serialization of wire protocol types.
module JsonSpec (spec) where

import Data.Aeson (encode, decode, eitherDecode)
import Test.Hspec

import ExoMonad.Effect.GHCi
  ( GHCiRequest(..)
  , GHCiResponse(..)
  , GHCiError(..)
  )


spec :: Spec
spec = do
  describe "GHCiRequest" $ do

    it "round-trips ReqQueryType" $ do
      let req = ReqQueryType "fmap"
      decode (encode req) `shouldBe` Just req

    it "round-trips ReqQueryInfo" $ do
      let req = ReqQueryInfo "Monad"
      decode (encode req) `shouldBe` Just req

    it "round-trips ReqQueryKind" $ do
      let req = ReqQueryKind "Maybe"
      decode (encode req) `shouldBe` Just req

    it "round-trips ReqEvaluate" $ do
      let req = ReqEvaluate "1 + 1"
      decode (encode req) `shouldBe` Just req

    it "round-trips ReqCheckCompiles" $ do
      let req = ReqCheckCompiles "True && False"
      decode (encode req) `shouldBe` Just req

    it "round-trips ReqLoadModule" $ do
      let req = ReqLoadModule "Data.List"
      decode (encode req) `shouldBe` Just req

    it "round-trips ReqReloadModules" $ do
      let req = ReqReloadModules
      decode (encode req) `shouldBe` Just req

    it "round-trips ReqPing" $ do
      let req = ReqPing
      decode (encode req) `shouldBe` Just req

  describe "GHCiResponse" $ do

    it "round-trips RespSuccess" $ do
      let resp = RespSuccess "Functor f => (a -> b) -> f a -> f b"
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespBool True" $ do
      let resp = RespBool True
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespBool False" $ do
      let resp = RespBool False
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespUnit" $ do
      let resp = RespUnit
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespPong" $ do
      let resp = RespPong
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespError with GHCiNotConnected" $ do
      let resp = RespError GHCiNotConnected
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespError with GHCiServerError" $ do
      let resp = RespError (GHCiServerError "connection failed")
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespError with GHCiTimeout" $ do
      let resp = RespError (GHCiTimeout ":type fmap" 5000)
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespError with GHCiParseError" $ do
      let resp = RespError (GHCiParseError "bad syntax" "parse error at line 1")
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespError with GHCiLoadError" $ do
      let resp = RespError (GHCiLoadError "MyModule" "Could not find module")
      decode (encode resp) `shouldBe` Just resp

    it "round-trips RespError with GHCiSessionCrashed" $ do
      let resp = RespError (GHCiSessionCrashed "segfault" (Just 139))
      decode (encode resp) `shouldBe` Just resp

  describe "Cross-compatibility" $ do

    it "decodes GHCiRequest JSON from oracle server format" $ do
      -- Verify we can decode what the server would send
      let json = "{\"tag\":\"ReqQueryType\",\"contents\":\"fmap\"}"
      eitherDecode json `shouldBe` Right (ReqQueryType "fmap")

    it "decodes GHCiResponse JSON from oracle server format" $ do
      let json = "{\"tag\":\"RespSuccess\",\"contents\":\"Int -> Int\"}"
      eitherDecode json `shouldBe` Right (RespSuccess "Int -> Int")
