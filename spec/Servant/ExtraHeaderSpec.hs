module Servant.ExtraHeaderSpec where

import Test.Hspec
import Test.Hspec.Wai
import Servant.Server
import Data.Proxy
import Servant

import Servant.ExtraHeader

type TestAPI = ExtraHeader "X-Test-Header" String :> GetNoContent

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ExtraHeader" do

    with testApplication do

      it "returns a response with X-Test-Header" do
        get "/" `shouldRespondWith` 204 {
          matchHeaders = [ "X-Test-Header" <:> "Example header value" ]
        }

testApplication :: IO Application
testApplication = do
  testApplicationContext <- mkTestApplicationContext
  pure $ serveWithContext (Proxy :: Proxy TestAPI) testApplicationContext server

mkTestApplicationContext :: IO (Context '[ HeaderKey "X-Test-Header" String])
mkTestApplicationContext = do
  hk <- newHeaderKey
  pure $ hk :. EmptyContext

server :: Server TestAPI
server = \ref -> do
  liftIO $ writeHeaderRef ref "Example header value"
  pure NoContent