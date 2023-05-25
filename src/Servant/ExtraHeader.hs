module Servant.ExtraHeader where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Functor
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Servant.API
import Servant.Server
import Servant.Server.Internal
import Network.Wai
import qualified Data.Vault.Lazy as Vault
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as ByteString

data ExtraHeader (h :: Symbol) (a :: Type)

newtype HeaderRef (h :: Symbol) (a :: Type) =
  HeaderRef (TVar [a])

writeHeaderRef :: HeaderRef h a -> a -> IO ()
writeHeaderRef (HeaderRef tvar) val =
  atomically $ writeTVar tvar [val]

newtype HeaderKey (h :: Symbol) (a :: Type) =
  HeaderKey (Vault.Key (HeaderRef h a))

newHeaderKey :: IO (HeaderKey h a)
newHeaderKey = HeaderKey <$> Vault.newKey

instance (HasContextEntry ctx (HeaderKey h a), KnownSymbol h, ToHttpApiData a, HasServer api ctx) => HasServer (ExtraHeader h a :> api) ctx where
  type ServerT (ExtraHeader h a :> api) m = HeaderRef h a -> ServerT api m

  route Proxy ctx server = do
    let HeaderKey vaultKey :: HeaderKey h a =
          getContextEntry ctx
        serverWithRef =
          passWithVaultLookup vaultKey server
    route (Proxy :: Proxy api) ctx serverWithRef <&> \app req resp -> do
      tvar <- newTVarIO []
      let modifiedReq = req {
            vault = Vault.insert vaultKey (HeaderRef tvar :: HeaderRef h a) (vault req)
          }
      app modifiedReq $ \rr -> do
        providedHeaders <- atomically (readTVar tvar)
        let headerName = CI.mk $ ByteString.pack $ symbolVal (Proxy :: Proxy h)
            renderedHeaders =
              map ((,) headerName . toHeader) providedHeaders
        resp $ fmap (mapResponseHeaders (++ renderedHeaders)) rr

  hoistServerWithContext Proxy ctx nt server =
    hoistServerWithContext (Proxy :: Proxy api) ctx nt . server

passWithVaultLookup :: Vault.Key a
                    -> Delayed env (a -> server)
                    -> Delayed env server
passWithVaultLookup vk Delayed{..} =
  Delayed
    { serverD = \c p h a b req ->
        case Vault.lookup vk (vault req) of
          Just res -> fmap ($ res) $ serverD c p h a b req
          Nothing -> FailFatal err500
    , ..
    }


instance HasLink api => HasLink (ExtraHeader h a :> api) where
  type MkLink (ExtraHeader h a :> api) f = MkLink api f

  toLink f Proxy = toLink f (Proxy :: Proxy api)