module Main
  ( DomainRequest(..)
  , DomainResponse(..)
  , HttpType(..)
  , main
  , handle
  ) where

import Prelude
import Control.Promise (Promise, fromAff)
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Effect (Effect)

main ∷ Effect Unit
main = init

foreign import init :: Effect Unit

data ResponseHeader
  = Custom1 String
  | ContentType

type DomainResponse
  = { headers :: Array String, body :: String }

data HttpType
  = Post { body :: String }
  | Put { body :: String }
  | Patch { body :: String }
  | Delete
  | Get
  | Head

data RequestHeader
  = Custom String
  | Accept
  | CacheControl

newtype DomainRequest
  = DomainRequest { headers :: Array String, type :: HttpType }

derive instance genericDomainRequest :: Generic DomainRequest _

derive instance genericHttpType :: Generic HttpType _

instance encodeDomainRequest :: EncodeJson DomainRequest where
  encodeJson a = genericEncodeJson a

instance encodeHttpType :: EncodeJson HttpType where
  encodeJson a = genericEncodeJson a

instance decodeDomainRequest :: DecodeJson DomainRequest where
  decodeJson a = genericDecodeJson a

instance decodeHttpType :: DecodeJson HttpType where
  decodeJson a = genericDecodeJson a

handle :: Json -> Effect (Promise DomainResponse)
handle req =
  fromAff
    $ case decodeJson req of
        Right (DomainRequest parsedReq) -> case parsedReq.type of
          Get -> pure { headers: [], body: "" }
          Delete -> pure { headers: [], body: "parsedReq.req" }
          Head -> pure { headers: [], body: "" }
          Post post -> pure { headers: [], body: post.body }
          Patch patch -> pure { headers: [], body: patch.body }
          Put put -> pure { headers: [], body: put.body }
        Left e -> pure { headers: [], body: show e }

