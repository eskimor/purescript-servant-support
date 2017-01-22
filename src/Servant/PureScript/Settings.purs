{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where


import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic (class Generic, GenericSpine(SString), toSpine)
import Global (encodeURIComponent)


-- | WARNING: encodeJson, decodeJson, toURLPiece: currently ignored due to compiler bug:
--   https://github.com/purescript/purescript/issues/1957
newtype SPSettings_ params = SPSettings_ {
    encodeJson :: forall a. Generic a => a -> Json -- Currently not used (does not work - compiler bug: )
  , decodeJson :: forall a. Generic a => Json -> Either String a
  , toURLPiece :: forall a. Generic a => a -> URLPiece
  , params :: params
  }

type URLPiece = String

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a. Generic a => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a. Generic a => a -> URLPiece
gDefaultEncodeHeader v =
  case toSpine v of
    SString s -> s -- Special case string - just use it as is (http-api-data compatibility).
    _ -> show <<< Aeson.encodeJson $ v

-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a. Generic a => a -> URLPiece
gDefaultEncodeURLPiece = encodeURIComponent <<< gDefaultToURLPiece


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : Aeson.encodeJson
  , decodeJson : Aeson.decodeJson
  , toURLPiece : gDefaultToURLPiece
  , params : params
}
