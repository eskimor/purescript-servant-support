{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where


import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Util (stripModulePath)
import Data.Array (null)
import Data.Either (Either)
import Data.Generic (class Generic, GenericSpine(SArray, SChar, SString, SNumber, SInt, SBoolean, SRecord, SProd, SUnit), toSpine)
import Data.String (joinWith)


-- | WARNING: encodeJson, decodeJson, toURLPiece: currently ignored due to compiler bug:
--   https://github.com/purescript/purescript/issues/1957
newtype SPSettings_ params = SPSettings_ {
    encodeJson :: forall a. Generic a => a -> Json -- Currently not used (does not work - compiler bug: )
  , decodeJson :: forall a. Generic a => Json -> Either String a
  , toURLPiece :: forall a. Generic a => a -> URLPiece
  , params :: params
  }

type URLPiece = String

-- Just use the robust JSON format.
gDefaultToURLPiece :: forall a. Generic a => a -> URLPiece
gDefaultToURLPiece = show <<< Aeson.encodeJson


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : Aeson.encodeJson
  , decodeJson : Aeson.decodeJson
  , toURLPiece : gDefaultToURLPiece
  , params : params
}
