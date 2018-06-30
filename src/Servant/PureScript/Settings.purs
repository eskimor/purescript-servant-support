{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where


import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep, genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep, genericEncodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Prelude (identity, (<<<))
import Servant.PureScript.JsUtils (encodeUriComponent)

-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_ (forall a rep. Generic a rep => EncodeRep rep => a -> Json)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_ (forall a rep. Generic a rep => DecodeRep rep => Json -> Either String a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_ (forall a. ToUrlPiece a => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_ (forall a. ToUrlPiece a => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

class ToUrlPiece a where
  toUrlPiece :: a -> URLPiece

instance stringToUrlPiece :: ToUrlPiece String where
  toUrlPiece = identity

else instance genericRepToUrlPiece :: (Generic a rep, EncodeRep rep) => ToUrlPiece a where
  toUrlPiece = stringify <<< genericEncodeJson

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultEncodeHeader = toUrlPiece


-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a. ToUrlPiece a => a -> URLPiece
gDefaultEncodeURLPiece = encodeUriComponent <<< gDefaultToURLPiece


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ genericEncodeJson
  , decodeJson : SPSettingsDecodeJson_ genericDecodeJson
  , toURLPiece : SPSettingsToUrlPiece_ gDefaultToURLPiece
  , encodeHeader : SPSettingsEncodeHeader_ gDefaultEncodeHeader
  , params : params
}
