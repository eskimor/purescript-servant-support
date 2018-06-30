module Servant.PureScript.Util where

import Prelude

import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Network.HTTP.Affjax (AffjaxRequest)
import Servant.PureScript.Ajax (AjaxError, ErrorDescription, makeAjaxError)
import Servant.PureScript.Settings (SPSettings_(SPSettings_), SPSettingsToUrlPiece_(SPSettingsToUrlPiece_), SPSettingsEncodeHeader_(SPSettingsEncodeHeader_))


encodeListQuery :: forall a b rep. Generic a rep => EncodeRep rep => SPSettings_ b -> String -> Array a -> String
encodeListQuery opts'@(SPSettings_ opts) fName = intercalate "&" <<< map (encodeQueryItem opts' fName)

-- | The given name is assumed to be already escaped.
encodeQueryItem :: forall a b rep. Generic a rep => EncodeRep rep => SPSettings_ b -> String -> a -> String
encodeQueryItem opts'@(SPSettings_ opts) fName val = fName <> "=" <> encodeURLPiece opts' val

-- | Call opts.toURLPiece and encode the resulting string with encodeURIComponent.
encodeURLPiece :: forall a rep params. Generic a rep => EncodeRep rep => SPSettings_ params -> a -> String
encodeURLPiece (SPSettings_ opts) = case opts.toURLPiece of SPSettingsToUrlPiece_ f -> f

encodeHeader :: forall a rep params. Generic a rep => EncodeRep rep => SPSettings_ params -> a -> String
encodeHeader (SPSettings_ opts) = case opts.encodeHeader of SPSettingsEncodeHeader_ f -> f

reportRequestError :: forall res. AffjaxRequest -> (String -> ErrorDescription res) -> String -> String -> AjaxError res
reportRequestError req' err source msg = makeAjaxError req' $ reportError err source msg

reportError :: forall err. (String -> err) -> String -> String  -> err
reportError err source msg = err $ msg <> ", source: '" <> source <> "'"
