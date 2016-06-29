module Servant.PureScript.Util where

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foreign (ForeignError, Foreign, readString)
import Data.Generic (class Generic)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (AjaxError(DecodingError, ParsingError, UnexpectedHTTPStatus))
import Servant.PureScript.Settings (SPSettings_(SPSettings_))
import Unsafe.Coerce (unsafeCoerce)

getResult :: forall a m. (Generic a, MonadError AjaxError m) => (Json -> Either String a) -> AffjaxResponse String -> m a
getResult decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  fVal <- if stCode >= 200 && stCode < 300
            then pure resp.response
            else throwError $ UnexpectedHTTPStatus resp
  jVal <- throwLeft <<< lmap (reportError ParsingError fVal) <<< jsonParser $ fVal
  throwLeft <<< lmap (reportError DecodingError (show jVal)) <<< decode $ jVal

throwLeft :: forall a e m. MonadError e m => Either e a -> m a
throwLeft (Left e) = throwError e
throwLeft (Right a) = pure a


-- encodeListQuery :: forall a b. Generic a => Settings b -> String -> Array a -> String
encodeListQuery opts'@(SPSettings_ opts) fName = intercalate "&" <<< map (encodeQueryItem opts' fName)

-- | The given name is assumed to be already escaped.
-- encodeQueryItem :: forall a b. Generic a => SPSettings_ b -> String -> a -> String
encodeQueryItem (SPSettings_ opts) fName val = fName <> "=" <> (encodeURIComponent <<< unsafeCoerce opts.toURLPiece) val


reportError :: (String -> AjaxError) -> String -> String  -> AjaxError 
reportError err source msg = err $ msg <> ", source: '" <> source <> "'"
