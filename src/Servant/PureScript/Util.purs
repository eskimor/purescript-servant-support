module Servant.PureScript.Util where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json())
import Data.Generic (class Generic)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Global (encodeURIComponent)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Foreign (ForeignError(), Foreign(), readString)
import Data.Bifunctor (lmap)
import Data.Argonaut.Parser (jsonParser)
import Unsafe.Coerce (unsafeCoerce) -- Make the type checker happy (I have to figure out why this is necessary.)

import Servant.PureScript.Affjax (AjaxError(DecodingError, UnexpectedHTTPStatus))
import Servant.PureScript.Settings (SPSettings_(SPSettings_))

getResult :: forall a m. (Generic a, MonadError AjaxError m) => (Json -> Either String a) -> AffjaxResponse String -> m a
getResult decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  fVal <- if stCode >= 200 && stCode < 300
            then return resp.response
            else throwError $ UnexpectedHTTPStatus resp
  jVal <- throwLeft <<< lmap DecodingError <<< jsonParser $ fVal
  throwLeft <<< lmap DecodingError <<< decode $ jVal

throwLeft :: forall a e m. MonadError e m => Either e a -> m a
throwLeft (Left e) = throwError e
throwLeft (Right a) = return a


-- encodeListQuery :: forall a b. Generic a => Settings b -> String -> Array a -> String
encodeListQuery opts'@(SPSettings_ opts) fName = intercalate "&" <<< map (encodeQueryItem opts' fName)

-- | The given name is assumed to be already escaped.
-- encodeQueryItem :: forall a b. Generic a => SPSettings_ b -> String -> a -> String
encodeQueryItem (SPSettings_ opts) fName val = fName <> "=" <> (encodeURIComponent <<< unsafeCoerce opts.toURLPiece) val
