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

import Servant.PureScript.Settings (AjaxError(DecodingError, InvalidData, UnexpectedHTTPStatus))

foreignToAjaxError :: forall a. Either ForeignError a -> Either AjaxError a
foreignToAjaxError = lmap InvalidData

getResult :: forall a m. (Generic a, MonadError AjaxError m) => (Json -> Either String a) -> AffjaxResponse Foreign -> m a
getResult decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  fVal <- if stCode >= 200 && stCode < 300
            then return resp.response
            else throwError $ UnexpectedHTTPStatus resp
  sVal <- throwLeft <<< lmap InvalidData <<< readString $ fVal
  jVal <- throwLeft <<< lmap DecodingError <<< jsonParser $ sVal
  throwLeft <<< lmap DecodingError <<< decode $ jVal

throwLeft :: forall a e m. MonadError e m => Either e a -> m a
throwLeft (Left e) = throwError e
throwLeft (Right a) = return a


-- encodeListQuery :: forall a b. Generic a => Settings b -> String -> Array a -> String
encodeListQuery opts fName = intercalate "&" <<< map (encodeQueryItem opts fName)

-- | The given name is assumed to be already escaped.
-- encodeQueryItem :: forall a b. Generic a => Settings b -> String -> a -> String
encodeQueryItem opts fName val = fName <> "=" <> (encodeURIComponent <<< opts.toURLPiece) val
