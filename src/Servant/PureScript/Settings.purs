{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where


import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json())
import Data.Generic (class Generic)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Global (encodeURIComponent)
import Data.Nullable (Nullable(), toNullable)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Foreign (ForeignError(), Foreign(), readString)
import Data.Bifunctor (lmap)
import Data.Argonaut.Parser (jsonParser)


type Settings params = {
    encodeJson :: forall a. Generic a => a -> Json
  , decodeJson :: forall a. Generic a => Json -> Either String a
  , toURLPiece :: forall a. Generic a => a -> URLPiece
  , params :: params
  }


data AjaxError =
    UnexpectedHTTPStatus (AffjaxResponse Foreign)
  | InvalidData ForeignError
  | DecodingError String

foreignToAjaxError :: forall a. Either ForeignError a -> Either AjaxError a
foreignToAjaxError = lmap InvalidData

getResult :: forall a m p. (Generic a, MonadError AjaxError m) => (Json -> Either String a) -> AffjaxResponse Foreign -> m a
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

type URLPiece = String

-- encodeListQuery :: forall a b. Generic a => Settings b -> String -> Array a -> String
encodeListQuery opts fName = intercalate "&" <<< map (encodeQueryItem opts fName)

-- | The given name is assumed to be already escaped.
-- encodeQueryItem :: forall a b. Generic a => Settings b -> String -> a -> String
encodeQueryItem opts fName val = fName <> "=" <> (encodeURIComponent <<< opts.toURLPiece) val
