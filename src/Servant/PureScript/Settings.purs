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



type URLPiece = String
