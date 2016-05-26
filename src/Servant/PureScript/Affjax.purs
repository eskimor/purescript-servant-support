{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}

module Servant.PureScript.Affjax where

import Prelude

import Data.Argonaut.Core (Json())
import Data.Generic (class Generic)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Global (encodeURIComponent)
import Data.Nullable (Nullable(), toNullable)
import Data.Maybe (Maybe(..))
import DOM.XHR.Types (XMLHttpRequest())
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeader)
import Data.Function (Fn5(), runFn5, Fn4(), runFn4, on)
import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..), attempt, later', forkAff, cancel)
import Network.HTTP.Affjax (AJAX, AffjaxResponse)
import Control.Monad.Eff (Eff())
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Foreign (Foreign(), readString)
import Network.HTTP.Affjax (AJAX, AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Data.Foreign (ForeignError(), Foreign(), readString)
import Data.Bifunctor (lmap)
import Data.Argonaut.Parser (jsonParser)
import Data.Generic (class Generic)


data AjaxError =
    UnexpectedHTTPStatus (AffjaxResponse String)
  | DecodingError String

instance showAjaxError :: Show AjaxError where
  show (UnexpectedHTTPStatus resp) = "An unexpected HTTP status was received: " <> show resp.status
  show (DecodingError str) = "Decoding failed: " <> str



type AjaxRequest =
  { method :: String
  , url :: String
  , headers :: Array { field :: String, value :: String }
  , content :: Nullable String
  , responseType :: String
  , username :: Nullable String
  , password :: Nullable String
  , withCredentials :: Boolean
  }

defaultRequest :: AjaxRequest
defaultRequest = {
    method : "GET"
  , url : ""
  , headers : [ {field : "Accept", value : "application/json"}
              , {field : "content-type", value : "application/json"}]
  , content : toNullable (Nothing :: Maybe String)
  , responseType : "json"
  , username : toNullable (Nothing :: Maybe String)
  , password : toNullable (Nothing :: Maybe String)
  , withCredentials : false
  }


affjax :: forall e. AjaxRequest -> Aff (ajax :: AJAX | e) (AffjaxResponse String)
affjax = makeAff' <<< ajax

ajax :: forall e.
     AjaxRequest
  -> (Error -> Eff (ajax :: AJAX | e) Unit)
  -> (AffjaxResponse String -> Eff (ajax :: AJAX | e) Unit)
  -> Eff (ajax :: AJAX | e) (Canceler (ajax :: AJAX | e))
ajax req eb cb = runFn5 _ajax responseHeader req cancelAjax eb cb


foreign import _ajax
  :: forall e. Fn5 (String -> String -> ResponseHeader)
               AjaxRequest
               (XMLHttpRequest -> Canceler (ajax :: AJAX | e))
               (Error -> Eff (ajax :: AJAX | e) Unit)
               (AffjaxResponse String -> Eff (ajax :: AJAX | e) Unit)
               (Eff (ajax :: AJAX | e) (Canceler (ajax :: AJAX | e)))

cancelAjax :: forall e. XMLHttpRequest -> Canceler (ajax :: AJAX | e)
cancelAjax xhr = Canceler \err -> makeAff (\eb cb -> runFn4 _cancelAjax xhr err eb cb)

foreign import _cancelAjax
  :: forall e. Fn4 XMLHttpRequest
                   Error
                   (Error -> Eff (ajax :: AJAX | e) Unit)
                   (Boolean -> Eff (ajax :: AJAX | e) Unit)
                   (Eff (ajax :: AJAX | e) Unit)
