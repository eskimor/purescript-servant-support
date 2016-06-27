{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}

module Servant.PureScript.Affjax where

import Prelude
import Control.Monad.Aff (makeAff', Aff, Canceler(Canceler), makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import DOM.XHR.Types (XMLHttpRequest)
import Data.Function.Uncurried (Fn5, runFn5, Fn4, runFn4)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Network.HTTP.Affjax (AJAX, AffjaxResponse)
import Network.HTTP.ResponseHeader (ResponseHeader, responseHeader)
import Network.HTTP.StatusCode (StatusCode())


data AjaxError =
  UnexpectedHTTPStatus { -- This is the same as AffjaxResponse String, but when using AffjaxResponse String deriving Generic does not work.
      status :: StatusCode
    , headers :: Array ResponseHeader
    , response :: String
    }
  | DecodingError String

derive instance genericAjaxError :: Generic AjaxError

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
