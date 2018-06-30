-- | This module defines types for some global Javascript functions
-- | and values.
module Servant.PureScript.JsUtils where

-- | uri component encoding
foreign import encodeUriComponent :: String -> String

foreign import unsafeToString :: forall obj. obj -> String