module Lib
  ( getJSON
  ) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Simple.JSON as JSON

getJSON :: forall a r. JSON.ReadForeign r => String -> (r -> Aff a) -> Aff Unit
getJSON url f = do
  res <- AX.request $ AX.defaultRequest
      { url = url
      , method = Left GET
      , responseFormat = ResponseFormat.json
      }

  case res.body of
    Left err -> log
      ( "GET "
        <> url
        <> "response failed to decode: "
        <> AX.printResponseFormatError err
      )
    Right json ->
      case JSON.readJSON (J.stringify json) of
        Right r -> void (f r)
        Left e -> log (show e)
