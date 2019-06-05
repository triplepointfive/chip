module Chip.Lib
  ( getJSON
  ) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Simple.JSON as JSON

getJSON :: forall r. JSON.ReadForeign r => String -> Aff (Maybe r)
getJSON url = do
  res <- AX.request $ AX.defaultRequest
      { url = url
      , method = Left GET
      , responseFormat = ResponseFormat.json
      }

  case res.body of
    Left err -> do
        log
            ( "GET "
              <> url
              <> "response failed to decode: "
              <> AX.printResponseFormatError err
            )
        pure Nothing
    Right json ->
        case JSON.readJSON (J.stringify json) of
          Right r -> pure (Just r)
          Left e -> do
            log (show e)
            pure Nothing
