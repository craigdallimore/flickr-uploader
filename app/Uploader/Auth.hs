{-# LANGUAGE OverloadedStrings #-}

module Uploader.Auth where

import Uploader.Types
import Uploader.HTTP

import Control.Lens ((^.))
import Data.Aeson.Lens ( key
                       , _String
                       )
import Data.Semigroup ((<>))

import Data.Yaml ( FromJSON(parseJSON)
                 , ParseException()
                 , decodeFileEither
                 , decode
                 , (.:)
                 )

import Network.Wreq ( get
                    , responseBody
                    )

import System.Directory (doesFileExist)

--------------------------------------------------------------------------------

tokenPath :: FilePath
tokenPath = ".token" -- Hardly safe.

--------------------------------------------------------------------------------

requestAuthToken :: Config -> Frob -> IO (Either AuthError AuthToken)
requestAuthToken config frob = do
  putStrLn "Requesting auth token..."
  response <- get (authTokenUrl config frob)

  let token = show $ response ^. responseBody . key "auth" . key "token" . key "_content" . _String
  if token == ""
    then pure $ Left (TokenError "Failed to get token")
    else pure $ Right token

--------------------------------------------------------------------------------

getAuthToken :: Config -> IO (Either AuthError AuthToken)
getAuthToken config = do

  isTokenFilePresent <- doesFileExist tokenPath

  if isTokenFilePresent

    then do
      token <- readFile tokenPath
      pure (Right token)

    else do
      response <- get (frobUrl config)

      let frob = show $ response ^. responseBody . key "frob" . key "_content" . _String

      if frob == ""

        then pure $ Left (TokenError "Failed to get frob")

        else do

          putStrLn "Please authorise the app at the following address"
          putStrLn $ signInUrl config frob
          putStrLn "Press a key when that is done"

          _ <- getChar

          eitherToken <- requestAuthToken config frob

          case eitherToken of
            Left err    -> pure $ Left err
            Right token -> do
              putStrLn $ "Storing token: " <> token
              writeFile tokenPath token
              pure (Right token)

--------------------------------------------------------------------------------

readConfig :: IO (Either ParseException Config)
readConfig = decodeFileEither "config.yaml"

--------------------------------------------------------------------------------

authorise :: IO (Either AuthError (Secret, ApiKey, AuthToken))
authorise = do

  eitherConfig <- readConfig

  case eitherConfig of
    Left err     -> pure $ Left (AuthError err)
    Right config -> do

      eitherToken <- getAuthToken config
      case eitherToken of
        Left err    -> pure $ Left err
        Right token -> pure $ Right (secret config, apiKey config, token)

--------------------------------------------------------------------------------
