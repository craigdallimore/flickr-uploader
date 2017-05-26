{-# LANGUAGE OverloadedStrings #-}

module Uploader.Types where

import Data.Aeson.Types (typeMismatch)

import Data.Map.Strict (Map)

import Data.Yaml ( FromJSON(parseJSON)
                 , ParseException()
                 , Value(Object)
                 , (.:)
                 )

--------------------------------------------------------------------------------

-- | The yaml config
data Config = Config { apiKey :: ApiKey, secret :: Secret } deriving (Show)

instance FromJSON Config where
  parseJSON (Object a) = Config <$> a .: "key"
                                <*> a .: "secret"
  parseJSON invalid    = typeMismatch "Config" invalid

type AuthToken  = String
type Frob       = String
type ApiKey     = String
type Secret     = String
type ArgMap     = Map String String
type TokenError = String

data AuthError  = AuthError ParseException | TokenError String deriving (Show)

--------------------------------------------------------------------------------
