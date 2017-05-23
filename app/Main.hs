{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))

import Data.ByteString ( ByteString
                       , unpack
                       )

import Data.Text.Encoding (decodeUtf8)

import Data.Map.Strict ( Map
                       , fromList
                       , foldrWithKey
                       )

import Data.Digest.Pure.MD5 ( md5 )

-- Signing
----------
-- - is required for all calls using an auth token.
-- - is required for all calls to `flickr.auth.*`
--
-- Sort your argument list into alphabetical order based on the parameter name.
-- e.g. foo=1, bar=2, baz=3 sorts to bar=2, baz=3, foo=1
-- concatenate the shared secret and argument name-value pairs
-- e.g. SECRETbar2baz3foo1
-- calculate the md5() hash of this string
-- append this value to the argument list with the name api_sig, in hexidecimal string form
-- e.g. api_sig=1f3870be274f6c49b3e31a0c6728957f

type ApiSig = String
type Frob   = String

type ApiKey = ByteString
type Secret = ByteString

type ArgMap = Map ByteString ByteString

y :: ByteString
y = "SECRET"

x :: ArgMap
x = fromList [ ("foo", "1")
             , ("bar", "2")
             , ("baz", "3")
             ]

genSig :: Secret -> ArgMap -> ByteString
genSig secret args = md5 $ secret <> foldrWithKey go "" args where
  go key val acc = key <> val <> acc


frobMap :: ApiKey -> ArgMap
frobMap key = fromList [ ("method", "flickr.auth.getFrob")
                       , ("api_key", key)
                       ]

--frobUrl :: ApiKey -> ApiSig -> String
--frobUrl key sig = "http://flickr.com/services/rest/?"api_sig=" <> sig

--signInUrl :: ApiKey -> ApiSig -> Frob -> String
--signInUrl key sig frob = "http://flickr.com/services/auth/?api_key=" <> key <> "&perms=write&frob=" <> frob <> "&api_sig=" <> sig

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn "Well..."
