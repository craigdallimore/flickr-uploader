{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( intersperse )
import Data.Semigroup ((<>))

import Data.ByteString.Lazy ( ByteString
                            , concat
                            , unpack
                             )

import qualified Data.ByteString.Lazy as BS

import Data.Text.Encoding (decodeUtf8)

import Data.Map.Strict ( Map
                       , fromList
                       , foldrWithKey
                       )

import Data.Digest.Pure.MD5 ( md5
                            , MD5Digest
                            )

import Network.HTTP ( simpleHTTP
                    , getRequest
                    )

type Frob   = ByteString
type ApiKey = ByteString
type Secret = ByteString
type ArgMap = Map ByteString ByteString

--------------------------------------------------------------------------------

-- Example secret and signature string for testing purposes
-- https://www.flickr.com/services/api/auth.howto.desktop.html

testSecret :: ByteString
testSecret = "000005fab4534d05"

testArgs :: ArgMap
testArgs = fromList [ ("method", "flickr.auth.getFrob")
                    , ("api_key", "9a0554259914a86fb9e7eb014e4e5d52")
                    ]

generatesValidSignature :: Bool
generatesValidSignature = genSig testSecret testArgs == "8ad70cd3888ce493c8dde4931f7d6bd0"

--------------------------------------------------------------------------------

-- Generate a flickr signature string from a shared secret and an argument map
genSig :: Secret -> ArgMap -> String
genSig secret args = (show . md5) $ secret <> foldrWithKey go "" args where
  go key val acc = key <> val <> acc

-- Add a signature to a collection of args
sign :: Secret -> ArgMap -> String
sign secret args = toQueryString args <> "&api_sig=" <> genSig secret args

-- Generate a queryString from a map
toQueryString :: ArgMap -> String
toQueryString args = show $ BS.concat pairs where
  pairs          = intersperse "&" $ foldrWithKey go [] args
  go key val acc = key <> "=" <> val : acc

--------------------------------------------------------------------------------

{-

  Notes related to uploading
  https://www.flickr.com/services/api/upload.api.html

  Endpoint
  --------
  https://up.flickr.com/services/upload/

  Arguments
  ---------
  Note that the 'photo' parameter should not be included in the signature.
  All other POST parameters should be included when generating the signature.

  photo
  The file to upload.

  title (optional)
  The title of the photo.

  description (optional)
  A description of the photo. May contain some limited HTML.

  tags (optional)
  A space-seperated list of tags to apply to the photo.

  is_public, is_friend, is_family (optional)
  Set to 0 for no, 1 for yes. Specifies who can view the photo.

  safety_level (optional)
  Set to 1 for Safe, 2 for Moderate, or 3 for Restricted.

  content_type (optional)
  Set to 1 for Photo, 2 for Screenshot, or 3 for Other.

  hidden (optional)
  Set to 1 to keep the photo in global search results, 2 to hide from public searches.

-}

--------------------------------------------------------------------------------

restEndpoint :: String
restEndpoint = "http://flickr.com/services/rest/?"

authEndpoint :: String
authEndpoint = "http://flickr.com/services/auth/?"

-- A url to use for requesting a "frob"
-- A frob is needed to construct a URL for a flickr permissions dialog
frobUrl :: Secret -> ApiKey -> String
frobUrl secret apikey = restEndpoint <> sign secret args where
  args = fromList [ ("method", "flickr.auth.getFrob")
                  , ("api_key", apikey)
                  ]

-- A url for a flickr permissions dialog
signInUrl :: Secret -> ApiKey -> Frob -> String
signInUrl secret apikey frob = authEndpoint <> sign secret args where
  args = fromList [ ("perms", "write")
                  , ("frob", frob)
                  , ("api_key", apikey)
                  ]

-- http://flickr.com/services/rest/?method=flickr.auth.getToken&api_key=987654321&frob=1a2b3c4d5e&api_sig=7f3870be274f6c49b3e31a0c6728957f
-- A url for an auth token
authTokenUrl :: Secret -> ApiKey -> Frob -> String
authTokenUrl secret apikey frob = restEndpoint <> sign secret args where
  args = fromList [ ("method", "flickr.auth.getToken")
                  , ("frob", frob)
                  , ("api_key", apikey)
                  ]

main :: IO ()
main = do
  putStrLn "This product uses the Flickr API but is not endorsed or certified by Flickr."
  putStrLn "Well..."
