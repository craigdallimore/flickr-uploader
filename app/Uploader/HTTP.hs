{-# LANGUAGE OverloadedStrings #-}

module Uploader.HTTP where

import Uploader.Types
import qualified Data.ByteString.Lazy.Char8 as BSC
import Control.Lens ((^.))
import Data.List as DL
import Data.String (fromString)
import Data.Semigroup ((<>))
import Data.Digest.Pure.MD5 (md5)
import Data.Map.Strict ( fromList
                       , foldrWithKey
                       )
import Network.Wreq ( Part
                    , partFileSource
                    , partText
                    , post
                    , responseBody
                    )

import Text.XML.Light.Types ( elName
                            , qName
                            , Content ( Text
                                      , Elem
                                      , CRef
                                      )
                            )
import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Cursor ( Cursor
                             , current
                             , findRec
                             , fromForest
                             )
import Text.XML.Light.Proc (strContent)

--------------------------------------------------------------------------------

-- Notes related to uploading
-- https://www.flickr.com/services/api/upload.api.html
--
-- Endpoint
-- --------
-- https://up.flickr.com/services/upload/
--
-- Arguments
-- ---------
-- Note that the 'photo' parameter should not be included in the signature.
-- All other POST parameters should be included when generating the signature.
--
-- photo
-- The file to upload.
--
-- title (optional)
-- The title of the photo.
--
-- description (optional)
-- A description of the photo. May contain some limited HTML.
--
-- tags (optional)
-- A space-seperated list of tags to apply to the photo.
--
-- is_public, is_friend, is_family (optional)
-- Set to 0 for no, 1 for yes. Specifies who can view the photo.
--
-- safety_level (optional)
-- Set to 1 for Safe, 2 for Moderate, or 3 for Restricted.
--
-- content_type (optional)
-- Set to 1 for Photo, 2 for Screenshot, or 3 for Other.
--
-- hidden (optional)
-- Set to 1 to keep the photo in global search results, 2 to hide from public searches.

--------------------------------------------------------------------------------

-- Generate a flickr signature string from a shared secret and an argument map
genSig :: Secret -> ArgMap -> Signature
genSig secret args = (show . md5 . BSC.pack) $ secret <> foldrWithKey go "" args where
  go key val acc = key <> val <> acc

-- Add a signature to a collection of args
sign :: Secret -> ArgMap -> String
sign secret args = toQueryString args <> "&api_sig=" <> genSig secret args

-- Generate a queryString from a map
toQueryString :: ArgMap -> String
toQueryString args = DL.concat pairs where
  pairs          = DL.intersperse "&" $ foldrWithKey go [] args
  go key val acc = key <> "=" <> val : acc

-- Generate the parts for a pultipart upload
makeParts :: FilePath
          -> Secret
          -> ArgMap
          -> [Part]
makeParts filepath secret args = foldrWithKey go xs args where
  go key val xs = partText (fromString key) (fromString val) : xs
  signature = genSig secret args
  xs = [ partFileSource "photo"   filepath
       , partText       "api_sig" (fromString signature)
       ]

--------------------------------------------------------------------------------

findPhotoId :: Cursor -> Bool
findPhotoId c = w (current c) where
  w (Elem e ) = (qName . elName) e == "photoid"
  w (Text _ ) = False
  w (CRef _ ) = False

upload :: Secret -> ApiKey -> AuthToken -> FilePath -> IO (Either UploadError PhotoId)
upload secret apikey token filepath = do

  let args = fromList [ ("api_key",        apikey)
                      , ("auth_token",     token)
                      , ("format",         "json")
                      , ("nojsoncallback", "1")
                      , ("is_public",      "0")
                      , ("is_friend",      "0")
                      , ("is_family",      "0")
                      ]

  response <- post uploadEndpoint (makeParts filepath secret args)

  let body        = response ^. responseBody
      xml         = parseXML body
      maybeId = do
        m        <- fromForest xml
        (Elem e) <- current <$> findRec findPhotoId m
        pure (strContent e)

  case maybeId of
    Nothing      -> pure $ Left ("Problem uploading " <> filepath)
    Just photoId -> pure $ Right photoId

--------------------------------------------------------------------------------

-- Example secret and signature string for testing purposes
-- https://www.flickr.com/services/api/auth.howto.desktop.html

testSecret :: String
testSecret = "000005fab4534d05"

testArgs :: ArgMap
testArgs = fromList [ ("method", "flickr.auth.getFrob")
                    , ("api_key", "9a0554259914a86fb9e7eb014e4e5d52")
                    ]

generatesValidSignature :: Bool
generatesValidSignature = genSig testSecret testArgs == "8ad70cd3888ce493c8dde4931f7d6bd0"

--------------------------------------------------------------------------------

restEndpoint :: String
restEndpoint = "https://api.flickr.com/services/rest/?"

authEndpoint :: String
authEndpoint = "https://api.flickr.com/services/auth/?"

uploadEndpoint :: String
uploadEndpoint = "https://up.flickr.com/services/upload/"

--------------------------------------------------------------------------------

-- A url to use for requesting a "frob"
-- A frob is needed to construct a URL for a flickr permissions dialog
frobUrl :: Config -> String
frobUrl config = restEndpoint <> sign (secret config) args where
  args = fromList [ ("method", "flickr.auth.getFrob")
                  , ("format", "json")
                  , ("nojsoncallback", "1")
                  , ("api_key", apiKey config)
                  ]

-- A url for a flickr permissions dialog (provide this to the user so they can
-- authorise the application)
signInUrl :: Config -> Frob -> String
signInUrl config frob = authEndpoint <> sign (secret config) args where
  args = fromList [ ("perms", "write")
                  , ("frob", frob)
                  , ("api_key", apiKey config)
                  ]

-- A url for an auth token
authTokenUrl :: Config -> Frob -> String
authTokenUrl config frob = restEndpoint <> sign (secret config) args where
  args = fromList [ ("method", "flickr.auth.getToken")
                  , ("format", "json")
                  , ("nojsoncallback", "1")
                  , ("frob", frob)
                  , ("api_key", apiKey config)
                  ]

--------------------------------------------------------------------------------
