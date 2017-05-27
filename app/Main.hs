module Main where

import Uploader.Auth (authorise)
import Uploader.HTTP (upload)
import Uploader.Types ( AuthError
                      , Secret
                      , ApiKey
                      , AuthToken
                      )

import Control.Monad (forM_)
import Data.Semigroup ((<>))
import System.Environment (getArgs)
import System.Directory ( listDirectory
                        , doesDirectoryExist
                        )
import System.FilePath (joinPath)

--------------------------------------------------------------------------------

getPhotoList :: IO (Either String [FilePath])
getPhotoList = do

  args <- getArgs

  case args of
    []    -> pure $ Left "A photo directory needs to be specified"
    dir:_ -> do

      isDirectoryPresent <- doesDirectoryExist dir

      if isDirectoryPresent

        then do
          files <- listDirectory dir
          let fullPathFiles = foldr (\file acc -> joinPath [dir, file]:acc) [] files
          pure $ Right fullPathFiles

        else pure $ Left ("Directory " <> dir <> " was not found")

--------------------------------------------------------------------------------

uploadPhoto :: Secret
            -> ApiKey
            -> AuthToken
            -> FilePath
            -> IO ()
uploadPhoto secret apikey token photo = do

  putStr $ "Uploading " <> photo <> "... "

  eitherUpload <- upload secret apikey token photo

  case eitherUpload of
    Left err      -> print err
    Right photoId -> putStrLn photoId

--------------------------------------------------------------------------------

main :: IO ()
main = do

  -- | This is the disclaimer
  putStrLn "This product uses the Flickr API but is not endorsed or certified by Flickr."

  eitherPhotoList <- getPhotoList

  case eitherPhotoList of

    Left err     -> print err
    Right photos -> do

      putStrLn $ "Found " <> show (length photos) <> " images"
      eitherAuth <- authorise

      case eitherAuth of
        Left err                      -> print err
        Right (secret, apikey, token) -> do
          forM_ photos (uploadPhoto secret apikey token)
          putStrLn "Done"

------------------------------------------------------------------------- KAIZEN
