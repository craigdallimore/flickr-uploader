module Main where

import Uploader.Auth (authorise)
import Uploader.HTTP (upload)

--------------------------------------------------------------------------------

main :: IO ()
main = do

  -- | This is the disclaimer
  putStrLn "This product uses the Flickr API but is not endorsed or certified by Flickr."

  eitherAuth <- authorise

  case eitherAuth of
    Left err                      -> print err
    Right (secret, apikey, token) -> upload secret apikey token "photo.jpg"

------------------------------------------------------------------------- KAIZEN
