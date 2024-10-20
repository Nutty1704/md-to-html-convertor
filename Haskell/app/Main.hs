{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import           Assignment              (convertADTHTML, markdownParser, getTime)
import           Data.Aeson              (object, (.=))
import           Data.Aeson.Key          (fromString)
import           Data.Text.Lazy          (Text, pack, unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Instances               (ParseResult (Result), parse)
import           Web.Scotty              (ActionM, body, json, post, scotty)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Exception       (try, SomeException)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _            = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= ((pack value) :: Text) | (key, value) <- pairs]

-- Format the current time to be used as a file name
formatTime :: String -> String
formatTime = map (\c -> if c == ':' then '-' else c)

-- Write the HTML content to a file
writeHTML :: String -> IO (Either SomeException String)
writeHTML htmlContent = do
  currentTime <- formatTime <$> getTime
  let fileName = "html_output_" ++ currentTime ++ ".html"
  -- Attempt to write the HTML content to a file and catch any exceptions
  result <- try (writeFile fileName htmlContent) :: IO (Either SomeException ())
  return $ case result of
    Left ex  -> Left ex  -- Return the exception
    Right () -> Right fileName  -- Return the file name on success


main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse markdownParser str) convertADTHTML

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]

  -- Endpoint to save the HTML content to a file
  post "/api/saveHTML" $ do
    requestBody <- body

    -- Extract the HTML content from the request body
    let htmlContent = unpack $ decodeUtf8 requestBody
    
    -- Attempt to write the HTML content to a file
    result <- liftIO $ writeHTML htmlContent
    
    -- Respond with a message indicating whether the file was saved successfully
    case result of
      Left _ -> do
        jsonResponse [("message", "File save failed"), ("success", "false")]
      Right fileName -> do
        jsonResponse [("message", "File saved as " ++ fileName), ("success", "true")]
    