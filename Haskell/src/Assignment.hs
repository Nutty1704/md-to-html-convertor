module Assignment (markdownParser, convertADTHTML, italicsParser) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, satisfy)
import           Control.Applicative

data ADT = Empty 
          | Italics String
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)


-- Parser for italics
italicsParser :: Parser ADT
italicsParser = (Italics <$> (is '_' *> some (satisfy (/= '_')) <* is '_')) <|> pure Empty

markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = ""
convertADTHTML (Italics s) = "<i>" ++ s ++ "</i>"
