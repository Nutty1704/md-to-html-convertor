module Assignment (markdownParser, convertADTHTML, boldParser, italicsParser, strikethroughParser) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, string, char)
import           Control.Applicative

data ADT = Empty 
          | Italics String
          | Bold String
          | Strikethrough String
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)


-- Parser for italics (_)
italicsParser :: Parser ADT
italicsParser = (Italics <$> (is '_' *> some (isNot '_') <* is '_')) <|> pure Empty

-- Parser for bold (**)
boldParser :: Parser ADT
boldParser = (Bold <$> (string "**" *> some (isNot '*') <* string "**")) <|> pure Empty

-- Parser for strikethrough (~~)
strikethroughParser :: Parser ADT
strikethroughParser = (Strikethrough <$> (string "~~" *> some (isNot '~') <* string "~~")) <|> pure Empty


markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = ""
convertADTHTML (Italics s) = "<i>" ++ s ++ "</i>"
convertADTHTML (Bold s) = "<b>" ++ s ++ "</b>"
convertADTHTML (Strikethrough s) = "<s>" ++ s ++ "</s>"
