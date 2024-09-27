module Assignment (markdownParser, convertADTHTML, imageParser) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, string, spaces, digit, oneof, eof)
import           Control.Applicative

data ADT = Empty 
          | Italics String
          | Bold String
          | Strikethrough String
          | Link String String
          | InlineCode String
          | Footnote String
          | Image String String String
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

-- Parser for Link ([...](...))
linkParser :: Parser ADT
linkParser = (Link <$> (is '[' *> some (isNot ']') <* is ']') <*> (spaces *> is '(' *> some (isNot ')') <* is ')')) <|> pure Empty

-- Parser for inline code (`...`)
inlineCodeParser :: Parser ADT
inlineCodeParser = (InlineCode <$> (is '`' *> some (isNot '`') <* is '`')) <|> pure Empty

-- Parser for footnotes ([^N])
footnoteParser :: Parser ADT
footnoteParser = (Footnote <$> (is '[' *> is '^' *> some (digit) <* is ']')) <|> pure Empty

-- Parser for image (![Alt Text](URL "Caption"))
imageParser :: Parser ADT
imageParser = (Image <$> altTextParser <*> urlParser <*> captionParser) <|> pure Empty
  where
    altTextParser = many (oneof "\t ") *> is '!' *> is '[' *> some (isNot ']') <* is ']'
    urlParser = spaces *> is '(' *> some (isNot ' ') <* is ' '
    captionParser = spaces *> is '"' *> some (isNot '"') <* is '"' <* is ')'


-- Parser for markdown
markdownParser :: Parser ADT
markdownParser = italicsParser <|> boldParser <|> strikethroughParser <|> linkParser <|> inlineCodeParser <|> footnoteParser <|> imageParser <* eof

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = ""
convertADTHTML (Italics s) = "<em>" ++ s ++ "</em>"
convertADTHTML (Bold s) = "<strong>" ++ s ++ "</strong>"
convertADTHTML (Strikethrough s) = "<del>" ++ s ++ "</del>"
convertADTHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
convertADTHTML (InlineCode s) = "<code>" ++ s ++ "</code>"
convertADTHTML (Footnote s) = "<sup>" ++ s ++ "</sup>"
convertADTHTML (Image alt url caption) = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\">"
