module Assignment (markdownParser, convertADTHTML, boldParser, italicsParser, strikethroughParser, linkParser, inlineCodeParser, footnoteParser) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, string, spaces, digit)
import           Control.Applicative

data ADT = Empty 
          | Italics String
          | Bold String
          | Strikethrough String
          | Link String String
          | InlineCode String
          | Footnote String
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


-- Parser for markdown
markdownParser :: Parser ADT
markdownParser = pure Empty

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML Empty = ""
convertADTHTML (Italics s) = "<i>" ++ s ++ "</i>"
convertADTHTML (Bold s) = "<b>" ++ s ++ "</b>"
convertADTHTML (Strikethrough s) = "<s>" ++ s ++ "</s>"
convertADTHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
convertADTHTML (InlineCode s) = "<code>" ++ s ++ "</code>"
convertADTHTML (Footnote s) = "<sup>" ++ s ++ "</sup>"
