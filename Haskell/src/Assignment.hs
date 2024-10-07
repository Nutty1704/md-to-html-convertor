module Assignment (markdownParser, convertADTHTML, headerParser) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, string, spaces, digit, oneof, noneof, eof)
import           Control.Applicative
import           Control.Monad    (guard)

data ADT = Italics String
          | Bold String
          | Strikethrough String
          | Link String String
          | InlineCode String
          | Footnote String
          | Image String String String
          | FootnoteReference ADT String
          | FreeText String
          | Header Int [ADT]
          | Blockquote [ADT]
          | Code String String
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)


-- Parser for italics (_)
italicsParser :: Parser ADT
italicsParser = (Italics <$> (is '_' *> some (isNot '_') <* is '_'))

-- Parser for bold (**)
boldParser :: Parser ADT
boldParser = (Bold <$> (string "**" *> some (isNot '*') <* string "**"))

-- Parser for strikethrough (~~)
strikethroughParser :: Parser ADT
strikethroughParser = (Strikethrough <$> (string "~~" *> some (isNot '~') <* string "~~"))

-- Parser for Link ([...](...))
linkParser :: Parser ADT
linkParser = (Link <$> (is '[' *> some (isNot ']') <* is ']') <*> (spaces *> is '(' *> some (isNot ')') <* is ')'))

-- Parser for inline code (`...`)
inlineCodeParser :: Parser ADT
inlineCodeParser = (InlineCode <$> (is '`' *> some (isNot '`') <* is '`'))

-- Parser for footnotes ([^N])
footnoteParser :: Parser ADT
footnoteParser = (Footnote <$> (is '[' *> is '^' *> some (digit) <* is ']'))


-- inline modifier parser
inlineModifierParser :: Parser ADT
inlineModifierParser = italicsParser <|> boldParser <|> strikethroughParser <|> linkParser <|> inlineCodeParser <|> footnoteParser

-- Parser for raw text
rawTextParser :: Parser ADT
rawTextParser = FreeText <$> some (noneof "_*~`[]^\n")

-- Parser for free text
freeTextParser :: Parser [ADT]
freeTextParser = many (inlineModifierParser <|> rawTextParser)


-- Parser for image (![Alt Text](URL "Caption"))
imageParser :: Parser ADT
imageParser = (Image <$> altTextParser <*> urlParser <*> captionParser)
  where
    altTextParser = many (oneof "\t ") *> is '!' *> is '[' *> some (isNot ']') <* is ']'
    urlParser = spaces *> is '(' *> some (isNot ' ') <* is ' '
    captionParser = spaces *> is '"' *> some (isNot '"') <* is '"' <* is ')'

-- Parser for footnotes references ([^N]: ...)
footnoteReferenceParser :: Parser ADT
footnoteReferenceParser = do
    spaces
    n <- footnoteParser
    _ <- string ": "
    ref <- spaces *> some (isNot '\n')
    return (FootnoteReference n ref)


-- Parser for headers (#)
headerParser :: Parser ADT
headerParser = do
    n <- length <$> some (is '#')
    guard (n >= 1 && n <= 6)
    many (oneof "\t ")
    content <- freeTextParser
    return $ Header n content


-- Parser for Blockquotes (>)
blockquoteParser :: Parser ADT
blockquoteParser = do
  is '\n'
  spaces
  content <- freeTextParser
  return $ Blockquote content


-- Parser for code
codeParser :: Parser ADT
codeParser = do
  is '\n'
  spaces *> string "```"
  language <- spaces *> some (isNot '\n')
  is '\n'
  text <- many (isNot '`')
  spaces *> string "```"
  return $ Code language text
  



-- Parser for markdown
markdownParser :: Parser ADT
markdownParser = undefined

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML (Italics s) = "<em>" ++ s ++ "</em>"
convertADTHTML (Bold s) = "<strong>" ++ s ++ "</strong>"
convertADTHTML (Strikethrough s) = "<del>" ++ s ++ "</del>"
convertADTHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
convertADTHTML (InlineCode s) = "<code>" ++ s ++ "</code>"
convertADTHTML (Footnote s) = "<sup>" ++ s ++ "</sup>"
convertADTHTML (Image alt url caption) = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\">"
convertADTHTML (FootnoteReference (Footnote s) ref) = "<sup id=\"" ++ ref ++ "\"><a href=\"#fnref:" ++ ref ++ "\">" ++ s ++ "</a></sup>"
convertADTHTML (FreeText s) = s
convertADTHTML (Header n content) = "<h" ++ show n ++ ">" ++ concatMap convertADTHTML content ++ "</h" ++ show n ++ ">"
convertADTHTML (Blockquote content) = "<blockquote>" ++ concatMap convertADTHTML content ++ "</blockquote>"
convertADTHTML (Code language text) = "<pre><code class=\"" ++ language ++ "\">" ++ text ++ "</code></pre>"
