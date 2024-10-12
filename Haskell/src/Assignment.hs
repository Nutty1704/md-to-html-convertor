module Assignment (markdownParser, convertADTHTML, orderedListParser, tableParser) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, string, spaces, digit, oneof, noneof, tok, char, charTok)
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
          | Code String
          | OrderedList [ADT]
          | OrderedListItem [ADT]
          | Table [[ADT]]
          | Char Char
          | Paragraph [ADT]
          | HTMLElems [ADT]
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
freeTextParser = many (inlineModifierParser <|> rawTextParser <|> charParser)


-- Parser for a single character that is not a newline
charParser :: Parser ADT
charParser = Char <$> isNot '\n'


-- Parser for paragraphs
paragraphParser :: Parser ADT
paragraphParser = do
    some (is '\n')
    content <- freeTextParser
    return $ Paragraph content

-- Parser for image (![Alt Text](URL "Caption"))
imageParser :: Parser ADT
imageParser = do
    -- Consume any leading newlines
    some (is '\n')
    -- Parse the image format (![Alt Text](URL "Caption"))
    Image <$> altTextParser <*> urlParser <*> captionParser
  where
    altTextParser = many (oneof "\t ") *> is '!' *> is '[' *> some (isNot ']') <* is ']'
    urlParser = spaces *> is '(' *> some (isNot ' ') <* is ' '
    captionParser = spaces *> is '"' *> some (isNot '"') <* is '"' <* is ')'



-- Parser for footnotes references ([^N]: ...)
footnoteReferenceParser :: Parser ADT
footnoteReferenceParser = do
    some (is '\n')
    spaces
    n <- footnoteParser
    charTok ':'
    ref <- spaces *> some (isNot '\n')
    return (FootnoteReference n ref)


-- Parser for headers (#)
headerParser :: Parser ADT
headerParser = do
    many (is '\n')
    n <- length <$> some (is '#')
    guard (n >= 1 && n <= 6)
    many (oneof "\t ")
    content <- freeTextParser
    return $ Header n content


-- Parser for Blockquotes (>)
blockquoteParser :: Parser ADT
blockquoteParser = do
  some (is '\n')
  spaces
  charTok '>'
  content <- freeTextParser
  return $ Blockquote content


-- Parser for code
codeParser :: Parser ADT
codeParser = do
  some (is '\n')
  spaces *> string "```"
  language <- spaces *> some (isNot '\n')
  is '\n'
  text <- many (isNot '`')
  spaces *> string "```"
  return $ if null language then Code text else Code language text


-- Parser for an ordered list item (e.g., "1. Item")
orderedListFirstItemParser :: Parser ADT
orderedListFirstItemParser = do
    -- Parse the number (must be positive)
    n <- some digit
    guard (n == "1") -- The first item must start with 1
    -- Parse the ". " separator
    is '.' *> spaces
    -- Parse the content of the list item (it can contain text modifiers or plain text)
    content <- freeTextParser
    return $ OrderedListItem content


orderedListItemParser :: Parser ADT
orderedListItemParser = do
    -- Parse the number (must be positive)
    n <- some digit
    -- Parse the ". " separator
    is '.' *> spaces
    -- Parse the content of the list item (it can contain text modifiers or plain text)
    content <- freeTextParser
    return $ OrderedListItem content

-- Parser for a sublist item (starting with exactly 4 spaces before the number)
sublistItemParser :: Parser ADT
sublistItemParser = do
    -- Parse the leading spaces
    string "    "
    -- Parse the sublist item (same format as an ordered list item)
    orderedListItemParser

--Parser for sub list
subListParser :: Parser ADT
subListParser = do
    string "    "
    -- Parse the first ordered list item
    firstItem <- orderedListFirstItemParser
    -- Parse additional items, if present, separated by exactly one newline
    moreItems <- many (is '\n' *> sublistItemParser)
    return $ OrderedList (firstItem : moreItems)


-- Parser for an ordered list (contains at least one item, separated by exactly one newline)
orderedListParser :: Parser ADT
orderedListParser = do
    some (is '\n')
    -- Parse the first ordered list item
    firstItem <- orderedListItemParser
    -- Parse additional items, if present, separated by exactly one newline
    moreItems <- many (is '\n' *> (subListParser<|> orderedListItemParser))
    return $ OrderedList (firstItem : moreItems)


-- Parser for a table row (e.g., "| Cell1 | Cell2 |")
tableRowParser :: Parser [ADT]
tableRowParser = do
    spaces
    -- Parse the leading pipe
    charTok '|'
    -- Parse each cell, separated by pipes, trimming spaces using `tok` for cells
    cells <- some (tok (some (noneof "|\n")) <* charTok '|')
    return $ map FreeText cells

-- Parser for the separator row (e.g., "| --- | --- |")
separatorRowParser :: Parser ()
separatorRowParser = do
    spaces
    -- Parse the leading pipe
    charTok '|'
    -- Parse dashes for each column, separated by pipes, using `spaces` to ignore extra spaces
    some (spaces *> some (is '-') *> spaces <* charTok '|')
    return ()

-- Parser for the entire table
tableParser :: Parser ADT
tableParser = do
    some (is '\n')
    -- Parse the header row
    header <- tableRowParser
    -- Parse the separator row
    separatorRowParser
    -- Parse the content rows
    rows <- many tableRowParser
    -- Return the complete table representation
    return $ Table (header : rows)


-- Parser for markdown
markdownParser :: Parser ADT
markdownParser = HTMLElems <$> many(imageParser <|> footnoteReferenceParser <|> headerParser <|> blockquoteParser <|> codeParser <|> orderedListParser <|> tableParser <|> paragraphParser)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

convertADTHTML :: ADT -> String
convertADTHTML (Italics s) = "<em>" ++ s ++ "</em>"
convertADTHTML (Bold s) = "<strong>" ++ s ++ "</strong>"
convertADTHTML (Strikethrough s) = "<del>" ++ s ++ "</del>"
convertADTHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
convertADTHTML (InlineCode s) = "<code>" ++ s ++ "</code>"
convertADTHTML (Footnote s) = "<sup><a id=\"fn" ++ s ++ "ref\" href=\"#fn" ++ s ++ "\">" ++ s ++ "</a></sup>"
convertADTHTML (Image alt url caption) = "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\">"
convertADTHTML (FootnoteReference (Footnote s) ref) = "<p id=\"fn" ++ s ++ "\">" ++ ref ++ "</p>"
convertADTHTML (FreeText s) = s
convertADTHTML (Header n content) = "<h" ++ show n ++ ">" ++ concatMap convertADTHTML content ++ "</h" ++ show n ++ ">"
convertADTHTML (Blockquote content) = undefined
convertADTHTML (Code language text) = "<pre><code class=\"" ++ language ++ "\">" ++ text ++ "</code></pre>"
convertADTHTML (Code text) = "<pre><code>" ++ text ++ "</code></pre>"
convertADTHTML (OrderedList items) = "<ol>" ++ concatMap (\item -> "<li>" ++ convertADTHTML item ++ "</li>") items ++ "</ol>"
convertADTHTML (OrderedListItem content) = concatMap convertADTHTML content
convertADTHTML (Table rows) = "<table>" ++ concatMap (\row -> "<tr>" ++ concatMap (\cell -> "<td>" ++ convertADTHTML cell ++ "</td>") row ++ "</tr>") rows ++ "</table>"
convertADTHTML (Char c) = [c]
convertADTHTML (Paragraph content) = "<p>" ++ concatMap convertADTHTML content ++ "</p>"
convertADTHTML (HTMLElems elems) = "<!DOCTYPE html>\n" ++
                                    "<html lang=\"en\">\n" ++
                                    "<head>\n" ++
                                    "  <meta charset=\"UTF-8\">\n" ++
                                    "<title>Test</title>\n" ++
                                    "</head>\n" ++
                                    "<body>\n" ++
                                        concatMap convertADTHTML elems ++
                                    "</body>\n" ++
                                    "</html>"
