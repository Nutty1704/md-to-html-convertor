module Assignment (markdownParser, convertADTHTML, getTime) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, string, spaces, digit, oneof, noneof, inlineSpace, charTok)
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
          | CodeNoLang String
          | OrderedList [ADT]
          | OrderedListItem [ADT]
          | Table [[ADT]]
          | TableCell [ADT]
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
rawTextParser :: String -> Parser ADT
rawTextParser except = FreeText <$> some (noneof except)

inlineModifierSymbols :: String
inlineModifierSymbols = "_*~`[]^"

-- Parser for free text
freeTextParserExcept :: String -> String -> Parser [ADT]
freeTextParserExcept rawExcept charExcept = some (inlineModifierParser <|> rawTextParser (rawExcept) <|> charParser charExcept)

freeTextParser :: Parser [ADT]
freeTextParser = freeTextParserExcept (inlineModifierSymbols ++ "\n") "\n"

-- Utility function to trim trailing spaces from a FreeText element
trimTrailingSpaces :: ADT -> ADT
trimTrailingSpaces (FreeText s) = FreeText $ reverse $ dropWhile (== ' ') $ reverse s
trimTrailingSpaces other = other

-- Parser for a single character that is not a newline
charParser :: String -> Parser ADT
charParser exclude = Char <$> noneof exclude


-- Parser for paragraphs
paragraphParser :: Parser ADT
paragraphParser = do
    _ <- many (is '\n')
    content <- freeTextParser
    return $ Paragraph content


-- Parser for image (![Alt Text](URL "Caption"))
imageParser :: Parser ADT
imageParser = do
    -- Consume any leading newlines
    _ <- many (is '\n')
    -- Parse the image format (![Alt Text](URL "Caption"))
    Image <$> altTextParser <*> urlParser <*> captionParser
  where
    altTextParser = many (oneof "\t ") *> is '!' *> is '[' *> some (isNot ']') <* is ']'
    urlParser = spaces *> is '(' *> some (isNot ' ') <* is ' '
    captionParser = spaces *> is '"' *> some (isNot '"') <* is '"' <* is ')'



-- Parser for footnotes references ([^N]: ...)
footnoteReferenceParser :: Parser ADT
footnoteReferenceParser = do
    _ <- many (is '\n')
    _ <- spaces
    n <- footnoteParser
    _ <- charTok ':'
    ref <- spaces *> some (isNot '\n')
    return (FootnoteReference n ref)


-- Parser for headers (#)
normalHeaderParser :: Parser ADT
normalHeaderParser = do
    _ <- many (is '\n')
    n <- length <$> some (is '#')
    guard (n >= 1 && n <= 6)
    _ <- many (oneof "\t ")
    content <- freeTextParser
    return $ Header n content


altHeaderParser :: Parser ADT
altHeaderParser = do
    _ <- many (is '\n') *> spaces
    text <- freeTextParser <* is '\n'
    nxtLine <- inlineSpace *> many (isNot '\n')
    let n = if all (== '=') nxtLine && length nxtLine > 1
                then 1 
            else if all (== '-') nxtLine && length nxtLine > 1
                then 2 
            else 0
    guard (n > 0)
    return $ Header n text


headerParser :: Parser ADT
headerParser = normalHeaderParser <|> altHeaderParser


-- Parser for Blockquotes (>)
blockquoteParser :: Parser ADT
blockquoteParser = do
  _ <- many (is '\n')
  lines <- some blockquoteLineParser
  return $ Blockquote lines


-- Parser for single blockquote line
blockquoteLineParser :: Parser ADT
blockquoteLineParser = do
  _ <- spaces
  _ <- charTok '>'
  content <- freeTextParser
  return $ Paragraph content


-- Parser for code
codeParser :: Parser ADT
codeParser = do
  _ <- many (is '\n')
  _ <- spaces *> string "```"
  language <- spaces *> some (isNot '\n')
  _ <- is '\n'
  content <- many (isNot '`')
  _ <- spaces *> string "```"
  let text = if last content == '\n' then init content else content
  return $ if null language then CodeNoLang text else Code language text


-- Parser for an ordered list item (e.g., "1. Item")
orderedListFirstItemParser :: Parser ADT
orderedListFirstItemParser = do
    -- Parse the number (must be positive)
    n <- some digit
    guard (n == "1") -- The first item must start with 1
    -- Parse the ". " separator
    _ <- is '.' *> inlineSpace
    -- Parse the content of the list item (it can contain text modifiers or plain text)
    content <- freeTextParser
    return $ OrderedListItem content


orderedListItemParser :: Parser ADT
orderedListItemParser = do
    -- Parse the number (must be positive)
    n <- some digit
    -- Parse the ". " separator
    _ <- is '.' *> spaces
    -- Parse the content of the list item (it can contain text modifiers or plain text)
    content <- freeTextParser
    return $ OrderedListItem content

-- Parser for a sublist item (starting with exactly 4 spaces before the number)
sublistItemParser :: Parser ADT
sublistItemParser = do
    -- Parse the leading spaces
    _ <- string "    "
    -- Parse the sublist item (same format as an ordered list item)
    orderedListItemParser

--Parser for sub list
subListParser :: Parser ADT
subListParser = do
    _ <- string "    "
    -- Parse the first ordered list item
    firstItem <- orderedListFirstItemParser
    -- Parse additional items, if present, separated by exactly one newline
    moreItems <- many (is '\n' *> sublistItemParser)
    return $ OrderedList (firstItem : moreItems)


-- Parser for an ordered list (contains at least one item, separated by exactly one newline)
orderedListParser :: Parser ADT
orderedListParser = do
    _ <- many (is '\n')
    -- Parse the first ordered list item
    firstItem <- orderedListFirstItemParser
    -- Parse additional items, if present, separated by exactly one newline
    moreItems <- many (is '\n' *> (subListParser <|> orderedListItemParser))
    let finalList = mergeSublistIntoPreviousItem (firstItem : moreItems)
    return $ OrderedList finalList


-- Function to merge sublists into the previous list item
mergeSublistIntoPreviousItem :: [ADT] -> [ADT]
mergeSublistIntoPreviousItem = foldl processItem []
  where
    processItem acc currentItem =
      case currentItem of
        OrderedList subItems ->
          case acc of
            (OrderedListItem prevItems : rest) ->
              -- Merge sublist into the previous list item
              OrderedListItem (prevItems ++ [OrderedList subItems]) : rest
            _ -> acc ++ [OrderedList subItems] -- If no previous item exists, just add the sublist
        _ -> acc ++ [currentItem]



-- Parser for table cell
tableCellParser :: Parser ADT
tableCellParser = do
    _ <- spaces
    -- Parse the cell content (it can contain text modifiers or plain text)
    content <- freeTextParserExcept (inlineModifierSymbols ++ "|\n") "|\n"
    let trimmedContent = case content of
            [] -> []
            _ -> init content ++ [trimTrailingSpaces (last content)]
    return $ TableCell trimmedContent


-- Parser for a table row (e.g., "| Cell1 | Cell2 |")
tableRowParser :: Parser [ADT]
tableRowParser = do
    _ <- spaces
    -- Parse the leading pipe
    _ <- charTok '|'
    -- Parse each cell, separated by pipes
    cells <- some (tableCellParser <* charTok '|')
    return $ cells

-- Parser for the separator row (e.g., "| --- | --- |")
separatorRowParser :: Parser ()
separatorRowParser = do
    _ <- spaces
    -- Parse the leading pipe
    _ <- charTok '|'
    -- Parse dashes for each column, separated by pipes, using `spaces` to ignore extra spaces
    _ <- some (spaces *> some (is '-') *> spaces <* charTok '|')
    return ()

-- Parser for the entire table
tableParser :: Parser ADT
tableParser = do
    _ <- many (is '\n')
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
markdownParser = HTMLElems <$> some(imageParser <|> footnoteReferenceParser <|> headerParser <|> blockquoteParser <|> codeParser <|> orderedListParser <|> tableParser <|> paragraphParser)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime


-- Helper function to add indentation
indent :: Int -> String
indent level = replicate (level * 4) ' '  -- Four spaces per indentation level

-- Convert ADT to HTML with indentation
convertWithIndent :: Int -> ADT -> String
-- Inline elements (no newlines after them)
convertWithIndent _ (Italics s) = "<em>" ++ s ++ "</em>"
convertWithIndent _ (Bold s) = "<strong>" ++ s ++ "</strong>"
convertWithIndent _ (Strikethrough s) = "<del>" ++ s ++ "</del>"
convertWithIndent _ (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
convertWithIndent _ (InlineCode s) = "<code>" ++ s ++ "</code>"
convertWithIndent _ (Footnote s) = "<sup><a id=\"fn" ++ s ++ "ref\" href=\"#fn" ++ s ++ "\">" ++ s ++ "</a></sup>"
convertWithIndent _ (FreeText s) = s

-- Block elements (with newlines and indentation)
convertWithIndent level (Image alt url caption) =
    indent level ++ "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\">\n"

convertWithIndent level (FootnoteReference (Footnote s) ref) =
    indent level ++ "<p id=\"fn" ++ s ++ "\">" ++ ref ++ "</p>\n"

convertWithIndent level (Header n content) =
    indent level ++ "<h" ++ show n ++ ">" ++ concatMap (convertWithIndent 0) content ++ "</h" ++ show n ++ ">\n"

convertWithIndent level (Blockquote content) =
    indent level ++ "<blockquote>\n" ++ concatMap (convertWithIndent (level + 1)) content ++ indent level ++ "</blockquote>\n"

convertWithIndent level (Code language text) =
    indent level ++ "<pre><code class=\"language-" ++ language ++ "\">" ++ text ++ "</code></pre>\n"

convertWithIndent level (CodeNoLang text) =
    indent level ++ "<pre><code>" ++ text ++ "</code></pre>\n"

-- Convert an ordered list
convertWithIndent level (OrderedList items) =
    indent level ++ "<ol>\n" ++ concatMap (convertWithIndent (level + 1)) items ++ indent level ++ "</ol>\n"

-- Convert an ordered list item
convertWithIndent level (OrderedListItem content) =
    if containsSublist content
    then
        indent level ++ "<li>" ++ concatMap (convertWithIndent (level + 1)) (init content) ++ "\n" ++
        convertWithIndent (level + 1) (last content) ++
        indent level ++ "</li>\n"
    else -- No sublist, format as a single line
        indent level ++ "<li>" ++ concatMap (convertWithIndent (level + 1)) content ++ "</li>\n"


convertWithIndent level (TableCell content) = concatMap (convertWithIndent 0) content

convertWithIndent level (Table (header:rows)) = 
    -- Start with the table tag
    indent level ++ "<table>\n" ++
    -- The header row in the <thead> section
    indent (level + 1) ++ "<thead>\n" ++
    indent (level + 2) ++ "<tr>\n" ++ 
    concatMap (\cell -> indent (level + 3) ++ "<th>" ++ convertWithIndent (level + 3) cell ++ "</th>\n") header ++
    indent (level + 2) ++ "</tr>\n" ++
    indent (level + 1) ++ "</thead>\n" ++
    -- The remaining rows in the <tbody> section
    indent (level + 1) ++ "<tbody>\n" ++
    concatMap (\row -> indent (level + 2) ++ "<tr>\n" ++
        concatMap (\cell -> indent (level + 3) ++ "<td>" ++ convertWithIndent (level + 3) cell ++ "</td>\n") row ++
        indent (level + 2) ++ "</tr>\n") rows ++
    indent (level + 1) ++ "</tbody>\n" ++
    indent level ++ "</table>\n"


convertWithIndent level (Char c) = indent level ++ [c] ++ "\n"

convertWithIndent level (Paragraph content) =
    indent level ++ "<p>" ++ concatMap (convertWithIndent (level + 1)) content ++ "</p>\n"

convertWithIndent level (HTMLElems elems) =
    indent level ++ "<!DOCTYPE html>\n" ++
    indent level ++ "<html lang=\"en\">\n\n" ++
    indent (level) ++ "<head>\n" ++
    indent (level + 1) ++ "<meta charset=\"UTF-8\">\n" ++
    indent (level + 1) ++ "<title>Test</title>\n" ++
    indent (level) ++ "</head>\n\n" ++
    indent (level) ++ "<body>\n" ++
    concatMap (convertWithIndent (level + 1)) elems ++
    indent (level) ++ "</body>\n\n" ++
    indent level ++ "</html>\n"
convertWithIndent _ _ = ""



-- Function to check if the content contains a sublist
containsSublist :: [ADT] -> Bool
containsSublist = any isOrderedList
  where
    isOrderedList (OrderedList _) = True
    isOrderedList _               = False

-- Original convertADTHTML function now using the helper
convertADTHTML :: ADT -> String
convertADTHTML adt = convertWithIndent 0 adt
