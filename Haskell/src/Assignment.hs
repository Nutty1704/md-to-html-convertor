module Assignment (markdownParser, convertADTHTML, getTime) where


import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, string, spaces, digit, oneof, noneof, inlineSpace, charTok)
import           Control.Applicative
import           Control.Monad    (guard, when)

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
          | Form String String [ADT]
          | Label String
          | Input String String
          | TextArea String
          | Button String String
  -- Your ADT **must** derive Show.
  deriving (Show, Eq)


-- Utility functions

-- Function to check if the content contains a sublist
containsSublist :: [ADT] -> Bool
containsSublist = any isOrderedList
  where
    isOrderedList (OrderedList _) = True
    isOrderedList _               = False

-- Function to trim trailing spaces from a FreeText element
trimTrailingSpaces :: ADT -> ADT
trimTrailingSpaces (FreeText s) = FreeText $ reverse $ dropWhile (== ' ') $ reverse s
trimTrailingSpaces other = other

-- Function to extract text from FreeText elements
extractText :: ADT -> String
extractText (FreeText s) = s
extractText _ = ""


-- PARSERS


-- Parsers for inline modifiers

-- Parser for italics (_)
italicsParser :: Parser ADT
italicsParser = Italics <$> (is '_' *> some (noneof "_\n") <* is '_')

-- Parser for bold (**)
boldParser :: Parser ADT
boldParser = Bold <$> (string "**" *> some (noneof "*\n") <* string "**")

-- Parser for strikethrough (~~)
strikethroughParser :: Parser ADT
-- Parser for strikethrough (~~)
strikethroughParser :: Parser ADT
strikethroughParser = 
    Strikethrough <$> 
    (string "~~" *> 
    some (noneof "~\n") <* 
    string "~~")

-- Parser for Link ([...](...))
linkParser :: Parser ADT
linkParser = 
    Link <$> 
    (is '[' *> 
    some (noneof "]\n") <* 
    is ']') <*> 
    (spaces *> 
    is '(' *> 
    some (noneof ")\n") <* 
    is ')')

-- Parser for inline code (`...`)
inlineCodeParser :: Parser ADT
inlineCodeParser = (InlineCode <$> (is '`' *> some (noneof "`\n") <* is '`'))

-- Parser for footnotes ([^N])
footnoteParser :: Parser ADT
footnoteParser = (Footnote <$> (is '[' *> is '^' *> some (digit) <* is ']'))


-- inline modifier parser
inlineModifierParser :: Parser ADT
inlineModifierParser = italicsParser <|> boldParser <|>strikethroughParser
                        <|> linkParser <|> inlineCodeParser <|> footnoteParser

-- Parser for raw text
rawTextParser :: String -> Parser ADT
rawTextParser except = FreeText <$> some (noneof except)

inlineModifierSymbols :: String
inlineModifierSymbols = "_*~`[]^"

-- Parser for a single character that is not a newline
charParser :: String -> Parser ADT
charParser exclude = Char <$> noneof exclude

-- Parser for free text
freeTextParserExcept :: String -> String -> Parser [ADT]
freeTextParserExcept rawExcept charExcept = some (
    inlineModifierParser <|> rawTextParser (rawExcept) <|> charParser charExcept
    )

freeTextParser :: Parser [ADT]
freeTextParser = freeTextParserExcept (inlineModifierSymbols ++ "\n") "\n"



-- Parsers for block elements


-- Parser for paragraphs
paragraphParser :: Parser ADT
paragraphParser = do
    _ <- many (is '\n')
    content <- freeTextParser
    return $ Paragraph content


-- Parser for image (![Alt Text](URL "Caption"))
imageParser :: Parser ADT
imageParser = do
    _ <- many (is '\n')
    Image <$> altTextParser <*> urlParser <*> captionParser
  where
    -- Parses ![....] section of the image
    altTextParser =
        inlineSpace *>
        is '!' *>
        is '[' *>
        some (noneof "]\n") <*
        is ']'
    -- Parses (....) url section of the image until first space
    urlParser =
        inlineSpace *>
        is '(' *>
        some (noneof "\t\r\f\v \n")
    -- Parses "...." caption section of the image until the closing ")
    captionParser =
        inlineSpace *>
        is '"' *>
        some (noneof "\"\n") <*
        is '"' <*
        is ')'


-- Parser for footnotes references ([^N]: ...)
footnoteReferenceParser :: Parser ADT
footnoteReferenceParser = do
    _ <- many (is '\n') <* inlineSpace
    n <- footnoteParser <* charTok ':'  -- parse the [^N]: part
    ref <- inlineSpace *> some (isNot '\n')  -- parse the remaining part
    return $ FootnoteReference n ref


-- Parser for headers (#)
normalHeaderParser :: Parser ADT
normalHeaderParser = do
    _ <- many (is '\n') <* inlineSpace
    n <- length <$> some (is '#')  -- Count the number of #s
    guard (n >= 1 && n <= 6)        -- Ensure valid number of #s
    _ <- inlineSpace
    content <- freeTextParser       -- Parse the content of the header
    return $ Header n content

-- Parser for alternative headers (= or -)
altHeaderParser :: Parser ADT
altHeaderParser = do
    _ <- many (is '\n') *> spaces
    text <- freeTextParser <* is '\n'  -- Parse the header text
    nxtLine <- inlineSpace *> many (isNot '\n')  -- Parse the next line
    let n = if all (== '=') nxtLine && length nxtLine > 1
                then 1 
            else if all (== '-') nxtLine && length nxtLine > 1
                then 2 
            else 0
    guard (n > 0)  -- Ensure the next line is all = or -
    return $ Header n text


headerParser :: Parser ADT
headerParser = normalHeaderParser <|> altHeaderParser


-- Parser for Blockquotes (>)
blockquoteParser :: Parser ADT
blockquoteParser = do
  _ <- many (is '\n')
  lines <- some blockquoteLineParser  -- Parse all the consecutive blockquote lines
  return $ Blockquote lines


-- Parser for single blockquote line
blockquoteLineParser :: Parser ADT
blockquoteLineParser = do
  _ <- spaces <* charTok '>'
  content <- freeTextParser  -- Parse the content of the blockquote line
  return $ Paragraph content


-- Parser for code
codeParser :: Parser ADT
codeParser = do
  _ <- many (is '\n')
  _ <- inlineSpace *> string "```"
  language <- some (isNot '\n') <* is '\n'
  content <- many (isNot '`')
  _ <- spaces *> string "```"
  let text = if last content == '\n'
                then init content -- Strip the last newline if present
                else content
  return $ if null language
            then CodeNoLang text
            else Code language text


-- Parser for an ordered list item
orderedListItemParser :: Bool -> Parser ADT
orderedListItemParser isFirst = do
    n <- some digit
    when isFirst $ guard (n == "1") -- The first item must start with 1
    _ <- is '.' *> spaces
    content <- freeTextParser
    return $ OrderedListItem content

-- Parser for a sublist item (starting with exactly 4 spaces before the number)
sublistItemParser :: Parser ADT
sublistItemParser = do
    _ <- string "    "  -- Match exactly 4 spaces to identify a sublist
    orderedListItemParser False

--Parser for sub list
subListParser :: Parser ADT
subListParser = do
    _ <- string "    "
    firstItem <- orderedListItemParser True
    moreItems <- many (is '\n' *> sublistItemParser)  -- Parse the subsequent sublist items
    return $ OrderedList (firstItem : moreItems)


-- Parser for an ordered list
orderedListParser :: Parser ADT
orderedListParser = do
    _ <- many (is '\n')
    firstItem <- orderedListItemParser True
    moreItems <- many (is '\n' *> (subListParser <|> orderedListItemParser False))  -- Parse the subsequent sublist or list items
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
            -- If the last item is an OrderedListItem, merge the sublist into it
            [] -> [OrderedList subItems]  -- If acc is empty, add the sublist as the first item
            _  ->
              let prevItems = init acc
                  lastItem = last acc 
              in case lastItem of
                    -- If the last item is an OrderedListItem then merge the sublist into it (append the sublist to its content)
                   OrderedListItem content ->
                     prevItems ++ [OrderedListItem (content ++ [OrderedList subItems])] -- Merge the sublist into the last item (append to the content)
                   _ -> acc ++ [OrderedList subItems] -- Otherwise leave the sublist as a separate item
        _ -> acc ++ [currentItem]



-- Parser for table cell
tableCellParser :: Parser ADT
tableCellParser = do
    _ <- spaces
    content <- freeTextParserExcept (inlineModifierSymbols ++ "|\n") "|\n" -- Parse until the next pipe or newline
    let trimmedContent = case content of
            [] -> []
            _ -> init content ++ [trimTrailingSpaces (last content)] -- Remove trailing spaces from the last element
    return $ TableCell trimmedContent


-- Parser for a table row
tableRowParser :: Int -> Parser [ADT]
tableRowParser numCells = do
    _ <- spaces <* charTok '|'
    cells <- some (tableCellParser <* charTok '|')  -- Parse all the cells in the row
    let n = length cells
    guard (n == numCells || numCells == 0) -- Ensure same number of cells as the header row
    return $ cells

-- Parser for the separator row
separatorRowParser :: Int -> Parser ()
separatorRowParser numCells = do
    _ <- spaces <* charTok '|'
    n <- length <$> some (spaces *> some (is '-') *> spaces <* charTok '|') -- Count the number of cells
    guard (n == numCells) -- Ensure same number of cells as the header row
    return ()

-- Parser for the entire table
tableParser :: Parser ADT
tableParser = do
    _ <- many (is '\n')
    header <- tableRowParser 0
    let n = length header
    separatorRowParser n
    rows <- many $ tableRowParser n -- Parse all the rows
    return $ Table (header : rows)


-- Parser for form elements

-- Parser for label (LAB : name)
labelParser :: Parser ADT
labelParser = do
    _ <- inlineSpace <* string "LAB" <* inlineSpace <* charTok ':'
    -- Parse the content of the label then strip trailing spaces then extract the text as a string
    content <- (extractText . trimTrailingSpaces) <$> rawTextParser "\n"
    return $ Label content

-- Parser for input (INP : type ; name)
inputParser :: Parser ADT
inputParser = do
    _ <- inlineSpace <* string "INP" <* inlineSpace <* charTok ':'
    -- Similar processing to the label parser, parses the type of the input and the name
    inpType <- (extractText . trimTrailingSpaces) <$> rawTextParser ";\n" <* charTok ';'
    name <- (extractText . trimTrailingSpaces) <$> rawTextParser "\n"
    return $ Input inpType name

-- Parser for text area (TA : name)
textAreaParser :: Parser ADT
textAreaParser = do
    _ <- inlineSpace <* string "TA" <* inlineSpace <* charTok ':'
    -- Similar processing to the label parser, parses the name of the text area
    name <- (extractText . trimTrailingSpaces) <$> rawTextParser "\n"
    return $ TextArea name

-- Parser for button (BTN : type ; name)
buttonParser :: Parser ADT
buttonParser = do
    _ <- inlineSpace <* string "BTN" <* inlineSpace <* charTok ':'
    -- Similar processing to the label parser, parses the type of the button and the name
    btnType <- (extractText . trimTrailingSpaces) <$> rawTextParser ";\n" <* charTok ';'
    name <- (extractText . trimTrailingSpaces) <$> rawTextParser "\n"
    return $ Button btnType name

formElementParser :: Parser ADT
formElementParser = labelParser <|> inputParser <|> textAreaParser <|> buttonParser

-- Parser for form (FORM : action ; method)
formParser :: Parser ADT
formParser = do
    _ <- many (is '\n') <* inlineSpace <* string "FORM" <* inlineSpace
    -- Parse the action and method of the form (processed similarly to the label parser)
    action <- (extractText . trimTrailingSpaces) <$> rawTextParser " \n" <* inlineSpace
    method <- (extractText . trimTrailingSpaces) <$> rawTextParser "\n" <* inlineSpace
    content <- many (is '\n' *> formElementParser)
    return $ Form action method content



-- Parser for markdown
markdownParser :: Parser ADT
markdownParser = HTMLElems <$> 
    some(
        imageParser <|> footnoteReferenceParser <|>
        headerParser <|> blockquoteParser <|>
        codeParser <|> orderedListParser <|>
        tableParser <|> formParser <|>
        paragraphParser
        )

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime


-- Helper function to add indentation
indent :: Int -> String
indent level = replicate (level * 4) ' '  -- Four spaces per indentation level

-- Convert ADT to HTML with indentation
convertWithIndent :: Int -> ADT -> String
-- Inline elements
convertWithIndent _ (Italics s) =
    "<em>" ++ s ++ "</em>"

convertWithIndent _ (Bold s) =
    "<strong>" ++ s ++ "</strong>"

convertWithIndent _ (Strikethrough s) =
    "<del>" ++ s ++ "</del>"

convertWithIndent _ (Link text url) =
    "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"

convertWithIndent _ (InlineCode s) =
    "<code>" ++ s ++ "</code>"

convertWithIndent _ (Footnote s) =
    "<sup><a id=\"fn" ++ s ++ "ref\" href=\"#fn" ++ s ++ "\">" ++ s ++ "</a></sup>"

convertWithIndent _ (FreeText s) = s


-- Block elements
convertWithIndent level (Image alt url caption) =
    indent level ++ 
    "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ caption ++ "\">\n"

convertWithIndent level (FootnoteReference (Footnote s) ref) =
    indent level ++
    "<p id=\"fn" ++ s ++ "\">" ++ ref ++ "</p>\n"

convertWithIndent level (Header n content) =
    indent level ++
    "<h" ++ show n ++ ">" ++
    concatMap (convertWithIndent 0) content ++
    "</h" ++ show n ++ ">\n"

-- Wrap blockquote content in <blockquote> tags then call convertWithIndent on each line and concatenate
convertWithIndent level (Blockquote content) =
    indent level ++
    "<blockquote>\n" ++ concatMap (convertWithIndent (level + 1)) content ++ indent level ++ "</blockquote>\n"

convertWithIndent level (Code language text) =
    indent level ++
    "<pre><code class=\"language-" ++ language ++ "\">" ++ text ++ "</code></pre>\n"

convertWithIndent level (CodeNoLang text) =
    indent level ++
    "<pre><code>" ++ text ++ "</code></pre>\n"

-- Wrap the list in <ol> tags then call convertWithIndent on each item and concatenate
convertWithIndent level (OrderedList items) =
    indent level ++
    "<ol>\n" ++
    concatMap (convertWithIndent (level + 1)) items
    ++ indent level ++ "</ol>\n"

-- If the list item contains a sublist, format it as a list item with sublist, otherwise format as a single line
convertWithIndent level (OrderedListItem content) =
    if containsSublist content
    then -- Contains a sublist, format as a list item with sublist
        indent level ++ "<li>" ++
        concatMap (convertWithIndent (level + 1)) (init content) ++
        "\n" ++ convertWithIndent (level + 1) (last content) ++
        indent level ++ "</li>\n"
    else -- No sublist, format as a single line
        indent level ++ "<li>" ++
        concatMap (convertWithIndent (level + 1)) content ++
        "</li>\n"


convertWithIndent level (TableCell content) =
    concatMap (convertWithIndent 0) content


convertWithIndent level (Table (header:rows)) = 
    indent level ++ "<table>\n" ++
    -- The header row in the <thead> section
    indent (level + 1) ++ "<thead>\n" ++
    indent (level + 2) ++ "<tr>\n" ++ 
    concatMap (\cell -> indent (level + 3) ++ "<th>" ++
                convertWithIndent (level + 3) cell ++ "</th>\n"
            ) header ++
    indent (level + 2) ++ "</tr>\n" ++
    indent (level + 1) ++ "</thead>\n" ++
    -- The remaining rows in the <tbody> section
    indent (level + 1) ++ "<tbody>\n" ++
    concatMap (\row -> indent (level + 2) ++ "<tr>\n" ++
        concatMap (
            \cell -> indent (level + 3) ++ "<td>" ++ convertWithIndent (level + 3) cell ++ "</td>\n"
            ) row ++
        indent (level + 2) ++ "</tr>\n") rows ++
    indent (level + 1) ++ "</tbody>\n" ++
    indent level ++ "</table>\n"


convertWithIndent level (Char c) =
    indent level ++ [c] ++ "\n"

convertWithIndent level (Paragraph content) =
    indent level ++
    "<p>" ++ concatMap (convertWithIndent (level + 1)) content ++ "</p>\n"


convertWithIndent level (Label content) =
    indent level ++ "<label for=\"" ++ content ++ "\">" ++
    content ++ "</label>\n"

convertWithIndent level (Input inpType name) =
    indent level ++ "<input type=\"" ++ inpType ++ "\" name=\"" ++ name ++ "\">\n"

convertWithIndent level (TextArea name) =
    indent level ++ "<textarea name=\"" ++ name ++ "\"></textarea>\n"

convertWithIndent level (Button btnType name) =
    indent level ++ "<button type=\"" ++ btnType ++ "\">" ++ name ++ "</button>\n"

convertWithIndent level (Form action method content) = 
    indent level ++ "<form action=\"" ++ action ++
    "\" method=\"" ++ method ++ "\">\n" ++
    -- Convert each form element with indentation
    concatMap (convertWithIndent (level + 1)) content ++
    indent level ++ "</form>\n"

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

-- Convert ADT to HTML
convertADTHTML :: ADT -> String
convertADTHTML = convertWithIndent 0
