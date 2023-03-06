module Process where
import Text.Pandoc
import Text.Pandoc.Class (trace)
import Text.Pandoc.Walk (walk)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Preprocess (filterComments)
import ReadWrite (readText, writeText)
import Turtle (FilePath)

changeInlineList :: [Inline] -> [Inline]
changeInlineList l = l >>= changeInline

-- mdbook only understands things well when all the characters are escaped
escapeSymbols :: Text -> Text
escapeSymbols text = 
  let escapes = ["{", "}", "_", "\\"] 
      add ch = T.replace ch ("\\" <> ch) 
      maps = map add escapes in
  foldr ($) text maps

changeInline :: Inline -> [Inline]
-- Inline all special symbols in the formula
changeInline (Math t txt) = return $ Math t (escapeSymbols txt)
-- Do not touch anything else
changeInline x = [x]

change :: Pandoc -> Pandoc
change = walk changeInlineList

process :: Text -> IO Text
process text =
  let filtered = filterComments text in
  let processIO = readText filtered >>= writeText . change in
  runIOorExplode processIO

transpile :: FilePath -> FilePath -> IO ()
transpile srcFile dstFile = do
  text <- TIO.readFile srcFile
  processed <- process text
  TIO.writeFile dstFile processed