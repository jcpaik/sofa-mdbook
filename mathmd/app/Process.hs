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

changeInline :: Inline -> [Inline]
-- Replace LaTeX
changeInline (Math InlineMath txt) = return (Str ("\\\\(" <> txt <> "\\\\)"))
changeInline (Math DisplayMath txt) = return (Str ("\\\\[" <> txt <> "\\\\]"))
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