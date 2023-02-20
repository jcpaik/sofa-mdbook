#!/usr/bin/env stack
{- stack script --resolver lts-20.11 --verbosity=info
  --package process
  --package pandoc
  --package pandoc-types
  --package text
-}
{-# LANGUAGE OverloadedStrings   #-}

import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readerOptions :: ReaderOptions
readerOptions = def {
  readerExtensions = extensionsFromList [
    Ext_tex_math_dollars,
    Ext_raw_html,
    Ext_strikeout
    -- ,Ext_wikilinks_title_after_pipe
  ]
}

readText :: Text -> PandocIO Pandoc
readText = readMarkdown readerOptions

writerOptions :: WriterOptions
writerOptions = def {
  writerExtensions = extensionsFromList [
    Ext_raw_html,
    Ext_strikeout
  ]
}

writeText :: Pandoc -> PandocIO Text
writeText = writeMarkdown writerOptions

changeInlineList :: [Inline] -> [Inline]
changeInlineList l = l >>= changeInline

changeInline :: Inline -> [Inline]
-- Replace LaTeX
changeInline (Math InlineMath txt) = return (Str ("\\(" <> txt <> "\\)"))
changeInline (Math DisplayMath txt) = return (Str ("\\[" <> txt <> "\\]"))
-- Do not touch anything else
changeInline x = [x]

change :: Pandoc -> Pandoc
change = walk changeInlineList

main :: IO ()
main = do
  input <- TIO.getContents
  output <- runIOorExplode (
    (change <$> readText input) >>= writeText)
  TIO.putStrLn output