module Main where
import Process (transpile, mdBookProcessor, latexProcessor)
import System.Environment (getArgs)
import Turtle
import FileTree
import Summary (genSummaryTree, genSummary)
import qualified Data.Text.IO as TIO

main :: IO ()
main = let p = latexProcessor in do
  [src, dst] <- getArgs
  isSrcDir <- testdir src
  isDstDir <- testdir dst
  isSrcFile <- testfile src
  isDstFile <- testfile dst
  if isSrcDir && isDstDir then do
    srcTree <- lsTree src
    let summaryTree = genSummaryTree srcTree
        summary = genSummary summaryTree dst in do
      -- Write summary tree
      mapCpTree (transpile p) summaryTree dst
      -- Write summary file
      TIO.writeFile (dst </> "SUMMARY.md") summary
      -- Fill in a blank file
      TIO.writeFile (dst </> "blank.md") ""
  else if isSrcFile && not isDstDir then
    transpile p src dst
  else error "Not directories nor files"
