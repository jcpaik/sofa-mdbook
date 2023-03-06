module FileTree where
import Data.List ( sortOn )
import Turtle hiding ( sortOn )
import qualified Control.Foldl as F
import System.FilePath (pathSeparator, takeFileName)
import Data.Maybe ( catMaybes ) 

data Tree a =
      File { path :: a }
    | Directory { path :: a, children :: [Tree a] }
    deriving Foldable

-- stole from containers
foldTree :: (a -> b) -> (a -> [b] -> b) -> Tree a -> b
foldTree f g = go where
    go (File x) = f x
    go (Directory x ts) = g x (map go ts)

filterTree :: (a -> Bool) -> (a -> [Tree a] -> Bool) ->
              Tree a -> Maybe (Tree a)
filterTree ffile fdir = foldTree filterFile filterDir where
  filterFile x = if ffile x then Just (File x) else Nothing
  filterDir x maybes = let children = catMaybes maybes in
    if fdir x children then Just (Directory x children) else Nothing

headTree :: Tree a -> a
headTree (File x) = x
headTree (Directory x _) = x

sortTree :: Ord a => Tree a -> Tree a
sortTree = foldTree File 
  (\x ts -> Directory x (sortOn headTree ts))

type FileTree = Tree FilePath

filePath :: FileTree -> FilePath
filePath = headTree

fileName :: FileTree -> FilePath
fileName x = takeFileName (filePath x)

lsTree :: FilePath -> IO FileTree
lsTree src = do
  isDir <- testdir src
  isFile <- testfile src
  if isDir then do
    childPaths <- fold (ls src) F.list
    children <- mapM lsTree childPaths
    return $ Directory src children
  else if isFile then
    return $ File src
  else error "Neither a directory or file"

-- Quoting Turtle.Prelude.cptree ::
  -- The `system-filepath` library treats a path like "/tmp" as a file and not
  -- a directory and fails to strip it as a prefix from `/tmp/foo`.  Adding
  -- `(</> "")` to the end of the path makes clear that the path is a
  -- directory
stripPrefixDir :: FilePath -> FilePath -> Maybe FilePath
stripPrefixDir dir path =
  let attempt = stripPrefix dir path
      fallback = stripPrefix (dir <> [System.FilePath.pathSeparator]) path in
  attempt <|> fallback

mapCpTree :: (FilePath -> FilePath -> IO ()) -> FileTree -> FilePath -> IO ()
-- transverse the source tree, copy the directory structure
-- to `dst` directory and instead of copying files in src right to dst
-- apply the map `f` from source file to corresponding destination file
mapCpTree f srcTree dst = do
  mapM_ work srcTree
    where work srcPath = let  src = filePath srcTree
                              Just relPath = stripPrefixDir src srcPath
                              dstPath = dst </> relPath in
                              do
                                isDir <- testdir srcPath
                                when isDir $ mktree dstPath
                                isFile <- testfile srcPath
                                when isFile $ f srcPath dstPath