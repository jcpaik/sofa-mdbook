module Process where
import Text.Pandoc hiding (trace)
import Text.Pandoc.Walk (walk)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Preprocess (filterComments)
import ReadWrite (readText)
import Turtle (FilePath)
import Control.Monad ((>=>))
import Debug.Trace (trace, traceShow, traceShowId)

data TheoremEnvType =
  Theorem | Lemma | Definition | Corollary | Remark | FigureEnv
  deriving (Eq, Enum, Show)

theoremEnvTypeText :: TheoremEnvType -> Text
theoremEnvTypeText t = 
  case t of
    Theorem -> "Theorem"
    Lemma -> "Lemma"
    Definition -> "Definition"
    Corollary -> "Corollary"
    Remark -> "Remark"
    FigureEnv -> "Figure"

-- "Theorem" -> Just Theorem
parseTheoremEnvType :: Text -> Maybe TheoremEnvType
parseTheoremEnvType text =
  case text of
    "Theorem" -> Just Theorem
    "Lemma" -> Just Lemma
    "Definition" -> Just Definition
    "Corollary" -> Just Corollary
    "Remark" -> Just Remark
    "Figure" -> Just FigureEnv
    _ -> Nothing

data TheoremEnv = TheoremEnv
    {
      theoremEnvType        :: TheoremEnvType
    , theoremEnvTag         :: Text
    , theoremEnvDescription :: [Block]
    }
  deriving Show

-- "[some-thm-name]." -> Just "some-thm-name"
parseTheoremEnvTag :: Text -> Maybe Text
parseTheoremEnvTag = T.stripPrefix "[" >=> T.stripSuffix "]."

clearTags :: [Inline] -> [Inline]
clearTags [] = []
clearTags [Str str] | T.isPrefixOf "^" str = []
clearTags [x] = [x]
clearTags (Space : Str str : rest) | T.isPrefixOf "^" str = rest
clearTags (x : y) = x : clearTags y

theoremEnv :: Block -> Maybe TheoremEnv
theoremEnv (BlockQuote
  (Para ((Strong [ Str typeStr , Space , Str nameStr ]) : Space :
    restFirstPara) : restBlocks)) = do
      envType <- parseTheoremEnvType typeStr
      envTag <- parseTheoremEnvTag nameStr
      -- Reject any figure captions with more than single paragraph
      if envType == FigureEnv && restBlocks /= [] then Nothing 
      else Just TheoremEnv
        {
          theoremEnvType = envType
        , theoremEnvTag = envTag
        , theoremEnvDescription = tagRemoved
        }
      where 
        quoted = Para restFirstPara : restBlocks
        tagRemoved = walk clearTags quoted
theoremEnv _ = Nothing

data Processor = Processor
    {
      processEquation      :: MathType -> Text -> Inline
    , processTheoremEnv    :: TheoremEnv -> [Block]
    , processImage         :: Attr -> [Inline] -> Target -> Inline
    , processLink          :: Attr -> [Inline] -> Target -> Inline
    , writeText            :: Pandoc -> PandocIO Text
    }

processPandoc :: Processor -> Pandoc -> Pandoc
processPandoc (Processor {
    processEquation = procEq
  , processImage = procImg
  , processLink = procLink
  , processTheoremEnv = procTheoremEnv }) =
  walk inlineWalker . walk blockWalker where
    inlineWalker (Math t txt) = procEq t txt
    inlineWalker (Image attr desc target) =
      procImg attr desc target
    inlineWalker (Link attr desc target) =
      procLink attr desc target
    inlineWalker x = x

    blockWalker blocks = [outBlock | 
      block <- blocks, outBlock <- expandBlock block]
    expandBlock block = maybe [block] 
      procTheoremEnv (theoremEnv block)

processText :: Processor -> Text -> IO Text
processText processor text =
  let filtered = filterComments text in runIOorExplode (do
    readPandoc <- readText filtered
    let writePandoc = processPandoc processor readPandoc in
      writeText processor writePandoc)

transpile :: Processor -> FilePath -> FilePath -> IO ()
transpile processor srcFile dstFile = do
  text <- TIO.readFile srcFile
  processed <- processText processor text
  TIO.writeFile dstFile processed

mdBookProcessor :: Processor
mdBookProcessor = Processor
  {
    processEquation = mdBookProcessEquation
  , processTheoremEnv = mdBookProcessTheoremEnv
  , processImage = Image
  , processLink = Link
  , writeText = mdBookWriteText
  }

-- mdbook only understands things well when all the characters are escaped
mdBookProcessEquation :: MathType -> Text -> Inline
mdBookProcessEquation t txt =
  Math t (escapeAllSymbols txt) where
  escapeAllSymbols text =
    let escapes = ["{", "}", "_", "\\"]
        add ch = T.replace ch ("\\" <> ch)
        maps = map add escapes in
    foldr ($) text maps


mdBookProcessTheoremEnv :: TheoremEnv -> [Block]
mdBookProcessTheoremEnv (TheoremEnv
  { theoremEnvType = typ
  , theoremEnvTag = tag
  , theoremEnvDescription = desc }) =
    return $ BlockQuote (firstPara : restBlocks) where
      Para restFirstPara : restBlocks = desc
      typeText = theoremEnvTypeText typ
      tagText = "[" <> tag <> "]."
      header = Strong [ Str typeText , Space , Str tagText ]
      firstPara = Para (header : Space : restFirstPara)

mdBookWriteText :: Pandoc -> PandocIO Text
mdBookWriteText = writeMarkdown options where
  options = def {
    writerExtensions = extensionsFromList [
      Ext_tex_math_double_backslash,
      Ext_raw_html,
      Ext_strikeout
    ],
    writerWrapText = WrapPreserve
  }

latexProcessor :: Processor
latexProcessor = Processor
  {
    processEquation = latexProcessEquation
  , processTheoremEnv = latexProcessTheoremEnv
  , processImage = latexProcessImage
  , processLink = latexProcessLink
  , writeText = latexWriteText
  }

latexProcessEquation :: MathType -> Text -> Inline
-- fall back to raw inline if the equation has any \begin{ - \end{ inside
latexProcessEquation DisplayMath txt | 
  T.isInfixOf "\\begin{" txt && T.isInfixOf "\\end{" txt = 
    RawInline "latex" txt
latexProcessEquation mt t = Math mt t

latexTheoremEnvTypeName :: TheoremEnvType -> Text
latexTheoremEnvTypeName t = 
  case t of
    Theorem -> "theorem"
    Lemma -> "lemma"
    Definition -> "definition"
    Corollary -> "corollary"
    Remark -> "remark"
    FigureEnv -> "figure"

latexTheoremEnvTypeHeader :: TheoremEnvType -> Text
latexTheoremEnvTypeHeader t = 
  case t of
    Theorem -> "thm:"
    Lemma -> "lem:"
    Definition -> "def:"
    Corollary -> "cor:"
    Remark -> "rem:"
    FigureEnv -> "fig:"

latexProcessTheoremEnv :: TheoremEnv -> [Block]
-- Handle figures separately
latexProcessTheoremEnv (TheoremEnv FigureEnv envTag envDesc) = 
  [Para $ [envStart] ++ inlines ++ [envEnd]] where
    envStartTex = "\\begin{figure}\n\\centering\n\\caption{"
    envStart = RawInline "tex" envStartTex

    -- Figure environment should have only one paragraph for description
    [Para inlines] = envDesc

    label = "}\\label{fig:" <> envTag <> "}\n"
    envEndTex = label <> "\\end{figure}"
    envEnd = RawInline "tex" envEndTex
latexProcessTheoremEnv TheoremEnv
  {
    theoremEnvType = envType
  , theoremEnvTag = envTag
  , theoremEnvDescription = envDesc
  } = [envStart] ++ envDesc ++ [envEnd] where
    envName = latexTheoremEnvTypeName envType
    envStart = Plain [RawInline "tex" $ "\\begin{" <> envName <> "}"]
    envLabel = "\\label{" <> latexTheoremEnvTypeHeader envType <> 
               envTag <> "}\n"
    envEndTex = envLabel <> "\\end{" <> envName <> "}"
    envEnd = Plain [RawInline "tex" envEndTex]

latexProcessImage :: Attr -> [Inline] -> Target -> Inline
latexProcessImage = Image

latexProcessLink :: Attr -> [Inline] -> Target -> Inline
-- For single-valued wikilinks, just make a smart link.
latexProcessLink _ [Str tag] (desc, wikilink) | tag == desc =
  RawInline "tex" "Theorem \\ref{thm:todo-make-it-work}"
latexProcessLink att inl tar = traceShow (att, inl, tar) 
  Str "TODO: make links work"

latexWriteText :: Pandoc -> PandocIO Text
latexWriteText = writeLaTeX options where
  options = def {
    writerExtensions = extensionsFromList [
      Ext_tex_math_dollars,
      Ext_raw_html,
      Ext_raw_tex,
      Ext_strikeout
    ],
    writerWrapText = WrapPreserve
  }
