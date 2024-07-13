{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.List (unfoldr)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Data.EnumSet as ES
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer
import GHC.LanguageExtensions
import qualified GHC.Parser.Lexer as L
import GHC.Types.SrcLoc
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Error (emptyDiagOpts)
import GHC.Utils.Outputable (defaultSDocContext)

main :: IO ()
main = do
  print $ tokenizeHaskell "foo = bar"

----------------------------------------------------------------------------
-- Data types

-- | Token types that are used as tags to mark spans of source code.
data Token
  = -- | Keyword
    KeywordTok
  | -- | Pragmas
    PragmaTok
  | -- | Symbols (punctuation that is not an operator)
    SymbolTok
  | -- | Variable name (term level)
    VariableTok
  | -- | Data\/type constructor
    ConstructorTok
  | -- | Operator
    OperatorTok
  | -- | Character
    CharTok
  | -- | String
    StringTok
  | -- | Integer
    IntegerTok
  | -- | Rational number
    RationalTok
  | -- | Comment (including Haddocks)
    CommentTok
  | -- | Space filling
    SpaceTok
  | -- | Something else?
    OtherTok
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | The start and end positions of a span. The arguments of the data
-- constructor contain in order:
--
--     * Line number of start position of a span
--     * Column number of start position of a span
--     * Line number of end position of a span
--     * Column number of end position of a span
--
-- @since 0.0.2.0
data Loc = Loc !Int !Int !Int !Int
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------
-- High-level API

-- | Tokenize Haskell source code. If the code cannot be parsed, return
-- 'Nothing'. Otherwise return the original input tagged by 'Token's.
-- 'Nothing' is rarely returned, if ever, because it looks like the lexer is
-- capable of interpreting almost any text as a stream of GHC tokens.
--
-- The parser does not require the input source code to form a valid Haskell
-- program, so as long as the lexer can decompose your input (most of the
-- time), it'll return something in 'Just'.
tokenizeHaskell :: Text -> Maybe [(Token, Text)]
tokenizeHaskell input = sliceInputStream input <$> tokenizeHaskellLoc input

-- | Replace 'Loc' locations with actual chunks of input 'Text'.
sliceInputStream :: Text -> [(Token, Loc)] -> [(Token, Text)]
sliceInputStream input toks = unfoldr sliceOnce (initText' input, toks)
  where
    sliceOnce (txt, []) = do
      (txt', chunk) <- tryFetchRest txt
      return ((SpaceTok, chunk), (txt', []))
    sliceOnce (txt, tss@((t, l) : ts)) =
      case tryFetchSpace txt l of
        Nothing ->
          let (txt', chunk) = fetchSpan txt l
              t' = case t of
                CommentTok ->
                  if isHeaderPragma chunk
                    then PragmaTok
                    else CommentTok
                tok -> tok
           in Just ((t', chunk), (txt', ts))
        Just (txt', chunk) ->
          Just ((SpaceTok, chunk), (txt', tss))

-- | Similar to 'tokenizeHaskell', but instead of 'Text' chunks provides
-- locations of corresponding spans in the given input stream.
--
-- @since 0.0.2.0
tokenizeHaskellLoc :: Text -> Maybe [(Token, Loc)]
tokenizeHaskellLoc input =
  case L.unP (pure []) parseState of
    L.PFailed {} -> Nothing
    L.POk _ x -> Just x
  where
    location = mkRealSrcLoc (mkFastString "") 1 1
    buffer = stringToStringBuffer (T.unpack input)
    parseState = L.initParserState parserOpts buffer location
    parserOpts =
      L.mkParserOpts
        mempty
        emptyDiagOpts
        []
        True -- safe imports
        True -- keep Haddock tokens
        True -- keep comment tokens
        False -- lex LINE and COLUMN pragmas

-- | The Haskell lexer.
pLexer :: L.P [(Token, Loc)]
pLexer = go
  where
    go = do
      r <- L.lexer False return
      case r of
        L _ L.ITeof -> return []
        _ ->
          case fixupToken r of
            Nothing -> go
            Just x -> (x :) <$> go

-- | Convert @'Located' 'L.Token'@ representation into a more convenient for
-- us form.
fixupToken :: Located L.Token -> Maybe (Token, Loc)
fixupToken (L srcSpan tok) = (undefined tok,) <$> srcSpanToLoc srcSpan

-- | Convert 'SrcSpan' into 'Loc'.
srcSpanToLoc :: SrcSpan -> Maybe Loc
srcSpanToLoc (RealSrcSpan s _) =
  let srcSpanSLine = srcSpanStartLine s
      srcSpanSCol = srcSpanStartCol s
      srcSpanELine = srcSpanEndLine s
      srcSpanECol = srcSpanEndCol s
      start = (srcSpanSLine, srcSpanSCol)
      end = (srcSpanELine, srcSpanECol)
   in if start == end
        then Nothing -- NOTE Some magic auto-generated tokens that do not
        -- actually appear in the input stream. Drop them.
        else
          Just $
            Loc
              srcSpanSLine
              srcSpanSCol
              srcSpanELine
              srcSpanECol
srcSpanToLoc _ = Nothing

----------------------------------------------------------------------------
-- Text traversing

-- | A type for 'Text' with line\/column location attached.
data Text'
  = Text'
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Text
  deriving (Show)

-- | Create 'Text'' from 'Text'.
initText' :: Text -> Text'
initText' = Text' 1 1

-- | Try to fetch white space before start of span at 'Loc'.
tryFetchSpace :: Text' -> Loc -> Maybe (Text', Text)
tryFetchSpace txt (Loc sl sc _ _) =
  let (txt', r) = reachLoc txt sl sc
   in if T.null r
        then Nothing
        else Just (txt', r)

-- | Try to fetch the rest of 'Text'' stream.
tryFetchRest :: Text' -> Maybe (Text', Text)
tryFetchRest (Text' l c txt) =
  if T.null txt
    then Nothing
    else Just (Text' l c "", txt)

-- | Fetch a span at 'Loc'.
fetchSpan :: Text' -> Loc -> (Text', Text)
fetchSpan txt (Loc _ _ el ec) = reachLoc txt el ec

-- | Reach given line\/column location and return 'Text' that has been
-- traversed.
reachLoc ::
  Text' ->
  -- | Line number to reach
  Int ->
  -- | Column number to reach
  Int ->
  (Text', Text)
reachLoc txt@(Text' _ _ original) l c =
  let chunk = T.unfoldr f txt
      f (Text' l' c' s) = do
        guard (l' < l || c' < c)
        (ch, s') <- T.uncons s
        let (l'', c'') = case ch of
              '\n' -> (l' + 1, 1)
              '\t' -> (l', c' + 8 - ((c' - 1) `rem` 8))
              _ -> (l', c' + 1)
        return (ch, Text' l'' c'' s')
   in (Text' l c (T.drop (T.length chunk) original), chunk)

----------------------------------------------------------------------------
-- Pragmas detection

-- | Detect file header pragma.
isHeaderPragma :: Text -> Bool
isHeaderPragma txt0 = isJust $ do
  txt1 <- T.stripStart <$> T.stripPrefix "{-#" txt0
  guard (T.isPrefixOf "LANGUAGE" txt1 || T.isPrefixOf "OPTIONS_GHC" txt1)
