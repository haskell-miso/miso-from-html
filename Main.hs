{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import           Control.Applicative

import           Data.Attoparsec.Text
import           Data.Char
import           Data.List            hiding (takeWhile)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Prelude              hiding (takeWhile)
import           System.Exit
import           Text.Pretty.Simple

data HTML
  = Branch TagName [ Attr ] [ HTML ]
  | Leaf Text
  deriving (Eq)

newtype CSS = CSS (Map Text Text)
  deriving (Eq)
  deriving newtype (Monoid, Semigroup)

instance Show CSS where
  show (CSS hmap) =
    mconcat
    [ "M.fromList [ "
    , intercalate "," (go <$> M.assocs hmap)
    , " ]"
    ]
    where
      go (k,v) = "(" <> "\"" <>
        T.unpack k <> "\" ," <> "\"" <>
          T.unpack v <> "\" )"

data Attr = Attr Text (Maybe Text)
  deriving (Eq)

instance Show HTML where
  show (Leaf x) = "\"" <> T.unpack x <> "\""
  show (Branch t as cs) =
    mconcat $
    [ T.unpack t
    , "_ "
    , show as
    ] ++ [ show cs | not (isEmpty t) ]

instance Show Attr where
  show (Attr "style" (Just v)) =
    mconcat
    [ "style_ $ "
    , T.unpack v
    ]
  show (Attr k (Just v))
    | T.any (=='-') k =
      mconcat
      [ "textProp \""
      , T.unpack k
      , "\""
      , " \""
      , T.unpack v
      , "\""
      ]
    | otherwise =
      mconcat
      [ T.unpack k
      , "_ "
      , "\""
      , T.unpack v
      , "\""
      ]
  show (Attr "checked" Nothing) =
    "checked_ True"
  show (Attr k Nothing) =
    mconcat
    [ "textProp \""
    , T.unpack k
    , "\" \"\""
    ]

type TagName = Text

tag :: Parser (TagName, [Attr])
tag = do
  _ <- char '<'
  t <- takeWhile1 isAlphaNum
  _ <- char '>'
  pure (t, [])

tagWithAttrs :: Parser (TagName, [Attr])
tagWithAttrs = do
  _ <- char '<'
  t <- takeWhile1 (/=' ')
  _ <- char ' '
  as <- attrs `sepBy` char ' '
  skipSpace
  _ <- char '/' <|> char '>'
  pure (t, as)

attrs :: Parser Attr
attrs = kvAttr <|> attr
  where
    predicate x = isAlpha x || x == '-'
    kvAttr  = Attr <$> key <*> do Just <$> value
    attr    = flip Attr Nothing <$> justKey
    justKey = takeWhile1 predicate
    key = do
      k <- takeWhile1 predicate
      _ <- char '='
      pure k
    value = do
      _ <- char '"'
      v <- takeWhile (/= '"')
      _ <- char '"'
      pure v

children :: Parser [HTML]
children = many htmlOrLeaf

htmlOrLeaf :: Parser HTML
htmlOrLeaf = html <|> leaf

leaf :: Parser HTML
leaf = Leaf <$> do
  strip . T.filter (/='\n') <$>
    takeWhile1 (/='<')
  where
    strip = T.reverse
          . T.dropWhile (==' ')
          . T.reverse
          . T.dropWhile (==' ')

dropFluff :: Parser ()
dropFluff = do
  _ <- takeWhile (`elem` ("\n " :: String))
  pure ()

html :: Parser HTML
html = do
  (openTag, as) <-
    tag <|> tagWithAttrs
  dropFluff
  cs <-
    if isEmpty openTag
      then pure []
      else do
        cs <- children
        closeTag
        pure cs
  dropFluff
  let hasStyle (Attr k _) = k == "style"
  pure $ case find hasStyle as of
    Just (Attr key (Just cssText)) -> do
      let parsedCss = T.pack $ show (parseCss cssText)
          newAttr = Attr key (Just parsedCss)
          oldAttrs = filter (not . hasStyle) as
      Branch openTag (newAttr : oldAttrs) cs
    _ ->
      Branch openTag as cs

parseCss :: Text -> CSS
parseCss cssText = CSS cssMap
  where
    cssMap
      = M.fromList
      [ (k,v)
      | [k,v] <- T.splitOn ":" <$> T.splitOn ";" cssText
      ]

isEmpty :: Text -> Bool
isEmpty =
  flip elem
  [ "area"
  , "base"
  , "br"
  , "col"
  , "embed"
  , "hr"
  , "img"
  , "input"
  , "link"
  , "meta"
  , "param"
  , "source"
  , "track"
  , "wbr"
  , "circle"
  , "path"
  ]

closeTag :: Parser ()
closeTag = do
  _ <- string "</"
  _ <- takeWhile1 isAlphaNum
  _ <- char '>'
  pure ()

main :: IO ()
main = do
  file <- stripDoctype . removeComments <$> T.getContents
  case parseOnly html file of
    Right r ->
      pPrint r
    Left e -> do
      print e
      exitFailure

-- | Layered lexer
data Mode
  = InComment
  | Normal
  deriving (Show, Eq)

stripDoctype :: Text -> Text
stripDoctype t = do
  let firstLine = T.takeWhile (/='\n') t
  if "<!doctype html>" `T.isPrefixOf` (T.toLower firstLine)
    then T.drop 1 (T.dropWhile (/='\n') t)
    else t

-- | Remove HTML comments using a layered lexer
--
-- @
-- > removeComments "<a><!-- hey --></a>"
-- > <a></a>
-- @
--
removeComments :: Text -> Text
removeComments = go Normal Nothing
  where
    go Normal Nothing txt =
      case T.uncons txt of
        Nothing ->
          mempty
        Just (c, next) ->
          T.singleton c <>
            go Normal (Just c) next
    go Normal (Just _) txt =
      case T.uncons txt of
        Nothing ->
          mempty
        Just (c,next) ->
          case T.uncons next of
            Just (c',next') ->
              if [c,c'] == "<!"
              then
                go InComment (Just c') next'
              else
                T.singleton c <>
                  go Normal (Just c) next
            Nothing ->
              T.singleton c <>
                go Normal (Just c) next
    go InComment Nothing txt =
      case T.uncons txt of
        Nothing ->
          error "Comment not terminated"
        Just (c,next) ->
          go InComment (Just c) next
    go InComment (Just prev) txt =
      case T.uncons txt of
        Nothing ->
          error "Comment not terminated"
        Just (c,next) ->
         if [prev,c] == "->"
           then go Normal (Just c) next
           else go InComment (Just c) next
