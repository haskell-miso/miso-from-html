-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
module Miso.From.Html where
-----------------------------------------------------------------------------
import Data.Char
import           Control.Monad (guard)
import           Control.Monad.State
import           Control.Applicative
import           Data.List hiding (takeWhile)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import           Prelude hiding (takeWhile)
import           Text.HTML.Parser
import           Text.Pretty.Simple
import           Text.HTML.Tree
-----------------------------------------------------------------------------
type IsOpen = Bool
-----------------------------------------------------------------------------
data HTML
  = Node IsOpen HTMLTagName [ HTMLAttr ] [ HTML ]
  | TextNode Text
  deriving stock (Eq)
-----------------------------------------------------------------------------
newtype CSS = CSS (Map Text Text)
  deriving stock (Eq)
  deriving newtype (Monoid, Semigroup)
-----------------------------------------------------------------------------
instance Show CSS where
  show (CSS hmap) =
    mconcat
    [ "CSS.style [ "
    , intercalate "," (go <$> M.assocs hmap)
    , " ]"
    ]
    where
      go (k,v) = "\"" <>
        T.unpack (T.strip k) <> "\" =: " <> "\"" <>
          T.unpack (T.strip v) <> "\""
-----------------------------------------------------------------------------
data HTMLAttr = HTMLAttr Text (Maybe Text)
  deriving (Eq)
-----------------------------------------------------------------------------
instance Show HTML where
  show (TextNode x) = "\"" <> T.unpack x <> "\""
  show (Node isOpen t as cs) =
    mconcat $
    [ T.unpack t
    , "_ "
    , show as
    ] ++
    [ show cs
    | isOpen
    ]
-----------------------------------------------------------------------------
upper :: Text -> Text
upper (T.uncons -> Just (h,xs)) = T.cons (toUpper h) xs
upper x = x
-----------------------------------------------------------------------------
instance Show HTMLAttr where
  show (HTMLAttr "style" (Just v)) =
    mconcat
    [ T.unpack v
    ]
  show (HTMLAttr k (Just v))
    | "stroke-" `T.isPrefixOf` k
    , Just rest <- T.stripPrefix "stroke-" k =
      mconcat
      [ " stroke" <> T.unpack (upper rest) <> "_"
      , " \""
      , T.unpack v
      , "\" "
      ]
    | "data-" `T.isPrefixOf` k
    , Just rest <- T.stripPrefix "data-" k =
      mconcat
      [ " data_ \""
      , T.unpack rest
      , "\""
      , " \""
      , T.unpack v
      , "\" "
      ]
    | "aria-" `T.isPrefixOf` k
    , Just rest <- T.stripPrefix "aria-" k =
      mconcat
      [ " aria_ \""
      , T.unpack rest
      , "\""
      , " \""
      , T.unpack v
      , "\" "
      ]
    | T.any (=='-') k =
      mconcat
      [ " textProp \""
      , T.unpack k
      , "\""
      , " \""
      , T.unpack v
      , "\" "
      ]
    | otherwise =
      mconcat
      [ " "
      , T.unpack k
      , "_ "
      , "\""
      , T.unpack v
      , "\" "
      ]
  show (HTMLAttr x@(T.isPrefixOf "data-" -> True) Nothing) =
    case T.stripPrefix "data-" x of
      Just rest -> "data_ " <> "\"" <> T.unpack rest <> "\"" <> " \"\""
      Nothing -> T.unpack x
  show (HTMLAttr "checked" Nothing) =
    "checked_ True"
  show (HTMLAttr k Nothing) =
    mconcat
    [ "textProp \""
    , T.unpack k
    , "\" \"\""
    ]
-----------------------------------------------------------------------------
type HTMLTagName = Text
-----------------------------------------------------------------------------
html :: Parser HTML
html = withoutKids <|> withKids
  where
    withoutKids =
      textNode <|> tagSelfClose
    withKids = do
      (openName, attrs) <- tagOpen
      kids <- many html
      closeName <- tagClose
      guard (openName == closeName)
      pure (Node True openName attrs kids)
-----------------------------------------------------------------------------
tagOpen :: Parser (TagName, [HTMLAttr])
tagOpen = do
  TagOpen openName attrs <-
    satisfy $ \case
      TagOpen{} -> True
      _ -> False
  let htmlAttrs =
        [ processStyle (HTMLAttr attrName value)
        | Attr attrName attrValue <- attrs
        , let value
                | T.null attrValue = Nothing
                | otherwise = Just attrValue
        ]
  pure (openName, htmlAttrs)
-----------------------------------------------------------------------------
tagClose :: Parser TagName
tagClose = do
  TagClose closeName <-
    satisfy $ \case
      TagClose{} -> True
      _ -> False
  pure closeName
-----------------------------------------------------------------------------
tagSelfClose :: Parser HTML
tagSelfClose = do
  TagSelfClose name attrs <-
    satisfy $ \case
      TagSelfClose {} -> True
      _ -> False
  let htmlAttrs =
        [ processStyle (HTMLAttr attrName value)
        | Attr attrName attrValue <- attrs
        , let value
                | T.null attrValue = Nothing
                | otherwise = Just attrValue
        ]
  pure (Node False name htmlAttrs [])
-----------------------------------------------------------------------------
textNode :: Parser HTML
textNode = leaf <|> leafChar
  where
    leaf :: Parser HTML
    leaf = do
      ContentText txt <-
        satisfy $ \case
          ContentText {} -> True
          _ -> False
      pure (TextNode txt)

    leafChar :: Parser HTML
    leafChar = do
      ContentChar t <-
        satisfy $ \case
          ContentChar {} -> True
          _ -> False
      pure (TextNode (T.singleton t))
------------------------------------------------------------------------------
processStyle :: HTMLAttr -> HTMLAttr
processStyle (HTMLAttr "style" (Just cssText)) =
  HTMLAttr "style" $ Just (T.pack (show parsedCss))
    where
      parsedCss :: CSS
      parsedCss = CSS cssMap
        where
          cssMap
            = M.fromList
            [ (k,v)
            | [k,v] <- T.splitOn ":" <$> T.splitOn ";" cssText
            ]
processStyle attr = attr
------------------------------------------------------------------------------
isComment :: Token -> Bool
isComment Comment {} = True
isComment _ = False
-----------------------------------------------------------------------------
isDoctype :: Token -> Bool
isDoctype Doctype {} = True
isDoctype _ = False
-----------------------------------------------------------------------------
isEmptyTextNode :: Token -> Bool
isEmptyTextNode (ContentText txt)
  = T.null
  $ T.filter (`notElem` ['\n', '\t', ' '])
  $ txt
isEmptyTextNode _ = False
-----------------------------------------------------------------------------
getTokens :: Text -> [Token]
getTokens input = preprocess $
  let
    tokens = parseTokens input
  in
    [ case t of
        ContentText txt ->
          ContentText (T.strip txt)
        _ -> t
    | t <- tokens
    , not (isComment t)
      && not (isDoctype t)
      && not (isEmptyTextNode t)
    ]
-----------------------------------------------------------------------------
process :: Text -> Text
process input =
  case parse html (getTokens input) of
    Right r ->
      T.pack (show r)
    Left e ->
      T.pack (show e)
-----------------------------------------------------------------------------
preprocess :: [Token] -> [Token]
preprocess = fmap go
  where
    go (TagOpen name attrs)
      | name `elem` nonClosing = TagSelfClose name attrs
      | otherwise = TagOpen name attrs
    go x = x
-----------------------------------------------------------------------------
processPretty :: Text -> Text
processPretty input =
  case parse html (getTokens input) of
    Right r ->
      LT.toStrict (pShow r)
    Left e ->
      LT.toStrict (pShow e)
-----------------------------------------------------------------------------
data ParseError a
  = UnexpectedParse [Token]
  | Ambiguous [(a, [Token])]
  | NoParses Token
  | EmptyStream
  deriving (Show, Eq)
-----------------------------------------------------------------------------
parse :: Parser a -> [Token] -> Either (ParseError a) a
parse _ []          = Left EmptyStream
parse parser tokens =
  case runStateT parser tokens of
    []        -> Left (NoParses (last tokens))
    [(x, [])] -> Right x
    [(_, xs)] -> Left (UnexpectedParse xs)
    xs        -> Left (Ambiguous xs)
-----------------------------------------------------------------------------
type Parser a = StateT [Token] [] a
-----------------------------------------------------------------------------
satisfy :: (Token -> Bool) -> Parser Token
satisfy f = StateT $ \tokens ->
  case tokens of
    t : ts | f t -> [(t, ts)]
    _ -> []
-----------------------------------------------------------------------------
