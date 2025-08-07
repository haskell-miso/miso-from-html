-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Prelude hiding (unlines, rem)
-----------------------------------------------------------------------------
import           Miso hiding (media_)
import           Miso.Media
import           Miso.Lens
import           Miso.From.Html (process)
import           Miso.String
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
import           Ormolu (ormolu)
import           Ormolu.Config
-----------------------------------------------------------------------------
#ifndef WASM
import           Data.FileEmbed
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
newtype Model = Model { _value :: MisoString }
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model v) = toMisoString v
-----------------------------------------------------------------------------
value :: Lens Model MisoString
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
data Action
  = OnInput MisoString
  | CopyToClipboard
  | Copied
  | ErrorCopy JSVal
  | SetText MisoString
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp app)
-----------------------------------------------------------------------------
formatString :: MisoString -> IO MisoString
formatString = fmap toMisoString . ormolu cfg "<input>" . fromMisoString
  where
    cfg =
      defaultConfig
      { cfgPrinterOpts =
          defaultPrinterOpts
          { poColumnLimit = pure (ColumnLimit 50)
          }
      }

-----------------------------------------------------------------------------
app :: App Model Action
app = (component (Model mempty) updateModel viewModel)
#ifndef WASM
  { styles =
      [ Style $ ms $(embedFile "static/style.css")
      ]
  }
#endif
-----------------------------------------------------------------------------
updateModel :: Action -> Transition Model Action
updateModel (OnInput input) = do
  let output = ms (process (fromMisoString input))
  io (SetText <$> liftIO (formatString output))
updateModel CopyToClipboard = do
  input <- use value
  let output = ms (process (fromMisoString input))
  copyClipboard output Copied ErrorCopy
updateModel (ErrorCopy jsval) =
  io_ (consoleLog' jsval)
updateModel Copied =
  io_ (consoleLog "copied")
updateModel (SetText txt) =
  value .= txt
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel (Model input) =
  div_
  []
  [ div_
    [ CSS.style_ [ CSS.textAlign "center" ]
    ]
    [ h1_ []
      [ "🍜 miso-from-html"
      ]
    , h4_ []
      [ "Convert HTML to miso"
      ]
    , button_
      [ onClick CopyToClipboard
      , CSS.style_
        [ CSS.width "180px"
        , CSS.height "60px"
        , CSS.margin "5px"
        ]
      ]
      [ "Copy to Clipboard"
      ]
    ]
    , div_
      [ className "container"
      ]
      [ div_
        [ class_ "panel"
        ]
        [ div_
          [ class_ "panel-header"
          ]
          [ "HTML Input"
          ]
        , textarea_
          [ placeholder_ "Type your text here..."
          , class_ "input-area"
          , onInput OnInput
          ]
          []
        ]
      , div_
        [ class_ "panel" ]
        [ div_
          [ class_ "panel-header" ]
          [ "miso View output"
          ]
        , pre_
          [ id_ "output"
          , class_ "output-area"
          ]
          [ text input
          ]
        ]
      ]
   ]
-----------------------------------------------------------------------------
