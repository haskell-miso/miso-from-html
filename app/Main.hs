-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Prelude hiding (unlines, rem)
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso hiding (media_)
import           Miso.Media
import           Miso.Lens hiding (set)
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
      [ Href "assets/style.css"
      , Href "https://cdn.jsdelivr.net/npm/toastify-js/src/toastify.min.css" 
      ]
  , scripts =
      [ Src "https://cdn.jsdelivr.net/npm/toastify-js"
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
  copyClipboard input Copied ErrorCopy
updateModel (ErrorCopy err) =
  io_ (consoleLog' err)
updateModel Copied =
  io_ showToast
updateModel (SetText txt) =
  value .= txt
-----------------------------------------------------------------------------
githubStar :: View parent action
githubStar = iframe_
    [ title_ "GitHub"
    , height_ "30"
    , width_ "170"
    , textProp "scrolling" "0"
    , textProp "frameborder" "0"
    , src_
      "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=miso-from-html&type=star&count=true&size=large"
    ]
    []
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel (Model input) =
  div_
  []
  [ githubStar
  , div_
    [ CSS.style_ [ CSS.textAlign "center" ]
    ]
    [ h1_ []
      [ "🍜 miso-from-html"
      ]
    , h4_ []
      [ "Convert HTML to miso"
      ]
    , button_
      [ onClick (SetText mempty)
      , CSS.style_
        [ CSS.width "120px"
        , CSS.height "50px"
        , CSS.fontSize "20px"
        ]
      ]
      [ "Clear"
      ]
    , button_
      [ onClick CopyToClipboard
      , CSS.style_
        [ CSS.width "120px"
        , CSS.height "50px"
        , CSS.margin "5px"
        , CSS.fontSize "20px"
        ]
      ]
      [ "Copy"
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
showToast :: JSM ()
showToast = do
  o <- create
  set @MisoString "text" "Copied to clipboard..." o
  set @Int "duration" 3000 o
  toastify <- new (jsg @MisoString "Toastify") [o]
  void $ toastify # ("showToast" :: MisoString) $ ()
-----------------------------------------------------------------------------
