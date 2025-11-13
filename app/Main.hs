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
import           Miso
import           Miso.Html hiding (title_)
import           Miso.Html.Property
import           Miso.Navigator
import           Miso.Lens hiding (set)
import           Miso.From.Html (process)
import           Miso.String
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
import           Ormolu (ormolu)
import           Ormolu.Config
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Mode
  = Editing
  | Clear
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model
  = Model
  { _value :: MisoString
  , _mode :: Mode
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToMisoString Model where
  toMisoString (Model v _) = toMisoString v
-----------------------------------------------------------------------------
value :: Lens Model MisoString
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
mode :: Lens Model Mode
mode = lens _mode $ \m v -> m { _mode = v }
-----------------------------------------------------------------------------
data Action
  = OnInput MisoString
  | CopyToClipboard
  | Copied
  | ErrorCopy JSVal
  | SetText MisoString
  | ClearText
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
app = (component (Model mempty Clear) updateModel viewModel)
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
  mode .= Editing
  let output = ms (process (fromMisoString input))
  io (SetText <$> liftIO (formatString output))
updateModel CopyToClipboard = do
  input <- use value
  copyClipboard input Copied ErrorCopy
updateModel (ErrorCopy err) =
  io_ (consoleLog' err)
updateModel Copied =
  io_ (showToast "Copied to clipboard...")
updateModel (SetText txt) =
  value .= txt
updateModel ClearText = do
  mode .= Clear
  value .= mempty
-----------------------------------------------------------------------------
githubStar :: View model action
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
viewModel (Model input mode_) =
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
      [ onClick ClearText
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
        , optionalAttrs
          textarea_
          [ placeholder_ "Type your text here..."
          , class_ "input-area"
          , onInput OnInput
          ] (mode_ == Clear)
          [ value_ ""
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
showToast :: MisoString -> JSM ()
showToast msg = do
  o <- create
  set @MisoString "text" msg o
  set @Int "duration" 3000 o
  toastify <- new (jsg @MisoString "Toastify") [o]
  void $ toastify # ("showToast" :: MisoString) $ ()
-----------------------------------------------------------------------------
