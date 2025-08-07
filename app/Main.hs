-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Prelude hiding (unlines, rem)
-----------------------------------------------------------------------------
import           Miso hiding (media_)
import           Miso.Lens
import           Miso.From.Html (process)
import           Miso.String
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
data Action = OnInput MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp app)
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
updateModel (OnInput input) = value .= input
-----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel (Model input) = div_ []
 [ h1_ []
    [ "🍜 miso-from-html"
    ]
  , h4_ []
    [ "Convert HTML to miso"
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
        [ "Input"
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
        [ "Output"
        ]
      , pre_
        [ id_ "output"
        , class_ "output-area"
        ]
        [ text $ ms (process (fromMisoString input))
        ]
      ]
  ]
]
-----------------------------------------------------------------------------
