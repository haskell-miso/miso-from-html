-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import qualified Data.Text.IO as T
-----------------------------------------------------------------------------
import Miso.From.Html (processPretty)
-----------------------------------------------------------------------------
main :: IO ()
main = T.putStrLn =<< processPretty <$> T.getContents
-----------------------------------------------------------------------------
