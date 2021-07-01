{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module IHaskell.Display.Widgets.Output
  ( -- * The Output Widget
    OutputWidget
    -- * Constructor
  , mkOutputWidget
    -- * Using the output widget
  , printOutput
  , appendOutput
  , clearOutput
  , clearOutput_
  , replaceOutput
  ) where

-- To keep `cabal repl` happy when running from the ihaskell repo
import           Prelude

import           Data.Aeson
import           Data.Text
import           Data.IORef (newIORef)

import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Types
import           IHaskell.IPython.Message.UUID as U

import           IHaskell.Display.Widgets.Types

-- | An 'OutputWidget' represents a Output widget from IPython.html.widgets.
type OutputWidget = IPythonWidget 'OutputType

-- | Create a new output widget
mkOutputWidget :: IO OutputWidget
mkOutputWidget = do
  -- Default properties, with a random uuid
  wid <- U.random

  let widgetState = WidgetState defaultOutputWidget

  stateIO <- newIORef widgetState

  let widget = IPythonWidget wid stateIO

  -- Open a comm for this widget, and store it in the kernel state
  widgetSendOpen widget $ toJSON widgetState

  -- Return the image widget
  return widget

printOutput :: OutputWidget -> String -> IO ()
printOutput out val = do
  widgetSendStream out Stdout val

-- | Append to the output widget
appendOutput :: IHaskellDisplay a => OutputWidget -> a -> IO ()
appendOutput widget out = do
  disp <- display out
  widgetPublishDisplay widget disp
  widgetSendView widget

-- | Clear the output widget immediately
clearOutput :: OutputWidget -> IO ()
clearOutput widget = widgetClearOutput widget False

-- | Clear the output widget on next append
clearOutput_ :: OutputWidget -> IO ()
clearOutput_ widget = widgetClearOutput widget True

-- | Replace the currently displayed output for output widget
replaceOutput :: IHaskellDisplay a => OutputWidget -> a -> IO ()
replaceOutput widget d = do
  clearOutput_ widget
  appendOutput widget d

instance IHaskellWidget OutputWidget where
  getCommUUID = uuid
