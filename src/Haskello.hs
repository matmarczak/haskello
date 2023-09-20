{-# LANGUAGE OverloadedStrings #-}

module Haskello where

import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan)
import Brick.Main (App(..), appAttrMap, customMain, showFirstCursor)
import Brick.Util (fg)
import Control.Monad (void)
import Event (handleUIEventPostProcessed)
import Graphics.Vty (mkVty)
import Graphics.Vty.Attributes.Color (cyan)
import Graphics.Vty.Config (defaultConfig)
import State.AppState (buildInitialState)
import State.Types (AppState, HaskelloEvent, ResourceName)
import UI (drawScreen)

haskello :: IO ()
haskello = do
  let config = mkVty defaultConfig
  handle <- config
  eventChan <- newBChan 10
  initialState <- buildInitialState eventChan
  void $ customMain handle config (Just eventChan) haskelloApp initialState

haskelloApp :: App AppState HaskelloEvent ResourceName
haskelloApp =
  App
    { appDraw = drawScreen
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleUIEventPostProcessed
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("active", fg cyan)]
    }
