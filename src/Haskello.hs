{-# LANGUAGE OverloadedStrings #-}

module Haskello where

import Brick.AttrMap (attrMap)
import Brick.Main (App(..), appAttrMap, customMain, showFirstCursor)
import Brick.Util (fg)
import Control.Monad (void)
import Event (handleUIEventPostProcessed)
import Graphics.Vty (mkVty)
import Graphics.Vty.Attributes.Color (cyan)
import Graphics.Vty.Config (defaultConfig)
import State.AppState (buildInitialState)
import State.Types (AppState, ResourceName)
import UI (drawScreen)

haskello :: IO ()
haskello = do
  initialState <- buildInitialState
  let config = mkVty defaultConfig
  handle <- config
  void $ customMain handle config Nothing haskelloApp initialState

haskelloApp :: App AppState e ResourceName
haskelloApp =
  App
    { appDraw = drawScreen
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleUIEventPostProcessed
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("active", fg cyan)]
    }
