{-# LANGUAGE CApiFFI #-}

module Lib () where

import           Config (keyBindings)
import           WXYZMonad

import qualified Data.Map as M
import           Foreign.C.Types

handle_keysym :: Modifier -> KeySym -> WXYZMonad CBool
handle_keysym mod sym = case M.lookup (mod, sym) keyBindings
                          of Nothing     -> pure $ CBool 0
                             Just action -> do action 
                                               pure $ CBool 1

foreign export ccall handle_keysym :: Modifier -> KeySym -> WXYZMonad CBool

