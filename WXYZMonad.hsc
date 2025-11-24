{-# LANGUAGE CApiFFI #-}

module WXYZMonad
    ( KeySym(..)
    , KeyCode(..)
    , Modifier(..)
    , WXYZMonad(..)
    , hello
    , next_toplevel
    , shell
    , terminate
    , xkb_key_d
    , xkb_key_h
    , xkb_key_q
    , xkb_key_t
    , xkb_key_tab
    , wlr_modifier_alt
    ) where

import           Control.Monad
import           Data.Word
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr

import qualified System.Process as P

#include "clib.h"

type Modifier = Word32
wlr_modifier_alt :: Modifier
wlr_modifier_alt = #const WLR_MODIFIER_ALT

type KeyCode = Word32
type KeySym = Word32

xkb_key_d :: Modifier
xkb_key_d = #const XKB_KEY_d
xkb_key_h :: Modifier
xkb_key_h = #const XKB_KEY_h
xkb_key_q :: Modifier
xkb_key_q = #const XKB_KEY_q
xkb_key_t :: Modifier
xkb_key_t = #const XKB_KEY_t
xkb_key_tab :: Modifier
xkb_key_tab = #const XKB_KEY_Tab

-----------------------------------------------------
-- TODO: For now, we are just a wrapper around IO.
-- State variables are store in global by the C side.
type WXYZMonad = IO

-- Operations that a user's configuration may perform
-----------------------------------------------------

foreign import capi "clib.h wxyz_terminate"
    terminate :: WXYZMonad ()
foreign import capi "clib.h wxyz_next_toplevel"
    next_toplevel :: WXYZMonad ()

shell :: String -> WXYZMonad ()
shell cmd = do _ <- P.createProcess $ P.shell cmd
               pure ()

hello :: WXYZMonad ()
hello = putStr "====================\nHello!\n============================\n"

