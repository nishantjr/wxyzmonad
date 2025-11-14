{-# LANGUAGE CApiFFI #-}

module Lib (terminate, next_toplevel, shell) where

import WXYZMonad

import Data.Word
import Foreign.C.Types
import qualified Data.Map as M
import qualified System.Process as P

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_keyboard.h>

type Modifier = Word32
wlr_modifier_alt :: Modifier
wlr_modifier_alt = #const WLR_MODIFIER_ALT

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

-- Operations that a user's configuration may perform
-----------------------------------------------------

foreign import capi "lib.h wxyz_terminate"
    terminate :: WXYZMonad ()
foreign import capi "lib.h wxyz_next_toplevel"
    next_toplevel :: WXYZMonad ()

shell :: String -> WXYZMonad ()
shell cmd = do _ <- P.createProcess $ P.shell cmd
               pure ()

hello :: WXYZMonad ()
hello = putStr "====================\nHello!\n============================\n"

-----------------------------------------------------

handle_keysym :: Modifier -> KeySym -> WXYZMonad CBool
handle_keysym mod sym = case M.lookup (mod, sym) keyBindings
                          of Nothing     -> pure $ CBool 0
                             Just action -> do action 
                                               pure $ CBool 1

foreign export ccall handle_keysym :: Modifier -> KeySym -> WXYZMonad CBool

-----------------------------------------------------
-- User configuration

keyBindings :: M.Map (Modifier,KeySym) (WXYZMonad ())
keyBindings = M.fromList [ ((wlr_modifier_alt, xkb_key_q),      terminate)
                         , ((wlr_modifier_alt, xkb_key_t),      shell "alacritty")
                         , ((wlr_modifier_alt, xkb_key_h),      hello)
                         , ((wlr_modifier_alt, xkb_key_tab),    next_toplevel)
                         ]

