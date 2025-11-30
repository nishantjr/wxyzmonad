module Key
    ( KeySym
    , KeyCode
    , Modifier
    , xkb_key_d
    , xkb_key_h
    , xkb_key_q
    , xkb_key_t
    , xkb_key_tab
    , wlr_modifier_alt
    ) where

import           Data.Word

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


