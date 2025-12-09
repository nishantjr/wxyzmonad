module Key
    ( KeySym
    , KeyCode
    , Modifier

    , xkb_key_d
    , xkb_key_h
    , xkb_key_j
    , xkb_key_k
    , xkb_key_l
    , xkb_key_m
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

xkb_key_d :: KeySym
xkb_key_d = #const XKB_KEY_d
xkb_key_h :: KeySym
xkb_key_h = #const XKB_KEY_h
xkb_key_j :: KeySym
xkb_key_j = #const XKB_KEY_j
xkb_key_k :: KeySym
xkb_key_k = #const XKB_KEY_k
xkb_key_l :: KeySym
xkb_key_l = #const XKB_KEY_l
xkb_key_m :: KeySym
xkb_key_m = #const XKB_KEY_m
xkb_key_q :: KeySym
xkb_key_q = #const XKB_KEY_q
xkb_key_t :: KeySym
xkb_key_t = #const XKB_KEY_t

xkb_key_tab :: KeySym
xkb_key_tab = #const XKB_KEY_Tab


