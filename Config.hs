module Config (keyBindings)
    where

import qualified Data.Map as M
import           WXYZMonad

-----------------------------------------------------
-- User configuration

keyBindings :: M.Map (Modifier,KeySym) (WXYZMonad ())
keyBindings = M.fromList [ ((wlr_modifier_alt, xkb_key_q),      terminate)
                         , ((wlr_modifier_alt, xkb_key_t),      shell "alacritty")
                         , ((wlr_modifier_alt, xkb_key_h),      hello)
                         , ((wlr_modifier_alt, xkb_key_tab),    next_toplevel)
                         ]


