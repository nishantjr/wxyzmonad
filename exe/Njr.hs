-- An example configuration for WXYZ Monad

module Main (main)
    where

import qualified Data.Map as M
import           MainWXYZ (wxyz)
import           WXYZMonad

main :: IO ()
main = wxyz (Config keyBindings)
  where
    keyBindings :: M.Map (Modifier,KeySym) (WXYZ ())
    keyBindings = M.fromList [ ((wlr_modifier_alt, xkb_key_q),      terminate)
                             , ((wlr_modifier_alt, xkb_key_t),      shell "alacritty")
                             , ((wlr_modifier_alt, xkb_key_h),      hello)
                             , ((wlr_modifier_alt, xkb_key_tab),    next_toplevel)
                             ]

