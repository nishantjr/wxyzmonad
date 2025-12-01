-- An example configuration for WXYZ Monad

module Main (main)
    where

import qualified Data.Map as M

import           Key
import           Operations
import           Tiling
import           WXYZMonad

main :: IO ()
main = wxyz $
        Config { keyBindings
               , layoutHook = Layout tiled
               , workspaces = ["first", "second", "third"]
               }
  where
    keyBindings :: M.Map (Modifier,KeySym) (WXYZ ())
    keyBindings = M.fromList [ ((wlr_modifier_alt, xkb_key_q),      terminate)
                             , ((wlr_modifier_alt, xkb_key_t),      shell "alacritty")
                             , ((wlr_modifier_alt, xkb_key_h),      hello)
                             , ((wlr_modifier_alt, xkb_key_tab),    next_toplevel)
                             ]
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
