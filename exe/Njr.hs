-- An example configuration for WXYZ Monad

module Main (main)
    where

import qualified Data.Map as M

import           Key
import           Operations
import           Tiling
import           WXYZMonad
import qualified StackSet as W

main :: IO ()
main = wxyz $
        Config { keyBindings
               , layoutHook = Layout tiled
               , workspaces = ["first", "second", "third"]
               }
  where
    keyBindings :: M.Map (Modifier,KeySym) (WXYZ ())
    keyBindings = M.fromList
        [ ((modMask, xkb_key_q),      terminate)
        , ((modMask, xkb_key_t),      shell "alacritty")
        , ((modMask, xkb_key_h),      hello)
        , ((modMask, xkb_key_tab),    next_toplevel)

        -- move focus up or down the window stack
        , ((modMask, xkb_key_j),      windows W.focusDown)      -- %! Move focus to the next window
        , ((modMask, xkb_key_k),      windows W.focusUp)        -- %! Move focus to the previous window
        , ((modMask, xkb_key_m),      windows W.focusMaster  )  -- %! Move focus to the master window
        ]
    modMask = wlr_modifier_alt
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes
