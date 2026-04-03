-- An example configuration for WXYZ Monad

module Main (main)
    where

import qualified Data.Map as M
import           Data.Bits ((.|.))

import           Key
import           Operations
import           Tiling
import           WXYZMonad
import           WXYZMain
import qualified StackSet as W
import           Layout.PerLayer

main :: IO ()
main = wxyz $
        Config { keyBindings
               , startupHook =
                    shell "swaybg -i /home/njr/.config/sway/background.jpg -m fill"
               , layoutHook = Layout $
                   PerLayer {
                        background = Full,       -- swaybg
                        bottom     = Full,       -- ?? Panels, launchers?
                        toplevels  = tiled,      -- Ordinary applications
                        top        = Full,       -- Waybar?
                        overlay    = Full        -- Notifications?
                   }
               , workspaces = workspaces
               }
  where
    keyBindings :: M.Map (Modifier,KeySym) (WXYZ ())
    keyBindings = M.fromList $
        [ ((modMask, xkb_key_q),      terminate)
        , ((modMask, xkb_key_t),      shell "alacritty")
        , ((modMask, xkb_key_d),      shell "bemenu-run")
        , ((modMask, xkb_key_h),      hello)

        -- move focus up or down the window stack
        , ((modMask, xkb_key_h),      sendMessage Shrink)       -- %! Shrink the master area
        , ((modMask, xkb_key_j),      windows W.focusDown)      -- %! Move focus to the next window
        , ((modMask, xkb_key_k),      windows W.focusUp)        -- %! Move focus to the previous window
        , ((modMask, xkb_key_l),      sendMessage Expand)       -- %! Expand the master area

        , ((modMask, xkb_key_m),      windows W.focusMaster  )  -- %! Move focus to the master window
        ] ++
        [((m, k), windows $ f i)
            | (i, k) <- zip workspaces [xkb_key_1 .. xkb_key_9]
            , (f, m) <- [ (W.greedyView, modMask),        -- Change to Workspace
                          (W.shift,      modMask .|. shiftMask) -- Move window to workspace
                        ]
        ]
    workspaces  = ["first", "second", "third"]
    modMask     = wlr_modifier_logo
    shiftMask   = wlr_modifier_shift
    tiled       = Tall nmaster delta ratio
    nmaster     = 1      -- Default number of windows in the master pane
    ratio       = 1/2    -- Default proportion of screen occupied by master pane
    delta       = 3/100  -- Percent of screen to increment by when resizing panes
