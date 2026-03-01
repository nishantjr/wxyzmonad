module Layout.PerLayer (PerLayer(..)
  ) where

import           WXYZMonad
import qualified StackSet as W
import           Event (Layer(..), layerSurfaceGetLayer)

import           Control.Monad (join)
import           Data.Maybe

data PerLayer bg bot tl top overlay a
    = PerLayer { background :: bg a         -- swaybg
               , bottom     :: bot a        -- ?? Panels, launchers?
               , toplevels  :: tl a         -- Ordinary applications
               , top        :: top a        -- Waybar?
               , overlay    :: overlay a    -- Notifications?
               }
  deriving (Show, Read)

instance ( LayoutClass bg Window
         , LayoutClass bot Window
         , LayoutClass tl Window
         , LayoutClass top Window
         , LayoutClass overlay Window
         )
  => LayoutClass (PerLayer bg bot tl top overlay) Window where

    doLayout l r s = do
        (bgRects, bgLayout) <-  (bgStack s) >>= (doLayoutOrEmpty (background l) r)
        (botRects, botLayout) <-  (botStack s) >>= (doLayoutOrEmpty (bottom l) r)
        (toplevelsRects, toplevelsLayout) <-  (toplevelsStack s) >>= (doLayoutOrEmpty (toplevels l) r)
        (topRects, topLayout) <-  (topStack s) >>= (doLayoutOrEmpty (top l) r)
        (overlayRects, overlayLayout) <-  (overlayStack s) >>= (doLayoutOrEmpty (overlay l) r)
        let newLayout = PerLayer { background = bgLayout `orElse` (background l)
                                 , bottom = botLayout `orElse` (bottom l)
                                 , toplevels = toplevelsLayout `orElse` (toplevels l)
                                 , top = topLayout `orElse` (top l)
                                 , overlay = overlayLayout `orElse` (overlay l)
                                 }
        pure (bgRects ++ botRects ++ toplevelsRects ++ topRects ++ overlayRects, Just newLayout)

      where
        doLayoutOrEmpty layout rect stack =
            maybe (emptyLayout layout rect)
                  (doLayout layout rect)
                  stack

        -- filter stack to appropriate layer
        bgStack :: W.Stack Window -> WXYZ (Maybe (W.Stack Window))
        bgStack stack = W.filterM (inLayer Background) stack
        botStack stack = W.filterM (inLayer Bottom) stack
        toplevelsStack stack = W.filterM isTopLevel stack
        topStack stack = W.filterM (inLayer Top) stack
        overlayStack stack = W.filterM (inLayer Background) stack

        orElse = flip fromMaybe

        isTopLevel (TopLevel _) = pure True
        isTopLevel _ = pure False

        inLayer :: Layer -> Window -> WXYZ Bool
        inLayer layer (LayerSurface ptr) = io $ do l <- layerSurfaceGetLayer ptr
                                                   pure $ l == layer
        inLayer _ _ = pure False
