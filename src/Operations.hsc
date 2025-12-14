{-# LANGUAGE CApiFFI #-}

-- | Operations that a user's configuration may perform

module Operations
    ( hello
    , shell
    , terminate
    , setTopFocus
    , windows
    , sendMessage
    ) where

import           Control.Monad
import           Control.Monad.Extra (whenJust)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid (Any(..))
import qualified Data.Map as M
import qualified System.Process as P

import qualified StackSet as W
import           WXYZMonad
import           Tiling (Full(..))

foreign import capi "clib.h wxyz_terminate"
    _terminate :: IO ()
terminate :: WXYZ ()
terminate = liftIO _terminate

shell :: String -> WXYZ ()
shell cmd = liftIO $ do _ <- P.createProcess $ P.shell cmd
                        pure ()

hello :: WXYZ ()
hello = liftIO $ putStr "====================\nHello!\n============================\n"

-- ---------------------------------------------------------------------
-- Managing windows

-- | Modify the current window list with a pure function, and refresh

-- TODO: This function has been severly cut down from the XMonad version.
-- Restore missing functionality
windows :: (WindowSet -> WindowSet) -> WXYZ ()
windows f = do
    State { windowset = old } <- get
    let ws = f old

    modify (\s -> s { windowset = ws })

    -- for each workspace, layout the currently visible workspaces
    let allscreens     = W.screens ws
        summed_visible = scanl (++) [] $ map (W.integrate' . W.stack . W.workspace) allscreens
    rects <- fmap concat $ forM (zip allscreens summed_visible) $ \ (w, vis) -> do
        let wsp   = W.workspace w
            this  = W.view n ws
            n     = W.tag wsp
            tiled = (W.stack . W.workspace . W.current $ this)
                    >>= W.filter (`M.notMember` W.floating ws)
                    >>= W.filter (`notElem` vis)
            viewrect = screenRect $ W.screenDetail w

        -- just the tiled windows:
        -- now tile the windows on this workspace, modified by the gap
        (rs, ml') <- runLayout wsp { W.stack = tiled } viewrect `catchWXYZ`
                     runLayout wsp { W.stack = tiled, W.layout = Layout Full } viewrect
        updateLayout n ml'

        let m   = W.floating ws
            flt = [(fw, scaleRationalRect viewrect r)
                    | fw <- filter (`M.member` m) (W.index this)
                    , fw `notElem` vis
                    , Just r <- [M.lookup fw m]]
            vs = flt ++ rs

        -- return the visible windows for this workspace:
        return vs

    mapM_ (uncurry tileWindow) rects

    setTopFocus

-- | Modify the @WindowSet@ in state with no special handling.
modifyWindowSet :: (WindowSet -> WindowSet) -> WXYZ ()
modifyWindowSet f = modify $ \xst -> xst { windowset = f (windowset xst) }

-- | Perform an @WXYZ@ action and check its return value against a predicate p.
-- If p holds, unwind changes to the @WindowSet@ and replay them using @windows@.
windowBracket :: (a -> Bool) -> WXYZ a -> WXYZ a
windowBracket p action = withWindowSet $ \old -> do
  a <- action
  when (p a) . withWindowSet $ \new -> do
    modifyWindowSet $ const old
    windows         $ const new
  return a

-- | Perform an @WXYZ@ action. If it returns @Any True@, unwind the
-- changes to the @WindowSet@ and replay them using @windows@. This is
-- a version of @windowBracket@ that discards the return value and
-- handles an @WXYZ@ action that reports its need for refresh via @Any@.
windowBracket_ :: WXYZ Any -> WXYZ ()
windowBracket_ = void . windowBracket getAny


-- | Move and resize @w@ such that it fits inside the given rectangle,
-- including its border.
tileWindow :: Window -> Rectangle -> WXYZ ()
tileWindow w r = do
    -- give all windows at least 1x1 pixels
    let bw = 1
        least x | x <= bw*2  = 1
                | otherwise  = x - bw*2
    io $ moveResizeWindow w (rect_x r) (rect_y r)
                          (least $ rect_width r) (least $ rect_height r)

-- | Produce the actual rectangle from a screen and a ratio on that screen.
scaleRationalRect :: Rectangle -> W.RationalRect -> Rectangle
scaleRationalRect (Rectangle sx sy sw sh) (W.RationalRect rx ry rw rh)
 = Rectangle (sx + scale sw rx) (sy + scale sh ry) (scale sw rw) (scale sh rh)
 where scale s r = floor (toRational s * r)



foreign import capi "wlr/types/wlr_seat.h wxyz_toplevel_set_position"
    _wxyz_toplevel_set_position :: Window -> Position -> Position -> IO ()
foreign import capi "wlr/types/wlr_seat.h wxyz_toplevel_set_size"
    _wxyz_toplevel_set_size :: Window -> Dimension -> Dimension -> IO ()

moveResizeWindow :: Window -> Position -> Position -> Dimension -> Dimension -> IO ()
moveResizeWindow win x y w h
     = do _wxyz_toplevel_set_position win x y
          _wxyz_toplevel_set_size win w h


-- ---------------------------------------------------------------------
-- Setting keyboard focus

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: WXYZ ()
setTopFocus = withWindowSet $ maybe (pure ()) focusTopLevel . W.peek

foreign import capi "clib.h focus_toplevel"
    _focus_toplevel :: Window -> IO ()
focusTopLevel :: Window -> WXYZ ()
focusTopLevel w = liftIO $ _focus_toplevel w


------------------------------------------------------------------------
-- Message handling

-- | Throw a message to the current 'LayoutClass' possibly modifying how we
-- layout the windows, in which case changes are handled through a refresh.
sendMessage :: Message a => a -> WXYZ ()
sendMessage a = windowBracket_ $ do
    w <- gets $ W.workspace . W.current . windowset
    ml' <- handleMessage (W.layout w) (SomeMessage a) `catchWXYZ` return Nothing
    whenJust ml' $ \l' ->
        modifyWindowSet $ \ws -> ws { W.current = (W.current ws)
                                     { W.workspace = (W.workspace $ W.current ws)
                                       { W.layout = l' }}}
    return (Any $ isJust ml')

-- | Update the layout field of a workspace.
updateLayout :: WorkspaceId -> Maybe (Layout Window) -> WXYZ ()
updateLayout i ml = whenJust ml $ \l ->
    runOnWorkspaces $ \ww -> return $ if W.tag ww == i then ww { W.layout = l} else ww


