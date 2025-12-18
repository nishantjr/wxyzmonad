{-# LANGUAGE CApiFFI #-}

module WXYZMain (wxyz) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.State.Class ()
import qualified Data.Map as M
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           Event
import           Key
import           Operations
import           StackSet hiding (modify)
import           WXYZMonad

-- Main Loop
------------

foreign import capi "wlr/types/wlr_seat.h wlr_seat_keyboard_notify_key"
    _wlr_seat_keyboard_notify_key :: Ptr Seat -> Word32 -> KeyCode -> WLKeyboardKeyState -> IO ()
foreign import capi "wlr/types/wlr_seat.h wxyz_toplevel_set_position"
    _wxyz_toplevel_set_position :: Ptr CXdgTopLevel -> Position -> Position -> IO ()
foreign import capi "wlr/types/wlr_seat.h wxyz_toplevel_set_size"
    _wxyz_toplevel_set_size :: Ptr CXdgTopLevel -> Dimension -> Dimension -> IO ()

handle_event :: Event -> WXYZ ()
handle_event (KeyPressEvent time_msec keycode st keysym modifiers seat)
    = do config <- ask
         case M.lookup (modifiers, keysym) (keyBindings config)
           of Just action | st == state_Pressed
                   -> action
              _    -> io $ _wlr_seat_keyboard_notify_key seat time_msec keycode st

handle_event (XdgTopLevelMapEvent win)
    = do st <- get
         put $ st{ windowset = insertUp (TopLevel win) (windowset st) }
         refresh

handle_event (XdgTopLevelUnmapEvent win)
    = do st <- get
         put $ st{ windowset = StackSet.delete (TopLevel win) (windowset st) }
         refresh

-- TODO: This is a hack: We just update the size of the current screen,
-- and re-layout. This is a work-around for incorrectly structured
-- StackSet.
handle_event (OutputNewEvent _output width height)
    = do st <- get
         let curr = (current.windowset) st
         let curr' = curr { screenDetail= SD $ Rectangle
               { rect_x=0, rect_y=0,
                 rect_width=(coerce width), rect_height=(coerce height) }
             }
         put $ st{ windowset = (windowset st) { current=curr' } }
         refresh
    where coerce n = fromIntegral n

handle_event e@(OutputDestroyEvent _output)
    = io $ putStrLn $ "unhandled event: " ++ (show e)

main_loop :: WXYZ ()
main_loop = do e <- io next_event
               case e of
                 Nothing -> pure ()
                 Just e' -> do handle_event e'
                               main_loop

foreign import capi "clib.h wxyz_init"     _wxyz_init :: IO CInt
foreign import capi "clib.h wxyz_shutdown" _wxyz_shutdown :: IO ()

wxyz :: WXYZConf ->  IO ()
wxyz config =
    do let layout = layoutHook config
           num_outputs = 0
           initialWinset = let padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
                in new layout (padToLen num_outputs (WXYZMonad.workspaces config)) $ map SD [Rectangle 0 0 500 500]
           st = State initialWinset
       ret <- _wxyz_init
       if (ret /= 0)
          then pure ()
          else runWXYZ config st (startupHook config)
            >> runWXYZ config st main_loop
            >> (io _wxyz_shutdown)

