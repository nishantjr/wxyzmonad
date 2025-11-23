{-# LANGUAGE CApiFFI #-}

module Main (main) where

import           Config (keyBindings)
import           WXYZMonad

import           Data.Word
import qualified Data.Map as M
import           Foreign.Storable
import           Foreign.Ptr
import           Foreign.C.Types



#include "tinywl.h"



foreign import capi "tinywl.h wxyz_init"
    wxyz_init :: WXYZMonad CInt

foreign import capi "tinywl.h wxyz_next_event" _wxyz_next_event :: WXYZMonad (Ptr Event)

----------------------------------------------------

type WLKeyboardKeyState = Word32
state_Pressed :: WLKeyboardKeyState
state_Pressed = #const WL_KEYBOARD_KEY_STATE_PRESSED

type XdgTopLevel = Ptr ()
data Event = KeyPressEvent {
                time_msec :: Word32,
                keycode  :: KeyCode, -- libinput  keycode
                state :: Word32,
                keysym :: KeySym,
                modifiers :: Word32,
                seat :: Ptr () -- Don't try storing this. There's no guarantee
                               -- it will be available after the current event is handled.
                               -- Needed to pass to wlr_seat_keyboard_notify_key
             }
           | XdgTopLevelNewEvent { toplevel :: XdgTopLevel }
           | XdgTopLevelDestroyEvent  { toplevel :: XdgTopLevel }


  deriving Show

next_event :: WXYZMonad (Maybe Event)
next_event =
  do ptr <- _wxyz_next_event
     if (ptr == nullPtr)
     then pure Nothing
     else do ty <- #{peek struct tinywl_event, type} ptr
             unparse ty ptr
  where
    unparse :: Word8 -> Ptr Event -> WXYZMonad (Maybe Event)
    unparse #{const KEYBOARD_KEY} ptr
        = do time_msec  <- (#{peek struct tinywl_event, keyboard_key.event.keycode} ptr)
             keycode    <- (#{peek struct tinywl_event, keyboard_key.event.keycode} ptr)
             state      <- (#{peek struct tinywl_event, keyboard_key.event.state}   ptr)
             keysym     <- (#{peek struct tinywl_event, keyboard_key.keysym}        ptr)
             modifiers  <- (#{peek struct tinywl_event, keyboard_key.modifiers}     ptr)
             seat       <- (#{peek struct tinywl_event, keyboard_key.seat}          ptr)
             pure $ Just (KeyPressEvent time_msec keycode state keysym modifiers seat)
    unparse #{const XDG_TOPLEVEL_NEW} ptr
        = do toplevel <- (#{peek struct tinywl_event, xdg_toplevel_new.toplevel} ptr)
             pure $ Just (XdgTopLevelNewEvent toplevel)
    unparse #{const XDG_TOPLEVEL_DESTROY} ptr
        = do toplevel <- (#{peek struct tinywl_event, xdg_toplevel_new.toplevel} ptr)
             pure $ Just (XdgTopLevelDestroyEvent toplevel)

foreign import capi "tinywl.h wxyz_shutdown"
    wxyz_shutdown :: WXYZMonad ()
foreign import capi "wlr/types/wlr_seat.h wlr_seat_keyboard_notify_key"
                                -- seat
    _wlr_seat_keyboard_notify_key :: Ptr () -> Word32 -> KeyCode -> WLKeyboardKeyState -> IO ()


-- TODO: Ideally, to allow unit testing, I would like to have a method
-- similar to:
--
--    event_stream :: IO [Event]
--    event_stream = do e <- next_event
--                      putStrLn $ show e
--                      case e of
--                        Nothing -> pure []
--                        Just e  -> do rest <- event_stream
--                                      pure $ e:rest
--
--  However, due to the IO wrapper, this never returns.
--  An alternative maybe to define a type class.

handle_event :: Event -> IO ()
handle_event (KeyPressEvent time_msec keycode state keysym modifiers seat)
    = case M.lookup (modifiers, keysym) keyBindings
        of Just action | state == state_Pressed
                -> action
           _    -> _wlr_seat_keyboard_notify_key seat time_msec keycode state
handle_event e = putStrLn $ show e

main_loop :: IO ()
main_loop = do e <- next_event
               case e of
                 Nothing -> pure ()
                 Just e  -> do handle_event e
                               main_loop

main :: IO ()
main = do  ret <- wxyz_init
           if (ret /= 0)
              then pure ()
              else main_loop
