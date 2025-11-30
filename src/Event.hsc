{-# LANGUAGE CApiFFI #-}

module Event
    ( Event(..)
    , Seat
    , WLKeyboardKeyState
    , XdgTopLevel
    , state_Pressed
    , next_event
    )
    where

import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable

import           Key

#include "clib.h"

type WLKeyboardKeyState = Word32
state_Pressed :: WLKeyboardKeyState
state_Pressed = #const WL_KEYBOARD_KEY_STATE_PRESSED

data XdgTopLevel
data Seat
data Event = KeyPressEvent {
                time_msec :: Word32,
                keycode  :: KeyCode, -- libinput  keycode
                state :: Word32,
                keysym :: KeySym,
                modifiers :: Word32,
                seat :: Ptr Seat -- Don't try storing this. There's no guarantee
                                 -- it will be available after the current event is handled.
                                 -- Needed to pass to wlr_seat_keyboard_notify_key
             }
           | XdgTopLevelNewEvent { toplevel :: Ptr XdgTopLevel }
           | XdgTopLevelDestroyEvent  { toplevel :: Ptr XdgTopLevel }
 deriving Show

foreign import capi "clib.h wxyz_next_event"
    _wxyz_next_event :: IO (Ptr Event)
next_event :: IO (Maybe Event)
next_event =
  do ptr <- _wxyz_next_event
     if (ptr == nullPtr)
     then pure Nothing
     else do ty <- #{peek struct wxyz_event, type} ptr
             unparse ty ptr
  where
    unparse :: Word8 -> Ptr Event -> IO (Maybe Event)
    unparse #{const KEYBOARD_KEY} ptr
        = do time_msec  <- (#{peek struct wxyz_event, keyboard_key.event.keycode} ptr)
             keycode    <- (#{peek struct wxyz_event, keyboard_key.event.keycode} ptr)
             st         <- (#{peek struct wxyz_event, keyboard_key.event.state}   ptr)
             keysym     <- (#{peek struct wxyz_event, keyboard_key.keysym}        ptr)
             modifiers  <- (#{peek struct wxyz_event, keyboard_key.modifiers}     ptr)
             seat       <- (#{peek struct wxyz_event, keyboard_key.seat}          ptr)
             pure $ Just (KeyPressEvent time_msec keycode st keysym modifiers seat)
    unparse #{const XDG_TOPLEVEL_NEW} ptr
        = do toplevel <- (#{peek struct wxyz_event, xdg_toplevel_new.toplevel} ptr)
             pure $ Just (XdgTopLevelNewEvent toplevel)
    unparse #{const XDG_TOPLEVEL_DESTROY} ptr
        = do toplevel <- (#{peek struct wxyz_event, xdg_toplevel_new.toplevel} ptr)
             pure $ Just (XdgTopLevelDestroyEvent toplevel)
    unparse e _
        = error $ "Unknown event" ++ (show e)
