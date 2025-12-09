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
import           Data.Int
import           Foreign.Ptr
import           Foreign.Storable

import           Key

#include "clib.h"

type WLKeyboardKeyState = Word32
state_Pressed :: WLKeyboardKeyState
state_Pressed = #const WL_KEYBOARD_KEY_STATE_PRESSED

data XdgTopLevel
data COutput
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
           | XdgTopLevelMapEvent { toplevel :: Ptr XdgTopLevel }
           | XdgTopLevelUnmapEvent  { toplevel :: Ptr XdgTopLevel }
           | OutputNewEvent {
               output :: Ptr COutput,
               width :: Int32,
               height :: Int32
             }
           | OutputDestroyEvent  { output :: Ptr COutput }
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
    unparse #{const XDG_TOPLEVEL_MAP} ptr
        = do toplevel <- (#{peek struct wxyz_event, xdg_toplevel_map.toplevel} ptr)
             pure $ Just (XdgTopLevelMapEvent toplevel)
    unparse #{const XDG_TOPLEVEL_UNMAP} ptr
        = do toplevel <- (#{peek struct wxyz_event, xdg_toplevel_unmap.toplevel} ptr)
             pure $ Just (XdgTopLevelUnmapEvent toplevel)
    unparse #{const OUTPUT_NEW} ptr
        = do output <- (#{peek struct wxyz_event, output_new.output} ptr)
             height <- (#{peek struct wxyz_event, output_new.height} ptr)
             width <- (#{peek struct wxyz_event, output_new.width} ptr)
             pure $ Just (OutputNewEvent output width height)
    unparse #{const OUTPUT_DESTROY} ptr
        = do output <- (#{peek struct wxyz_event, output_destroy.output} ptr)
             pure $ Just (OutputDestroyEvent output)
    unparse e _
        = error $ "Unknown event" ++ (show e)
