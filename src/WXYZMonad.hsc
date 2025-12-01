{-# LANGUAGE CApiFFI #-}

module WXYZMonad
    ( WXYZ(..)
    , WXYZConf(..)
    , WXYZState(..)
    , wxyz
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.State.Class ()
import           Data.List
import qualified Data.Map as M
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr

import           Event
import           Key

#include "clib.h"


-- Our window manager monad
---------------------------

type Window = Ptr XdgTopLevel

data WXYZState = State {
    windows :: ![Window]
}

data WXYZConf = Config {
    keyBindings :: M.Map (Modifier,KeySym) (WXYZ ())
}

newtype WXYZ a = WXYZ (ReaderT WXYZConf (StateT WXYZState IO) a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState WXYZState, MonadReader WXYZConf)

runWXYZ :: WXYZConf -> WXYZState -> WXYZ a -> IO (a, WXYZState)
runWXYZ c st (WXYZ a) = runStateT (runReaderT a c) st

-- Main Loop
------------

foreign import capi "wlr/types/wlr_seat.h wlr_seat_keyboard_notify_key"
    _wlr_seat_keyboard_notify_key :: Ptr Seat -> Word32 -> KeyCode -> WLKeyboardKeyState -> IO ()

handle_event :: Event -> WXYZ ()
handle_event (KeyPressEvent time_msec keycode st keysym modifiers seat)
    = do config <- ask
         case M.lookup (modifiers, keysym) (keyBindings config)
           of Just action | st == state_Pressed
                   -> action
              _    -> liftIO $ _wlr_seat_keyboard_notify_key seat time_msec keycode st
handle_event (XdgTopLevelMapEvent win)
    = do st <- get
         put $ st{ windows = (windows st) ++ [win] }
handle_event (XdgTopLevelUnmapEvent win)
    = do st <- get
         put $ st{ windows = delete win (windows st) }

main_loop :: WXYZ ()
main_loop = do e <- liftIO next_event
               case e of
                 Nothing -> pure ()
                 Just e' -> do handle_event e'
                               main_loop

foreign import capi "clib.h wxyz_init"     _wxyz_init :: IO CInt
foreign import capi "clib.h wxyz_shutdown" _wxyz_shutdown :: IO ()

wxyz :: WXYZConf ->  IO ()
wxyz config =
    do let st = State []
       ret <- _wxyz_init
       if (ret /= 0)
          then pure ()
          else runWXYZ config st main_loop >> (liftIO _wxyz_shutdown)
