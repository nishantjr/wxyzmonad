{-# LANGUAGE CApiFFI #-}

-- | Operations that a user's configuration may perform

module Operations
    ( hello
    , next_toplevel
    , shell
    , terminate
    ) where

import           Control.Monad.IO.Class
import qualified System.Process as P

import           WXYZMonad

foreign import capi "clib.h wxyz_terminate"
    _terminate :: IO ()
terminate :: WXYZ ()
terminate = liftIO _terminate

foreign import capi "clib.h wxyz_next_toplevel"
    _next_toplevel :: IO ()
next_toplevel :: WXYZ ()
next_toplevel = liftIO _next_toplevel

shell :: String -> WXYZ ()
shell cmd = liftIO $ do _ <- P.createProcess $ P.shell cmd
                        pure ()

hello :: WXYZ ()
hello = liftIO $ putStr "====================\nHello!\n============================\n"
