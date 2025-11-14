module WXYZMonad (WXYZMonad(..)) where

-- TODO: For now, we are just a wrapper around IO.
-- State variables are store in global by the C side.
type WXYZMonad = IO

