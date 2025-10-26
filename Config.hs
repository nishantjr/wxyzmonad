module Config where


hello :: IO ()
hello = putStr "====================\nHello!\n============================\n"

foreign export ccall hello :: IO ()
