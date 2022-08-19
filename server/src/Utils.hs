module Utils (immediatePutStrLn, immediatePrint) where


import System.IO (stdout, hFlush)


immediatePutStrLn :: String -> IO ()
immediatePutStrLn text = do
  putStrLn text
  hFlush stdout


immediatePrint :: Show a => a -> IO ()
immediatePrint x = do
  print x
  hFlush stdout
