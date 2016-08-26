module Main where

import qualified Codegen as C
import Parser
import Control.Monad.Trans
import System.Console.Haskeline

import System.IO
process :: String -> IO ()
process line = do
  let res = parseTopLevel line
  case res of
    Left err -> print err
    Right program -> do
      -- toHandle  <- getAndOpenFile "Copy to: " WriteMode
      handler <- openFile "output" WriteMode
      hPutStr handler (C.program program)
      hClose handler
      print  program




main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just x  -> (liftIO $ process x) >> loop
