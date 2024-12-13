module Main (main) where

import Data.List.Extra ((!?))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))

import AllDays (allDays)

main :: IO ()
main = do
    args <- getArgs
    d <- case args of
          [s] | [(n, "")] <- reads s,
                n >= 1 && n <= 25
             -> return n
          [] -> do
            putStrLn "Which day?"
            input <- getLine
            hSetBuffering stdin  LineBuffering
            hSetBuffering stdout NoBuffering
            case reads input of
                [(n, "")] | n >= 1 && n <= 25 -> return n
                _ -> putStrLn ("Invalid day! " ++ explanation) >> exitFailure
          _ -> putStrLn ("Invalid command line Argument! " ++ explanation) >> exitFailure
    case allDays !? (d - 1) of
        Just day -> day
        Nothing  -> putStrLn "Day not implemented yet"
  where explanation = "It has to be a number between 1 and 25."
