module Main (main) where

import Data.List.Extra ((!?))
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))

import AllDays (allDays)

main :: IO ()
main = do
    putStrLn "Which day?"
    input <- getLine
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout NoBuffering
    case reads input of
        [(n, "")]
            | n >= 1 && n <= 25 ->
                case allDays !? (n - 1) of
                    Just day -> day
                    Nothing  -> putStrLn "Day not implemented yet"
        _   -> putStrLn "Invalid day! It has to be a number between 1 and 25."
