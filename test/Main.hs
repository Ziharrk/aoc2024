module Main(main) where

import Control.Monad ( zipWithM_ )
import Data.Time ( diffUTCTime, UTCTime (..) )
import Data.Time.Clock.System ( getSystemTime, systemToUTCTime )
import Data.Time.Calendar.OrdinalDate ( toOrdinalDate )

import AllDays (allDays)

main :: IO ()
main = getDays >>= zipWithM_ printDay [1..]
  where
    getDays = do
      t <- systemToUTCTime <$> getSystemTime
      let (year, day) = toOrdinalDate (utctDay t)
      if year <= 2024
        then return $ take (day - 334) allDays
        else return allDays

    printDay :: Int -> IO () -> IO ()
    printDay n day = do
      let s = "Day " ++ show n
      putStrLn s >> putStrLn (replicate (length s) '=')
      t1 <- systemToUTCTime <$> getSystemTime
      day
      t2 <- systemToUTCTime <$> getSystemTime
      putStrLn $ "Time: " ++ show (diffUTCTime t2 t1)
      putStrLn ""
