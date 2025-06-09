{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
    ( parseRequest, httpLbs, responseBody
    , newManager, Manager, tlsManagerSettings, HttpException )
import Data.Aeson hiding ((.=))
import Data.Aeson.Key (fromString)
import System.Environment (getArgs)
import Data.Time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Control.Monad (when)
import Control.Exception (catch)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Char (toUpper)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List

-- Rate and RateTable adapted to parse tables endpoint (array of tables)
data Rate = Rate
  { currency :: String
  , code :: String
  , mid :: Double
  } deriving Show

instance FromJSON Rate where
  parseJSON = withObject "Rate" $ \v ->
    Rate <$> v .: fromString "currency"
         <*> v .: fromString "code"
         <*> v .: fromString "mid"

data RateTable = RateTable
  { effectiveDate :: String
  , rates :: [Rate]
  } deriving Show

instance FromJSON RateTable where
  parseJSON = withObject "RateTable" $ \v ->
    RateTable <$> v .: fromString "effectiveDate"
              <*> v .: fromString "rates"

-- The API returns an array of tables in the date-range query
type RateTables = [RateTable]

-- Fetch rates over date range for a given currency and table "A"
fetchRatesInRange :: Manager -> String -> Day -> Day -> IO [(Day, Double)]
fetchRatesInRange manager curr startDay endDay = do
  let
    startStr = formatTime defaultTimeLocale "%Y-%m-%d" startDay
    endStr = formatTime defaultTimeLocale "%Y-%m-%d" endDay
    url = "https://api.nbp.pl/api/exchangerates/tables/A/" ++ startStr ++ "/" ++ endStr ++ "?format=json"

    handler :: HttpException -> IO [(Day, Double)]
    handler _ = return []

  (do
      req <- parseRequest url
      resp <- httpLbs req manager
      let body = responseBody resp
      case eitherDecode body :: Either String RateTables of
        Right tables -> do
          -- For each date, find the mid rate for the currency if exists
          let pairs =
                [ (dayFromString (effectiveDate t), findMid curr (rates t))
                | t <- tables
                , currencyExists curr (rates t)
                ]
          return pairs
        Left err -> do
          putStrLn $ "Błąd dekodowania JSON: " ++ err
          return []
    )
    `catch` handler

-- Helper: parse date string "YYYY-MM-DD" to Day
dayFromString :: String -> Day
dayFromString s =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
    Just d -> d
    Nothing -> error $ "Niepoprawna data: " ++ s

-- Check if currency exists in the rates list
currencyExists :: String -> [Rate] -> Bool
currencyExists c = any (\r -> map toUpper (code r) == map toUpper c)

-- Find mid for currency, fallback error if missing
findMid :: String -> [Rate] -> Double
findMid c rs =
  case filter (\r -> map toUpper (code r) == map toUpper c) rs of
    (r:_) -> mid r
    [] -> error $ "Brak kursu dla waluty: " ++ c

main :: IO ()
main = do
  args <- getArgs
  case args of
    [curr] -> do
      manager <- newManager tlsManagerSettings
      today <- utctDay <$> getCurrentTime
      let startDay = addDays (-30) today -- last 30 days

      putStrLn $ "Pobieram kursy " ++ curr ++ " z ostatnich 10 dni..."

      ratesList <- fetchRatesInRange manager curr startDay today

      let sortedRates = List.sortOn fst ratesList
          last10 = take 10 $ reverse sortedRates

      when (null last10) $ putStrLn "Nie udało się pobrać żadnych kursów."

      toFile def (curr ++ "_to_PLN.png") $ do
        layout_title .= "Kurs " ++ curr ++ " do PLN (ostatnie 10 dni)"
        layout_x_axis . laxis_title .= "Data"
        layout_y_axis . laxis_title .= "Kurs"
        plot (line (curr ++ "/PLN") [ [ (d, r) | (d, r) <- last10 ] ])

    _ -> putStrLn "Podaj walutę jako argument."
