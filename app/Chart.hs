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
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Char (toUpper)
import qualified Data.List as List
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

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

type RateTables = [RateTable]

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

dayFromString :: String -> Day
dayFromString s =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" s of
    Just d -> d
    Nothing -> error $ "Niepoprawna data: " ++ s

currencyExists :: String -> [Rate] -> Bool
currencyExists c = any (\r -> map toUpper (code r) == map toUpper c)

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
      let startDay = addDays (-30) today

      putStrLn $ "Pobieram kursy " ++ curr ++ " z ostatnich 30 dni..."

      ratesList <- fetchRatesInRange manager curr startDay today

      let sortedRates = List.sortOn fst ratesList
          last10 = take 10 $ reverse sortedRates

      when (null last10) $ putStrLn "Nie udało się pobrać żadnych kursów."

      -- Create plots directory if it doesn't exist
      createDirectoryIfMissing True "plots"

      let fileName = "plots" </> (curr ++ "_to_PLN.svg")

      toFile def fileName $ do
        layout_title .= "Kurs " ++ curr ++ " do PLN (ostatnie 10 dni)"
        layout_x_axis . laxis_title .= "Data"
        layout_y_axis . laxis_title .= "Kurs"
        plot (line (curr ++ "/PLN") [ [ (d, r) | (d, r) <- last10 ] ])

      putStrLn $ "Wykres zapisano do " ++ fileName

    _ -> putStrLn "Podaj walutę jako argument."
