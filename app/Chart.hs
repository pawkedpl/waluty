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
        [] -> putStrLn "Podaj co najmniej jedną walutę jako argument."
        currs -> do
            manager <- newManager tlsManagerSettings
            now <- getCurrentTime
            let today = utctDay now
                startDay = addDays (-30) today
                timestamp = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" now
                currStr = List.intercalate "_" currs
                fileName = "plots" </> (timestamp ++ "_" ++ currStr ++ ".svg")

            putStrLn $ "Pobieram kursy walut: " ++ unwords currs ++ " z ostatnich 30 dni..."

            ratesLists <- mapM (\curr -> fetchRatesInRange manager curr startDay today >>= \rs -> return (curr, rs)) currs
            let filteredRates = filter (not . null . snd) ratesLists

            when (null filteredRates) $
                putStrLn "Nie udało się pobrać żadnych kursów dla podanych walut."

            let allPerCurr =
                    [ (curr, reverse $ List.sortOn fst rs)
                    | (curr, rs) <- filteredRates
                    ]

            createDirectoryIfMissing True "plots"

            toFile def fileName $ do
                layout_title .= "Kursy walut do PLN (ostatnie 10 dni)"
                layout_x_axis . laxis_title .= "Data"
                layout_y_axis . laxis_title .= "Kurs"
                mapM_ (\(curr, points) -> plot (line curr [points])) allPerCurr

            putStrLn $ "Wykres zapisano do " ++ fileName
