{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Aeson.Key (fromString)
import System.Environment (getArgs)
import Data.Time
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Control.Monad (when)
import Control.Exception (catch, IOException)
import qualified Data.Vector as V
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data Rate = Rate { currency :: String, code :: String, mid :: Double } deriving Show

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

-- Pobieranie kursów dla podanej waluty z danego dnia
fetchRateForDay :: String -> Day -> IO (Maybe (Day, Double))
fetchRateForDay curr day = do
    let dayStr = formatTime defaultTimeLocale "%Y-%m-%d" day
        url = "https://api.nbp.pl/api/exchangerates/rates/A/" ++ map toUpper curr ++ "/" ++ dayStr ++ "?format=json"
    response <- (simpleHttp url >>= \body -> 
                    case eitherDecode body of
                        Right (table :: RateTable) -> return $ Just (day, findMid curr (rates table))
                        Left _ -> return Nothing)
                `catch` (\(_ :: IOException) -> return Nothing)
    return response

-- Znalezienie kursu mid dla waluty (powinno być 1 element)
findMid :: String -> [Rate] -> Double
findMid _ (r:_) = mid r
findMid _ [] = error "Brak kursu"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [curr] -> do
            today <- utctDay <$> getCurrentTime
            let days = reverse $ take 15 $ iterate (addDays (-1)) today -- weź 15 dni wstecz (na wszelki wypadek, bo weekendy i święta)
            putStrLn $ "Pobieram kursy " ++ curr ++ " z ostatnich dni..."
            -- Pobierz kursy, filtrując Nothing
            ratesList <- fmap (filter (\(_, r) -> r > 0)) . fmap concat . sequence $ do
                day <- days
                return $ do
                    res <- fetchRateForDay curr day
                    case res of
                      Just (d, rate) -> return [(d, rate)]
                      Nothing -> return []
            -- Weź ostatnie 10 dni z dostępnych
            let last10 = take 10 ratesList

            when (null last10) $ putStrLn "Nie udało się pobrać żadnych kursów."

            -- Generowanie wykresu
            toFile def (curr ++ "_to_PLN.png") $ do
                layout_title .= "Kurs " ++ curr ++ " do PLN (ostatnie 10 dni)"
                layout_x_axis . laxis_title .= "Data"
                layout_y_axis . laxis_title .= "Kurs"
                plot (line (curr ++ "/PLN") [ [ (d, r) | (d, r) <- last10 ] ])
  where
    -- Instancja Show dla Day do wyświetlania na osi X w formacie "MM-DD"
    instance PlotValue Day where
        toValue = fromIntegral . toModifiedJulianDay

