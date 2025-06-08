{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Aeson.Key (fromString)
import System.Environment (getArgs)
import Data.Char (toUpper)
import Control.Exception (catch, IOException)
import Text.Read (readMaybe)
import Numeric (showFFloat)

data Rate = Rate { currency :: String, code :: String, mid :: Double } deriving Show

instance FromJSON Rate where
    parseJSON = withObject "Rate" $ \v ->
        Rate <$> v .: fromString "currency"
             <*> v .: fromString "code"
             <*> v .: fromString "mid"

newtype RatesTable = RatesTable { rates :: [Rate] } deriving Show

instance FromJSON RatesTable where
    parseJSON = withObject "RatesTable" $ \v ->
        RatesTable <$> v .: fromString "rates"

fetchRates :: IO (Either String [Rate])
fetchRates = do
    let url = "https://api.nbp.pl/api/exchangerates/tables/A?format=json"
    body <- simpleHttp url
    case eitherDecode body of
        Right [table] -> return $ Right (rates table)
        Left err -> return $ Left err
        _ -> return $ Left "Unexpected JSON structure"

showRounded4 :: Double -> String
showRounded4 x = showFFloat (Just 4) x ""

convertCurrency :: Double -> String -> String -> [Rate] -> Maybe Double
convertCurrency amount fromCode toCode rates = do
    let codeToUpper = map toUpper
        findRate c
          | c == "PLN" = Just 1.0
          | otherwise = lookup c codeRatePairs
        codeRatePairs = [(codeToUpper (code r), mid r) | r <- rates]
    fromRate <- findRate (codeToUpper fromCode)
    toRate <- findRate (codeToUpper toCode)
    return $ amount * fromRate / toRate

main :: IO ()
main = do
    args <- getArgs
    case args of
        (amountStr : fromCode : toCode : _) -> do
            case readMaybe amountStr of
                Just amount -> do
                    putStrLn "Pobieranie kursów walut z NBP..."
                    result <- fetchRates
                    case result of
                        Left err -> putStrLn $ "Błąd pobierania danych: " ++ err
                        Right rs -> case convertCurrency amount fromCode toCode rs of
                            Just converted -> putStrLn $ show amount ++ " " ++ map toUpper fromCode ++ " = " ++ showRounded4 converted ++ " " ++ map toUpper toCode
                            Nothing -> putStrLn "Nie znaleziono jednej z podanych walut."
                Nothing -> putStrLn "Podaj poprawną kwotę."
        _ -> putStrLn "Użycie: change <kwota> <z waluty> <na walutę>"

