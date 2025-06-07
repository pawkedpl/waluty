{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Aeson.Key (fromString)
import System.Environment (getArgs)
import Data.Char (toLower)
import Data.Function (on)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as Pandoc
import Text.Pandoc (runIOorExplode, def, writeLaTeX)
import qualified Data.Text.IO as T
import System.Process (callProcess)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, removeFile)
import Control.Exception (catch, IOException)

-- Example API: NBP (Narodowy Bank Polski)
-- We'll fetch current exchange rates for major currencies

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

showRates :: [Rate] -> IO ()
showRates = mapM_ print

matchesQuery :: String -> String -> Bool
matchesQuery query value = map toLower query `isInfixOf` map toLower value
  where isInfixOf = T.isInfixOf `on` T.pack

showFilteredRates :: [String] -> [Rate] -> IO ()
showFilteredRates queries rs =
    let filtered = filter (\r -> any (\q -> matchesQuery q (code r) || matchesQuery q (currency r)) queries) rs
    in if null filtered
        then putStrLn "Nie znaleziono podanych walut."
        else mapM_ print filtered

genReport :: [Rate] -> Pandoc.Pandoc
genReport rs =
    let headers = map (Pandoc.plain . Pandoc.text . T.pack) ["Currency", "Code", "Rate"]
        rows = [ [ Pandoc.plain (Pandoc.text (T.pack (currency r)))
                  , Pandoc.plain (Pandoc.text (T.pack (code r)))
                  , Pandoc.plain (Pandoc.text (T.pack (show (mid r)))) ]
                | r <- rs ]
        tbl = Pandoc.simpleTable headers rows
    in Pandoc.Pandoc mempty (Pandoc.toList tbl)

printReport :: [Rate] -> IO ()
printReport _ = return ()

saveReportAsTeX :: FilePath -> Pandoc.Pandoc -> String -> IO ()
saveReportAsTeX path doc _ = do
    texBodyRaw <- runIOorExplode $ writeLaTeX def doc
    let texBody1 = T.replace (T.pack "longtable") (T.pack "tabular") texBodyRaw
        texBody2 = T.replace (T.pack "\\begin{longtable}") (T.pack "\\begin{tabular}") texBody1
        texBody3 = T.replace (T.pack "\\end{longtable}") (T.pack "\\end{tabular}") texBody2
        -- Remove lines with longtable-specific commands
        texLines = T.lines texBody3
        texLinesFiltered = filter (\l -> not (any (`T.isInfixOf` l)
            [T.pack "\\endhead", T.pack "\\endfirsthead", T.pack "\\endfoot", T.pack "\\endlastfoot"])) texLines
        texBodyFinal = T.unlines texLinesFiltered
        preamble = T.pack $ unlines
            ["\\documentclass{article}",
             "\\usepackage{booktabs}",
             "\\begin{document}"]
        postamble = T.pack "\\end{document}\n"
        fullTex = T.concat [preamble, T.pack "\n", texBodyFinal, T.pack "\n", postamble]
    T.writeFile path fullTex

makeReportTitle :: String -> [Rate] -> IO String
makeReportTitle baseCurrency rs = do
    now <- getZonedTime
    let dateTime = formatTime defaultTimeLocale "%Y-%m-%d_%H-%M-%S" now
        currencies = if null rs then "none" else concatMap ((++"-") . code) rs
        currenciesStr = if null currencies then "none" else init currencies
    return $ dateTime ++ "_" ++ baseCurrency ++ "_" ++ currenciesStr

saveReportAsPDF :: FilePath -> IO ()
saveReportAsPDF texPath = do
    let (dir, file) = let rev = reverse texPath in
                          case span (/= '/') rev of
                            (fname, '/' : rest) -> (reverse rest, reverse fname)
                            (fname, _) -> (".", reverse fname)
    callProcess "pdflatex" ["-output-directory", dir, texPath]
    return ()

main :: IO ()
main = do
    args <- getArgs
    putStrLn "Pobieranie kursów walut z NBP..."
    result <- fetchRates
    case result of
        Right rs ->
            let filtered = if null args then rs else filter (\r -> any (\q -> matchesQuery q (code r) || matchesQuery q (currency r)) args) rs
                doc = genReport filtered
                baseCurrency = "PLN"
                reportsDir = "reports"
            in do
                if null args
                    then do
                        putStrLn "Aktualne kursy walut:"
                        showRates rs
                    else do
                        putStrLn $ "Kursy dla: " ++ show args
                        showFilteredRates args rs
                printReport filtered
                title <- makeReportTitle baseCurrency filtered
                let texFile = reportsDir ++ "/" ++ title ++ ".tex"
                    pdfFile = reportsDir ++ "/" ++ title ++ ".pdf"
                createDirectoryIfMissing True reportsDir
                saveReportAsTeX texFile doc title
                saveReportAsPDF texFile
                -- Remove the .tex and auxiliary files from the reports folder, keep only the PDF
                let auxFiles = map (\ext -> reportsDir ++ "/" ++ title ++ ext) [".tex", ".aux", ".log"]
                mapM_ (\f -> removeFile f `catch` (\(_ :: IOException) -> return ())) (filter (/= pdfFile) auxFiles)
                putStrLn $ "Zapisano raport PDF do " ++ pdfFile ++ "."
        Left err -> putStrLn $ "Błąd pobierania danych: " ++ err
