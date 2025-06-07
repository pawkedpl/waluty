# currency-rates

A Haskell console application that fetches and displays currency exchange rates from the NBP (Narodowy Bank Polski) API.

## Features
- Fetches current exchange rates for major currencies from the NBP API.
- Filters and displays rates for currencies specified by code or country (as command-line arguments).
- Case-insensitive and matches both currency code and country name.

## Usage

### Build

```
cabal build
```

### Run

To display all available rates:

```
cabal run
```

To display rates for specific currencies by code or country (case-insensitive, matches whole words):

```
cabal run currency-rates USD EUR
cabal run currency-rates Japonia
cabal run currency-rates ameryka
```

## Example Output

```
Pobieranie kursów walut z NBP...
Kursy dla: ["USD","EUR"]
Rate {currency = "dolar amerykański", code = "USD", mid = 3.7565}
Rate {currency = "euro", code = "EUR", mid = 4.2881}
```

## Requirements
- GHC
- Cabal

## License
Specify your license in the `currency-rates.cabal` file.
