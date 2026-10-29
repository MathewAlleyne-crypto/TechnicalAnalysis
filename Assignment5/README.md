# Assignment 5 – Technical Analysis using R (Development Phase)

**Mathew Alleyne** · Student ID 664429889 · BDA400

The technical indicators are written in base R and follow the templates and pseudocode from the brief. Each indicator has its own file, and none of them uses a package.

## Files

| File | Function | Returns |
|---|---|---|
| `sma.R` | `sma(data, period)` | numeric vector, length `n - period + 1` |
| `ema.R` | `ema(data, period)` | numeric vector, length `n` |
| `macd.R` | `macd(data, short_period, long_period, signal_period)` | list: `macd_line`, `signal_line`, `histogram` |
| `stdev.R` | `stdev(data)` | single number (population SD) |
| `linreg.R` | `linreg(regressionSource, regressionLength, regressionOffset)` | list: `slope`, `intercept`, `predicted_values` |
| `rsi.R` | `rsi(data, period)` | numeric vector, length `n`; first `period` values are `NA` |
| `stoch_rsi.R` | `stoch_rsi(data, period, k_period, d_period)` | list: `k_line`, `d_line` |
| `crossover.R` | `crossover(arr1, arr2)` | character vector: `"Up"`, `"Down"`, `"None"` |
| `crossunder.R` | `crossunder(arr1, arr2)` | character vector: `"True"`, `"False"` (first element `"None"`) |
| `run_all.R` | demo | brief examples, then stock data frames, filters and trading rules on the portfolio |
| `test_indicators.R` | tests | 46 checks against hand calculations and independent methods |
| `portfolio.txt` | data | same symbols as Assignment 2: AAPL, MSFT, GOOGL, AMZN, TSLA |
| `stock_data_2025.csv` | data | saved Yahoo Finance prices for 2025, used when offline |
| `MathewAlleyne_BDA400_A05.docx` | report | cover page, explanation of each indicator, how to run |

`macd()` needs `ema()`, and `stoch_rsi()` needs `rsi()` and `sma()`. Source those files first; `run_all.R` does this for you.

## How to run

Set the working directory to this `Assignment5` folder (RStudio: *Session → Set Working Directory → To Source File Location*), then run:

```r
source("test_indicators.R")  # should end with: RESULT: 46 passed, 0 failed
source("run_all.R")          # examples + portfolio analysis
```

`run_all.R` downloads the 2025 prices with `quantmod`, as in Assignment 2. If `quantmod` is not installed, or there is no internet connection, it uses `stock_data_2025.csv` instead and gives the same results. The indicator functions never use `quantmod` or any other package.
