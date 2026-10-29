# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R, Development Phase
# Mathew Alleyne (664429889)
#
# run_all.R - loads every indicator file, runs the examples from the
# assignment brief, then applies the indicators to stock data frames
# (one per symbol), filters them and generates buy/sell signals.
#
# HOW TO RUN (from this folder):
#   RStudio: Session > Set Working Directory > To Source File Location, then Source
#   Terminal: Rscript run_all.R
# The indicators use base R only. quantmod is used ONLY to download the price
# data (as in Assignment 2); if it is missing or offline, the saved copy
# stock_data_2025.csv is used instead.
# =============================================================================

# ---- 1. Load the indicator functions (one file per indicator) ---------------
indicator_files <- c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
                     "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")
for (f in indicator_files) source(f)
cat("Loaded:", paste(indicator_files, collapse = ", "), "\n")

show <- function(title, value) {
  cat("\n---", title, "---\n")
  print(value)
}

# ---- 2. Examples from the assignment brief ----------------------------------
cat("\n================ EXAMPLES FROM THE BRIEF ================\n")
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
show("sma(data, period = 3)", round(sma(data, period = 3), 4))
show("ema(data, period = 3)", round(ema(data, period = 3), 4))
show("macd(c(100,...,130), 3, 5, 2)",
     lapply(macd(c(100, 105, 110, 115, 120, 125, 130), short_period = 3,
                 long_period = 5, signal_period = 2), round, 4))
show("stdev(data)", round(stdev(data), 4))
show("linreg(data, regressionLength = 5, regressionOffset = 0)",
     lapply(linreg(data, 5, 0), round, 4))

rsi_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
show("rsi(rsi_data, period = 5)", round(rsi(rsi_data, period = 5), 4))
show("stoch_rsi(rsi_data, period = 14, k_period = 3, d_period = 3)  [brief's example: 10 points < 14]",
     suppressWarnings(stoch_rsi(rsi_data, period = 14, k_period = 3, d_period = 3)))
show("stoch_rsi(rsi_data, period = 3, k_period = 3, d_period = 3)",
     lapply(stoch_rsi(rsi_data, period = 3, k_period = 3, d_period = 3), round, 4))

arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
show("crossover(arr1, arr2)",  crossover(arr1, arr2))
show("crossunder(arr1, arr2)", crossunder(arr1, arr2))
show("crossunder(arr2, arr1)", crossunder(arr2, arr1))

# ---- 3. Stock data frames, one per symbol -----------------------------------
# Same portfolio and period as Assignment 2 (portfolio.txt, Yahoo Finance, 2025)
cat("\n================ STOCK DATA FRAMES ================\n")
cache_file <- "stock_data_2025.csv"
symbols <- unique(toupper(trimws(readLines("portfolio.txt", warn = FALSE))))
symbols <- symbols[symbols != ""]

download_stocks <- function(symbols) {
  if (!requireNamespace("quantmod", quietly = TRUE)) return(NULL)
  frames <- lapply(symbols, function(sym) {
    x <- tryCatch(quantmod::getSymbols(sym, src = "yahoo", from = "2025-01-01",
                                       to = "2025-12-31", auto.assign = FALSE),
                  error = function(e) NULL)
    if (is.null(x)) return(NULL)
    data.frame(Symbol = sym, Date = as.Date(zoo::index(x)),
               Open = round(as.numeric(x[, 1]), 4), High = round(as.numeric(x[, 2]), 4),
               Low = round(as.numeric(x[, 3]), 4), Close = round(as.numeric(x[, 4]), 4),
               Volume = as.numeric(x[, 5]))
  })
  all <- do.call(rbind, frames)
  if (is.null(all) || nrow(all) == 0) NULL else all
}

all_prices <- download_stocks(symbols)
if (!is.null(all_prices)) {
  write.csv(all_prices, cache_file, row.names = FALSE)
  cat("Downloaded from Yahoo Finance and saved to", cache_file, "\n")
} else {
  all_prices <- read.csv(cache_file)
  all_prices$Date <- as.Date(all_prices$Date)
  cat("Offline or quantmod missing - loaded saved copy", cache_file, "\n")
}
stocks <- split(all_prices, all_prices$Symbol)[intersect(symbols, unique(all_prices$Symbol))]
stocks <- lapply(stocks, function(df) { rownames(df) <- NULL; df })
for (s in names(stocks)) {
  cat(s, ":", nrow(stocks[[s]]), "rows,", format(min(stocks[[s]]$Date)), "to",
      format(max(stocks[[s]]$Date)), "\n")
}

# Add indicators to a data frame (vectors padded with NA to the frame length)
pad <- function(x, n) c(rep(NA, n - length(x)), x)
add_indicators <- function(df) {
  n <- nrow(df)
  m <- macd(df$Close, 12, 26, 9)
  df$SMA20       <- pad(sma(df$Close, 20), n)
  df$EMA12       <- ema(df$Close, 12)
  df$EMA26       <- ema(df$Close, 26)
  df$MACD        <- m$macd_line
  df$MACD_signal <- m$signal_line
  df$MACD_hist   <- m$histogram
  df$RSI14       <- rsi(df$Close, 14)
  df$Vol20       <- pad(sapply(20:n, function(i) stdev(df$Close[(i - 19):i])), n)
  df$Slope20     <- pad(sapply(21:n, function(i) linreg(df$Close[1:i], 20, 0)$slope), n)
  df
}
stocks <- lapply(stocks, add_indicators)
show("AAPL - last 5 rows with indicators",
     tail(stocks$AAPL[, c("Date", "Close", "SMA20", "EMA12", "MACD", "RSI14", "Slope20")], 5))

# ---- 4. Filters --------------------------------------------------------------
cat("\n================ FILTERS ================\n")
# Time period filter
filter_period <- function(df, from, to) df[df$Date >= as.Date(from) & df$Date <= as.Date(to), ]
# Time frame filter: daily -> weekly (last trading day of each week)
to_weekly <- function(df) {
  wk <- format(df$Date, "%Y-%W")
  do.call(rbind, lapply(split(df, wk), function(w)
    data.frame(Symbol = w$Symbol[1], Date = max(w$Date), Open = w$Open[1],
               High = max(w$High), Low = min(w$Low), Close = w$Close[nrow(w)],
               Volume = sum(w$Volume))))
}
q2 <- filter_period(stocks$AAPL, "2025-04-01", "2025-06-30")
cat("AAPL rows in Q2 2025:", nrow(q2), "\n")
weekly <- to_weekly(stocks$AAPL[, 1:7])
cat("AAPL weekly bars:", nrow(weekly), " (weekly RSI-14 last value:",
    round(tail(rsi(weekly$Close, 14), 1), 2), ")\n")

# Indicator screen: uptrend (Close > SMA20, positive slope) and RSI not overbought
latest <- do.call(rbind, lapply(stocks, function(df) df[nrow(df), ]))
latest$Passes <- latest$Close > latest$SMA20 & latest$Slope20 > 0 & latest$RSI14 < 70
show("Screen on latest bar: Close > SMA20, Slope20 > 0, RSI14 < 70",
     latest[, c("Symbol", "Close", "SMA20", "Slope20", "RSI14", "Passes")])

# ---- 5. Trading rules --------------------------------------------------------
cat("\n================ TRADING RULES ================\n")
# BUY  when MACD crosses above its signal line and RSI14 < 70
# SELL when MACD crosses below its signal line or RSI14 > 70
trade_signals <- function(df) {
  up   <- crossover(df$MACD, df$MACD_signal) == "Up"
  down <- crossunder(df$MACD, df$MACD_signal) == "True"
  rsi_ok <- !is.na(df$RSI14)
  df$Signal <- "HOLD"
  df$Signal[up & rsi_ok & df$RSI14 < 70] <- "BUY"
  df$Signal[(down | (rsi_ok & df$RSI14 > 70)) & df$Signal != "BUY"] <- "SELL"
  df
}
stocks <- lapply(stocks, trade_signals)
for (s in names(stocks)) {
  df <- stocks[[s]]
  cat(sprintf("%s: %d BUY, %d SELL signals\n", s, sum(df$Signal == "BUY"), sum(df$Signal == "SELL")))
}
show("AAPL - first 6 BUY signals",
     head(stocks$AAPL[stocks$AAPL$Signal == "BUY", c("Date", "Close", "MACD", "MACD_signal", "RSI14")], 6))
