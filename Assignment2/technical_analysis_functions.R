# ------------------------------------------------------------------
# technical_analysis_functions.R
# Author:  Mathew Alleyne (Student ID: 664429889)
# Course:  BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R, Preliminary Stage
#
# Purpose: Utility functions to
#   1. read stock symbols from portfolio.txt and import price data
#      with the 'quantmod' package (load_stock_data)
#   2. compute basic statistics: moving average, mean, mode, median
#      and standard deviation (calculate_statistics and helpers)
#   3. display the imported data and statistics (display_* / plot_*)
#
# Required packages: quantmod, TTR
# ------------------------------------------------------------------

library(quantmod)
library(TTR)

# ---- 1. Import -----------------------------------------------------

#' Read stock symbols from a text file (one symbol per line).
read_symbols <- function(portfolio_file = "portfolio.txt") {
  if (!file.exists(portfolio_file)) {
    stop("Portfolio file not found: ", portfolio_file)
  }
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- symbols[symbols != ""]
  unique(toupper(symbols))
}

#' Import stock data for every symbol in the portfolio file.
#' Returns a named list with one data frame per symbol, with columns:
#' Date, Open, High, Low, Close, Volume, Adjusted.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = "2025-01-01",
                            to   = "2025-12-31") {
  symbols    <- read_symbols(portfolio_file)
  stock_list <- list()

  for (sym in symbols) {
    result <- tryCatch({
      xts_data <- quantmod::getSymbols(sym, src = "yahoo",
                                       from = from, to = to,
                                       auto.assign = FALSE)
      df <- data.frame(Date = zoo::index(xts_data), zoo::coredata(xts_data))
      names(df) <- c("Date", "Open", "High", "Low",
                     "Close", "Volume", "Adjusted")
      df
    }, error = function(e) {
      warning("Could not load ", sym, ": ", conditionMessage(e))
      NULL
    })

    if (!is.null(result)) {
      stock_list[[sym]] <- result
    }
  }

  if (length(stock_list) == 0) {
    stop("No stock data could be loaded. Check your internet connection.")
  }
  stock_list
}

# ---- 2. Statistics -------------------------------------------------

#' Statistical mode (most frequent value). Base R has no mode function,
#' so prices are rounded to 2 decimals before counting.
get_mode <- function(x) {
  x  <- round(x[!is.na(x)], 2)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Simple moving average of a numeric vector (window n).
moving_average <- function(x, n = 20) {
  TTR::SMA(x, n = n)
}

#' Calculate basic statistics for one stock data frame.
#' Returns a list with:
#'   $summary        one-row data frame (mean, median, mode, sd, latest MA)
#'   $moving_average numeric vector (same length as the data)
calculate_statistics <- function(df, symbol = "", ma_window = 20) {
  close <- df$Close
  ma    <- moving_average(close, ma_window)

  summary_df <- data.frame(
    Symbol           = symbol,
    Observations     = sum(!is.na(close)),
    Mean             = round(mean(close, na.rm = TRUE), 2),
    Median           = round(median(close, na.rm = TRUE), 2),
    Mode             = get_mode(close),
    Std_Dev          = round(sd(close, na.rm = TRUE), 2),
    Latest_MA        = round(tail(ma[!is.na(ma)], 1), 2),
    stringsAsFactors = FALSE
  )
  names(summary_df)[names(summary_df) == "Latest_MA"] <-
    paste0("Latest_MA", ma_window)

  list(summary = summary_df, moving_average = ma)
}

#' Run calculate_statistics for every stock in the list.
#' Returns a list: $summary (all stocks in one table) and
#' $moving_averages (named list of MA vectors).
calculate_all_statistics <- function(stock_list, ma_window = 20) {
  results <- lapply(names(stock_list), function(sym) {
    calculate_statistics(stock_list[[sym]], symbol = sym,
                         ma_window = ma_window)
  })
  names(results) <- names(stock_list)

  list(
    summary = do.call(rbind, lapply(results, function(r) r$summary)),
    moving_averages = lapply(results, function(r) r$moving_average)
  )
}

# ---- 3. Display ----------------------------------------------------

#' Print the structure, first rows and last rows of each data frame.
display_stock_data <- function(stock_list, n = 5) {
  for (sym in names(stock_list)) {
    df <- stock_list[[sym]]
    cat("\n=====", sym, "=====\n")
    cat("Rows:", nrow(df), "| From", format(min(df$Date)),
        "to", format(max(df$Date)), "\n")
    cat("\nFirst", n, "rows:\n")
    print(head(df, n), row.names = FALSE)
    cat("\nLast", n, "rows:\n")
    print(tail(df, n), row.names = FALSE)
  }
  invisible(stock_list)
}

#' Line chart of closing price with its moving average.
plot_price_with_ma <- function(df, symbol, ma_window = 20) {
  ma <- moving_average(df$Close, ma_window)
  plot(df$Date, df$Close, type = "l", col = "steelblue",
       main = paste(symbol, "- Closing Price and", ma_window, "Day SMA"),
       xlab = "Date", ylab = "Price (USD)")
  lines(df$Date, ma, col = "firebrick", lwd = 2)
  legend("topleft", legend = c("Close", paste(ma_window, "day SMA")),
         col = c("steelblue", "firebrick"), lty = 1, lwd = c(1, 2),
         bty = "n")
}

#' Bar chart of volume.
plot_volume <- function(df, symbol) {
  barplot(df$Volume / 1e6, names.arg = format(df$Date, "%b"),
          col = "grey60", border = NA, las = 2,
          main = paste(symbol, "- Daily Trading Volume"),
          xlab = "Month", ylab = "Volume (millions)")
}

#' Bar chart comparing mean closing price with +/- 1 standard deviation.
plot_statistics_summary <- function(summary_df) {
  mids <- barplot(summary_df$Mean, names.arg = summary_df$Symbol,
                  col = "steelblue",
                  ylim = c(0, max(summary_df$Mean + summary_df$Std_Dev) * 1.1),
                  main = "Mean Closing Price (error bars = 1 SD)",
                  xlab = "Symbol", ylab = "Price (USD)")
  arrows(mids, summary_df$Mean - summary_df$Std_Dev,
         mids, summary_df$Mean + summary_df$Std_Dev,
         angle = 90, code = 3, length = 0.05)
}
