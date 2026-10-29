# Moving Average Convergence Divergence (MACD)
# Mathew Alleyne (664429889) - BDA400 Assignment 5
# Uses ema() from ema.R - source ema.R first (run_all.R does this for you).
if (!exists("ema", mode = "function") && file.exists("ema.R")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  if (short_period >= long_period) stop("short_period must be less than long_period")

  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(macd_line = macd_line, signal_line = signal_line, histogram = histogram)
  return(result)
}
