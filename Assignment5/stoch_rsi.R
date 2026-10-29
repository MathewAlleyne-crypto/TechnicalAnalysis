# Stochastic RSI (StochRSI)
# Mathew Alleyne (664429889) - BDA400 Assignment 5
# Uses rsi() and sma() - source rsi.R and sma.R first (run_all.R does this).
# As in the pseudocode, RSI is normalised with the min/max of the whole RSI
# series, %K = SMA(StochRSI, k_period) and %D = SMA(%K, d_period).
if (!exists("rsi", mode = "function") && file.exists("rsi.R")) source("rsi.R")
if (!exists("sma", mode = "function") && file.exists("sma.R")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  rsi_values <- rsi_values[!is.na(rsi_values)]      # drop the RSI warm-up NAs
  if (length(rsi_values) < k_period + d_period - 1) {
    warning("not enough RSI values for k_period and d_period. Returning NA.")
    return(list(k_line = NA_real_, d_line = NA_real_))
  }

  # Calculate the StochRSI
  min_rsi <- min(rsi_values)
  max_rsi <- max(rsi_values)
  if (max_rsi == min_rsi) {
    k_values <- rep(0.5, length(rsi_values))         # flat RSI: mid-scale
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (3-day simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
