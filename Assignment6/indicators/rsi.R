# Relative Strength Index (RSI) with Wilder's smoothing
# Mathew Alleyne (664429889) - BDA400 Assignment 5
# Follows the assignment pseudocode: the first averages use the first 'period'
# price changes, then Wilder smoothing runs from data point period + 1 onward.
# Output has the same length as data; the first 'period' values are NA.
rsi <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (length(period) != 1 || is.na(period) || period < 1 || period != round(period))
    stop("period must be a single positive integer")

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))
  if (length(data) <= period) {
    warning("data has ", length(data), " points; RSI needs more than period = ",
            period, ". Returning NA.")
    return(rsi_values)
  }

  # Calculate the differences between consecutive data points
  diff_values <- data[-1] - data[-length(data)]

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0) {
      rsi_values[i] <- if (avg_gain == 0) 50 else 100   # flat, or only rising
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
