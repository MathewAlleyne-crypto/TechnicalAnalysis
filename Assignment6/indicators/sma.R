# Simple Moving Average (SMA)
# Mathew Alleyne (664429889) - BDA400 Assignment 5
# Returns one average per complete window: length(data) - period + 1 values.
sma <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (length(period) != 1 || is.na(period) || period < 1 || period != round(period))
    stop("period must be a single positive integer")

  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  n_windows <- length(data) - period + 1
  sma_values <- numeric(n_windows)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_windows) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}
