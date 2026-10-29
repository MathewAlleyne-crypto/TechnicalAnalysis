# Exponential Moving Average (EMA)
# Mathew Alleyne (664429889) - BDA400 Assignment 5
# Seeded with the first data point, as in the pseudocode; same length as data.
ema <- function(data, period) {
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (length(data) == 0) stop("data must not be empty")
  if (length(period) != 1 || is.na(period) || period < 1)
    stop("period must be a single positive number")

  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
