# Crossover signals between two series
# Mathew Alleyne (664429889) - BDA400 Assignment 5
# Returns "Up" where arr1 crosses above arr2, "Down" where it crosses below,
# otherwise "None" (labels as given in the assignment pseudocode).
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep("None", length(arr1))

  # Check for crossovers at each data point
  for (i in seq_along(arr1)[-1]) {
    if (anyNA(c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1]))) next
    if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      crossover_signals[i] <- "Up"
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossover_signals[i] <- "Down"
    }
  }

  return(crossover_signals)
}
