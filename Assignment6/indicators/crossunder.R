# Crossunder signals between two series
# Mathew Alleyne (664429889) - BDA400 Assignment 5
# Returns "True" where arr1 crosses below arr2, otherwise "False"; the first
# element is "None" (labels as given in the assignment pseudocode).
crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("False", length(arr1))
  if (length(arr1) > 0) crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point
  for (i in seq_along(arr1)[-1]) {
    if (anyNA(c(arr1[i], arr2[i], arr1[i - 1], arr2[i - 1]))) next
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "True"
    }
  }

  return(crossunder_signals)
}
