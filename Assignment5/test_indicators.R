# =============================================================================
# BDA400 Assignment 5 - test_indicators.R
# Mathew Alleyne (664429889)
# Checks every indicator against hand-calculated values and an independent
# calculation (base R built-ins such as sd() and lm(), or a different
# algorithm). The built-ins are used ONLY here, never in the indicator files.
# Run from this folder:  Rscript test_indicators.R
# =============================================================================
for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R", "rsi.R",
            "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)

passed <- 0; failed <- 0
check <- function(label, ok) {
  ok <- isTRUE(ok)
  cat(sprintf("  [%s] %s\n", if (ok) "PASS" else "FAIL", label))
  if (ok) passed <<- passed + 1 else failed <<- failed + 1
}
throws <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
near <- function(a, b, tol = 1e-8) isTRUE(all.equal(a, b, tolerance = tol))

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
set.seed(5)
prices <- 100 * cumprod(1 + rnorm(300, 0, 0.02))       # synthetic price series

cat("\nSMA\n")
check("brief example (hand calc): 12.333 15.667 17.667 20 21.667 23.667 23.333",
      near(sma(data, 3), c(37, 47, 53, 60, 65, 71, 70) / 3))
check("length = n - period + 1", length(sma(prices, 20)) == 281)
check("matches stats::filter() moving average",
      near(sma(prices, 20), as.numeric(na.omit(stats::filter(prices, rep(1 / 20, 20), sides = 1)))))
check("period = length gives the overall mean", near(sma(data, 9), mean(data)))
check("error when data shorter than period", throws(sma(1:3, 5)))

cat("\nEMA\n")
ema_ref <- function(x, p) Reduce(function(prev, v) prev + 2 / (p + 1) * (v - prev), x, accumulate = TRUE)
check("brief example (hand calc first 3): 10, 11, 13", near(ema(data, 3)[1:3], c(10, 11, 13)))
check("matches Reduce() recursion on 300 prices", near(ema(prices, 12), ema_ref(prices, 12)))
check("same length as input", length(ema(prices, 12)) == 300)
check("constant series stays constant", near(ema(rep(5, 10), 4), rep(5, 10)))

cat("\nMACD\n")
m <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2)
check("macd_line = ema(3) - ema(5)",
      near(m$macd_line, ema_ref(c(100, 105, 110, 115, 120, 125, 130), 3) -
                        ema_ref(c(100, 105, 110, 115, 120, 125, 130), 5)))
check("hand calc: macd_line[2] = 102.5 - 101.6667 = 0.8333", near(m$macd_line[2], 2.5 - 5 / 3))
check("histogram = macd_line - signal_line", near(m$histogram, m$macd_line - m$signal_line))
check("returns list(macd_line, signal_line, histogram)",
      identical(names(m), c("macd_line", "signal_line", "histogram")))
check("rising prices -> positive MACD after the start", all(m$macd_line[-1] > 0))
check("error when short_period >= long_period", throws(macd(prices, 26, 12, 9)))

cat("\nSTDEV (population)\n")
# Hand calc: mean = 167/9, sum of squared deviations = 1982/9, variance = 1982/81
check("brief example: sqrt(1982/81) = 4.946629 (Sheets: STDEVP)", near(stdev(data), sqrt(1982 / 81)))
check("equals sd() * sqrt((n-1)/n)", near(stdev(prices), sd(prices) * sqrt(299 / 300)))
check("constant data -> 0", stdev(rep(3, 5)) == 0)
check("error on empty input", throws(stdev(numeric(0))))

cat("\nLINREG\n")
lr <- linreg(data, 5, 0)
sub <- data[(9 - 5):9]                                 # pseudocode window: indices 4..9
fit <- lm(sub ~ seq_along(sub))
check("window follows pseudocode (offset 0 -> 6 points: 20 18 22 25 24 21)",
      length(lr$predicted_values) == 6)
check("slope matches lm()", near(lr$slope, unname(coef(fit)[2])))
check("intercept matches lm()", near(lr$intercept, unname(coef(fit)[1])))
check("predicted values match lm() fitted values", near(lr$predicted_values, unname(fitted(fit))))
check("perfect line y = 2x + 1 recovered", near(linreg(2 * (1:10) + 1, 9, 0)$slope, 2))
check("offset shifts window back (offset 2 -> ends at n - 2)",
      near(linreg(1:20 * 1.0, 10, 2)$predicted_values[length(linreg(1:20 * 1.0, 10, 2)$predicted_values)], 18))
check("error when regressionLength > n", throws(linreg(data, 20, 0)))
check("error when regressionOffset >= regressionLength", throws(linreg(data, 5, 5)))

cat("\nRSI\n")
rsi_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
r <- rsi(rsi_data, 5)
# Hand calc (period 5): changes 5,-2,7,-3,-3 | 9,2,5,-3
# avg_gain0 = 12/5 = 2.4, avg_loss0 = 8/5 = 1.6
# i = 6: gain[5]=0, loss[5]=3 -> g = 2.4*4/5 = 1.92, l = (1.6*4+3)/5 = 1.88 -> RSI = 50.5263
check("first 5 values are NA", all(is.na(r[1:5])))
check("hand calc RSI[6] = 50.5263", near(r[6], 100 - 100 / (1 + 1.92 / 1.88)))
rsi_ref <- function(x, p) {           # independent vectorised version
  d <- diff(x); g <- pmax(d, 0); l <- pmax(-d, 0)
  ag <- mean(g[1:p]); al <- mean(l[1:p]); out <- rep(NA, length(x))
  for (i in (p + 1):length(x)) {
    ag <- (ag * (p - 1) + g[i - 1]) / p; al <- (al * (p - 1) + l[i - 1]) / p
    out[i] <- 100 * ag / (ag + al)
  }
  out
}
check("matches independent RSI = 100*avgGain/(avgGain+avgLoss) on 300 prices",
      near(rsi(prices, 14), rsi_ref(prices, 14)))
check("values within 0-100", all(rsi(prices, 14) >= 0 & rsi(prices, 14) <= 100, na.rm = TRUE))
check("only rising prices -> RSI = 100", all(rsi(1:30, 14)[15:30] == 100))
check("too little data -> all NA with a warning",
      all(is.na(suppressWarnings(rsi(1:5, 14)))) && inherits(tryCatch(rsi(1:5, 14), warning = function(w) w), "warning"))

cat("\nSTOCH RSI\n")
s <- stoch_rsi(prices, 14, 3, 3)
rv <- rsi(prices, 14); rv <- rv[!is.na(rv)]
kv <- (rv - min(rv)) / (max(rv) - min(rv))
check("k_line = SMA3 of normalised RSI", near(s$k_line, sma(kv, 3)))
check("d_line = SMA3 of k_line", near(s$d_line, sma(s$k_line, 3)))
check("values between 0 and 1", all(c(s$k_line, s$d_line) >= 0 & c(s$k_line, s$d_line) <= 1))
check("lengths: k = 286 - 2 = 284, d = 282", length(s$k_line) == 284 && length(s$d_line) == 282)
check("brief example (10 points, period 14) returns NA without crashing",
      all(is.na(unlist(suppressWarnings(stoch_rsi(rsi_data, 14, 3, 3))))))

cat("\nCROSSOVER / CROSSUNDER\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
check("crossover brief example: Up at point 4 only",
      identical(crossover(arr1, arr2), c("None", "None", "None", "Up", rep("None", 5))))
check("crossover detects Down", identical(crossover(arr2, arr1)[4], "Down"))
check("crossunder brief example: no crossunder",
      identical(crossunder(arr1, arr2), c("None", rep("False", 8))))
check("crossunder(arr2, arr1): True at point 4 only",
      identical(which(crossunder(arr2, arr1) == "True"), 4L))
check("touching then crossing counts once", identical(crossover(c(1, 2, 3), c(2, 2, 2)), c("None", "None", "Up")))
check("NA values give no signal", identical(crossover(c(NA, 1, 3), c(2, 2, 2)), c("None", "None", "Up")))
check("error on different lengths", throws(crossover(1:3, 1:4)) && throws(crossunder(1:3, 1:4)))
check("agrees with vectorised check on EMA12/EMA26 of 300 prices",
      identical(which(crossover(ema(prices, 12), ema(prices, 26)) == "Up"),
                which(diff(sign(ema(prices, 12) - ema(prices, 26))) > 0 &
                      (ema(prices, 12) - ema(prices, 26))[-1] > 0) + 1L))

cat(sprintf("\nRESULT: %d passed, %d failed\n", passed, failed))
