# =============================================================================
# make_screenshots.R - renders example dashboard charts to screenshots/ and
# prints the signal / back-test results, without starting the Shiny app.
# Mathew Alleyne (664429889) - BDA400 Assignment 6
# Run from the Assignment6 folder:  Rscript make_screenshots.R
# =============================================================================
source("app.R")   # defines all helpers; shinyApp() is created but not run

defaults <- list(ma_short = 20, ma_long = 50, ema_period = 12, bb_period = 20, bb_sd = 2,
                 rsi_period = 14, rsi_low = 30, rsi_high = 70,
                 macd_fast = 12, macd_slow = 26, macd_signal = 9)

examples <- list(
  list(file = "01_AAPL_candlestick_daily_MA_rule.png", symbol = "AAPL",
       range = c("2025-01-01", "2025-12-31"), tf = "Daily", type = "Candlestick", rule = "ma",
       ind = c("Moving Averages", "RSI", "MACD")),
  list(file = "02_MSFT_line_weekly_combined_rule.png", symbol = "MSFT",
       range = c("2025-01-01", "2025-12-31"), tf = "Weekly", type = "Line", rule = "combined",
       ind = c("EMA", "Bollinger Bands", "Volume", "RSI")),
  list(file = "03_TSLA_area_daily_RSI_rule.png", symbol = "TSLA",
       range = c("2025-01-01", "2025-12-31"), tf = "Daily", type = "Area", rule = "rsi",
       ind = c("Trend line (linreg)", "RSI")),
  list(file = "04_GOOGL_candlestick_daily_MACD_rule_no_overlays.png", symbol = "GOOGL",
       range = c("2025-09-01", "2025-12-31"), tf = "Daily", type = "Candlestick", rule = "macd",
       ind = character(0))
)

for (ex in examples) {
  res <- prepare_data(ex$symbol, ex$range, ex$tf, ex$rule, defaults)
  if (is.null(res$data)) { cat(ex$symbol, "FAILED:", res$note, "\n"); next }
  d <- res$df
  chart <- build_chart(d, ex$symbol, list(indicators = ex$ind, chart_type = ex$type,
                                          time_frame = ex$tf, rule = ex$rule, show_signals = TRUE),
                       defaults)
  n_panels <- 1 + sum(c("RSI", "MACD", "Volume") %in% ex$ind)
  ggsave(file.path("screenshots", ex$file), chart, width = 11, height = 4 + 1.6 * n_panels, dpi = 110)
  bt <- backtest(d)
  cat(sprintf("%-6s %-11s %-7s rule=%-8s bars=%3d  Buy=%2d Sell=%2d  strategy=%+6.1f%%  buy&hold=%+6.1f%%  [%s]\n",
              ex$symbol, ex$type, ex$tf, ex$rule, nrow(d), sum(d$Signal == "Buy"),
              sum(d$Signal == "Sell"), 100 * bt$strategy, 100 * bt$buy_hold, res$source))
}

# Check: signals sit exactly on the bars where the short SMA crosses the long SMA
res <- prepare_data("AAPL", c("2025-01-01", "2025-12-31"), "Daily", "ma", defaults)
d <- res$df
above <- d$SMA_short > d$SMA_long
crosses_up   <- which(above & !c(NA, head(above, -1)))
crosses_down <- which(!above & c(NA, head(above, -1)))
cat("\nAAPL MA rule check - Buy bars match SMA20 crossing above SMA50:",
    identical(which(d$Signal == "Buy"), crosses_up), "\n")
cat("AAPL MA rule check - Sell bars match SMA20 crossing below SMA50:",
    identical(which(d$Signal == "Sell"), crosses_down), "\n")
print(d[d$Signal != "Hold", c("Date", "Signal", "Close", "SMA_short", "SMA_long")], row.names = FALSE)
