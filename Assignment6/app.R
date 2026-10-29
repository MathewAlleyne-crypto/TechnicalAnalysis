# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Author:     Mathew Alleyne (Student ID: 664429889)
# Repository: https://github.com/MathewAlleyne-crypto/TechnicalAnalysis/tree/main/Assignment6
#
# Portfolio dashboard built with R Shiny. It fetches stock prices from Yahoo
# Finance, draws line / candlestick / area charts, overlays the technical
# indicators written in Assignment 5 (switchable on and off), applies a
# choice of trading rules and annotates the Buy / Sell bars on the chart.
#
# HOW TO RUN: open this file in RStudio and click "Run App", or from R:
#   shiny::runApp("Assignment6")
# =============================================================================


# =============================================================================
# STEP 1 - DATA COLLECTION AND SETUP
# =============================================================================

# ---- 1a. Install (first time only) and load the packages --------------------
required_packages <- c("shiny", "ggplot2", "quantmod", "patchwork", "bslib")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(shiny)
library(ggplot2)
library(quantmod)
library(patchwork)   # stacks the price, RSI, MACD and volume panels

# ---- 1b. Indicator functions from Assignment 5 (base R, one file each) ------
indicator_files <- c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
                     "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")
for (f in indicator_files) source(file.path("indicators", f), local = TRUE)

# ---- 1c. Data source: Yahoo Finance, with a local fallback ------------------
# Primary source: Yahoo Finance through quantmod::getSymbols().
# Fallback source: stock_data_2025.csv (2025 prices saved in Assignment 5), used
# when Yahoo cannot be reached, so the dashboard still works offline.
portfolio_symbols <- unique(toupper(trimws(readLines("portfolio.txt", warn = FALSE))))
portfolio_symbols <- portfolio_symbols[portfolio_symbols != ""]
fallback_file <- "stock_data_2025.csv"

price_cache <- new.env()              # avoids downloading the same data twice

fetch_stock <- function(symbol, from, to) {
  key <- paste(symbol, from, to, sep = "|")
  if (!is.null(price_cache[[key]])) return(price_cache[[key]])

  result <- tryCatch({
    x <- getSymbols(symbol, src = "yahoo", from = from, to = to + 1,
                    auto.assign = FALSE, warnings = FALSE)
    x <- na.omit(x[, 1:5])
    colnames(x) <- c("Open", "High", "Low", "Close", "Volume")
    if (nrow(x) == 0) stop("no rows returned")
    list(data = x, source = "Yahoo Finance", note = NULL)
  }, error = function(e) {
    # Fallback: saved CSV (only covers the Assignment 2 portfolio for 2025)
    if (file.exists(fallback_file)) {
      csv <- read.csv(fallback_file)
      csv <- csv[csv$Symbol == symbol, ]
      csv$Date <- as.Date(csv$Date)
      csv <- csv[csv$Date >= from & csv$Date <= to, ]
      if (nrow(csv) > 0) {
        x <- xts(csv[, c("Open", "High", "Low", "Close", "Volume")], order.by = csv$Date)
        return(list(data = x, source = "Saved CSV (stock_data_2025.csv)",
                    note = paste("Yahoo Finance unavailable:", conditionMessage(e))))
      }
    }
    list(data = NULL, source = NA,
         note = paste0("Could not load ", symbol, ": ", conditionMessage(e)))
  })

  if (!is.null(result$data)) price_cache[[key]] <- result
  result
}

# Example from the brief: fetch one symbol for a given period
# stock_data <- getSymbols("AAPL", src = "yahoo", from = "2025-01-01",
#                          to = "2025-12-31", auto.assign = FALSE)


# =============================================================================
# STEP 2 - HELPERS FOR VISUALIZING STOCK DATA
# =============================================================================

# Convert daily bars to the selected time frame (Daily / Weekly / Monthly)
to_timeframe <- function(x, time_frame) {
  if (time_frame == "Daily") return(x)
  period <- if (time_frame == "Weekly") "weeks" else "months"
  y <- to.period(x, period = period, OHLC = TRUE, indexAt = "endof")
  colnames(y) <- c("Open", "High", "Low", "Close", "Volume")
  y
}

# xts -> data frame with a Date column (what ggplot2 needs)
as_frame <- function(x) {
  data.frame(Date = as.Date(index(x)), coredata(x), row.names = NULL)
}


# =============================================================================
# STEP 3 - TECHNICAL INDICATORS (computed with the Assignment 5 functions)
# =============================================================================
pad <- function(v, n) c(rep(NA_real_, n - length(v)), v)   # align shorter outputs

add_indicators <- function(df, p) {
  n  <- nrow(df)
  cl <- df$Close
  safe_sma <- function(k) if (n >= k) pad(sma(cl, k), n) else rep(NA_real_, n)

  df$SMA_short <- safe_sma(p$ma_short)
  df$SMA_long  <- safe_sma(p$ma_long)
  df$EMA       <- ema(cl, p$ema_period)

  # Bollinger Bands: SMA +/- k standard deviations over the same window
  k <- p$bb_period
  if (n >= k) {
    roll_sd   <- pad(vapply(k:n, function(i) stdev(cl[(i - k + 1):i]), numeric(1)), n)
    df$BB_mid <- pad(sma(cl, k), n)
    df$BB_up  <- df$BB_mid + p$bb_sd * roll_sd
    df$BB_low <- df$BB_mid - p$bb_sd * roll_sd
  } else {
    df$BB_mid <- df$BB_up <- df$BB_low <- NA_real_
  }

  df$RSI <- suppressWarnings(rsi(cl, p$rsi_period))
  m <- macd(cl, p$macd_fast, p$macd_slow, p$macd_signal)
  df$MACD        <- m$macd_line
  df$MACD_signal <- m$signal_line
  df$MACD_hist   <- m$histogram
  df
}


# =============================================================================
# STEP 4 - TRADING RULES
# =============================================================================
# Every rule marks a bar "Buy" or "Sell" on the bar where its condition starts,
# using the crossover() / crossunder() functions from Assignment 5; all other
# bars are "Hold".
trading_rules <- c(
  "Moving Average crossover (short vs long SMA)" = "ma",
  "RSI leaves oversold / overbought zone"        = "rsi",
  "MACD crosses its signal line"                 = "macd",
  "Combined: MACD cross + RSI filter + trend"    = "combined"
)

generate_signals <- function(df, rule, p) {
  n <- nrow(df)
  is_up   <- function(a, b) crossover(a, b) == "Up"     # a crosses above b
  is_down <- function(a, b) crossunder(a, b) == "True"  # a crosses below b
  rsi_low  <- rep(p$rsi_low, n)
  rsi_high <- rep(p$rsi_high, n)

  if (rule == "ma") {
    buy  <- is_up(df$SMA_short, df$SMA_long)
    sell <- is_down(df$SMA_short, df$SMA_long)
  } else if (rule == "rsi") {
    buy  <- is_up(df$RSI, rsi_low)        # RSI climbs back above the oversold level
    sell <- is_down(df$RSI, rsi_high)     # RSI falls back below the overbought level
  } else if (rule == "macd") {
    buy  <- is_up(df$MACD, df$MACD_signal)
    sell <- is_down(df$MACD, df$MACD_signal)
  } else {                                # combined rule
    trend_up <- !is.na(df$SMA_long) & df$Close > df$SMA_long
    rsi_ok   <- !is.na(df$RSI) & df$RSI < p$rsi_high
    buy  <- is_up(df$MACD, df$MACD_signal) & rsi_ok & trend_up
    sell <- is_down(df$MACD, df$MACD_signal) | is_down(df$RSI, rsi_high)
  }

  signal <- rep("Hold", n)
  signal[buy] <- "Buy"
  signal[sell & !buy] <- "Sell"

  # Keep signals alternating (Buy, Sell, Buy, ...): a repeat of the previous
  # signal adds no information, so it is turned back into Hold.
  last <- ""
  for (i in which(signal != "Hold")) {
    if (signal[i] == last) signal[i] <- "Hold" else last <- signal[i]
  }
  signal
}

# Percentage text such as "+12.3%" (the + 0 turns a negative zero into 0)
fmt_pct <- function(x) sprintf("%+.1f%%", round(100 * x, 1) + 0)

# Simple long-only back-test: buy on "Buy", sell on "Sell", one position at a time
backtest <- function(d) {
  in_trade <- FALSE; entry <- NA; returns <- numeric(0)
  for (i in seq_len(nrow(d))) {
    if (d$Signal[i] == "Buy" && !in_trade) {
      in_trade <- TRUE; entry <- d$Close[i]
    } else if (d$Signal[i] == "Sell" && in_trade) {
      in_trade <- FALSE; returns <- c(returns, d$Close[i] / entry - 1)
    }
  }
  if (in_trade) returns <- c(returns, d$Close[nrow(d)] / entry - 1)  # open trade
  list(trades = length(returns),
       strategy = if (length(returns)) prod(1 + returns) - 1 else 0,
       buy_hold = d$Close[nrow(d)] / d$Close[1] - 1,
       win_rate = if (length(returns)) mean(returns > 0) else NA_real_)
}


# =============================================================================
# CHART BUILDER (price panel + optional indicator panels)
# =============================================================================
col_up <- "#1a9850"; col_down <- "#d73027"
overlay_colours <- c("SMA short" = "#2166ac", "SMA long" = "#e08214",
                     "EMA" = "#7b3294", "Bollinger" = "#878787", "Trend line" = "#000000")

build_chart <- function(d, symbol, opts, p) {
  ind  <- opts$indicators
  # Bars are plotted side by side by position (no weekend / holiday gaps);
  # the axis labels show the matching dates.
  n    <- nrow(d)
  d$X  <- seq_len(n)
  xlim <- c(1, n)
  step <- 1
  brk  <- unique(round(seq(1, n, length.out = min(n, 8))))
  date_fmt <- if (diff(range(d$Date)) > 300) "%b %Y" else "%d %b"
  base_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "top",
          plot.title = element_text(face = "bold"),
          plot.margin = margin(5.5, 22, 5.5, 5.5))   # room for the last date label
  x_scale <- scale_x_continuous(limits = xlim + c(-step, step), expand = c(0, 0),
                                breaks = brk, labels = format(d$Date[brk], date_fmt))

  # ---- price panel: chart type ------------------------------------------
  pr <- ggplot(d, aes(x = X))
  if (opts$chart_type == "Candlestick") {
    d$Direction <- ifelse(d$Close >= d$Open, "Up", "Down")
    half <- step * 0.35
    pr <- pr +
      geom_segment(data = d, aes(xend = X, y = Low, yend = High, colour = I(ifelse(Direction == "Up", col_up, col_down)))) +
      geom_rect(data = d, aes(xmin = X - half, xmax = X + half,
                              ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                              fill = I(ifelse(Direction == "Up", col_up, col_down))))
  } else if (opts$chart_type == "Area") {
    floor_y <- min(d$Low, na.rm = TRUE) * 0.97
    pr <- pr + geom_ribbon(aes(ymin = floor_y, ymax = Close), fill = "#4393c3", alpha = 0.35) +
      geom_line(aes(y = Close), colour = "#2166ac", linewidth = 0.7)
  } else {
    pr <- pr + geom_line(aes(y = Close), colour = "#2166ac", linewidth = 0.8)
  }

  # ---- overlays that sit on the price panel ------------------------------
  used <- character(0)
  if ("Bollinger Bands" %in% ind) {
    pr <- pr + geom_ribbon(aes(ymin = BB_low, ymax = BB_up), fill = "grey60", alpha = 0.15, na.rm = TRUE) +
      geom_line(aes(y = BB_up, colour = "Bollinger"), linetype = "dashed", linewidth = 0.4, na.rm = TRUE) +
      geom_line(aes(y = BB_low, colour = "Bollinger"), linetype = "dashed", linewidth = 0.4, na.rm = TRUE)
    used <- c(used, "Bollinger")
  }
  if ("Moving Averages" %in% ind) {
    pr <- pr + geom_line(aes(y = SMA_short, colour = "SMA short"), linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = SMA_long, colour = "SMA long"), linewidth = 0.8, na.rm = TRUE)
    used <- c(used, "SMA short", "SMA long")
  }
  if ("EMA" %in% ind) {
    pr <- pr + geom_line(aes(y = EMA, colour = "EMA"), linewidth = 0.7, na.rm = TRUE)
    used <- c(used, "EMA")
  }
  if ("Trend line (linreg)" %in% ind && nrow(d) >= 3) {
    fit <- linreg(d$Close, nrow(d) - 1, 0)          # regression over the visible bars
    d$Trend <- fit$predicted_values
    pr <- pr + geom_line(data = d, aes(y = Trend, colour = "Trend line"), linetype = "longdash", linewidth = 0.6)
    used <- c(used, "Trend line")
  }
  labels <- c("SMA short" = paste0("SMA ", p$ma_short), "SMA long" = paste0("SMA ", p$ma_long),
              "EMA" = paste0("EMA ", p$ema_period), "Bollinger" = paste0("Bollinger ", p$bb_period, ", ", p$bb_sd, " SD"),
              "Trend line" = "Linear trend")
  if (length(used)) {
    pr <- pr + scale_colour_manual(values = overlay_colours[used], labels = labels[used],
                                   breaks = used, name = NULL)
  }

  # ---- signal annotations on the bars ------------------------------------
  if (opts$show_signals) {
    span <- diff(range(c(d$Low, d$High), na.rm = TRUE))
    buys  <- d[d$Signal == "Buy", ]
    sells <- d[d$Signal == "Sell", ]
    if (nrow(buys)) {
      pr <- pr + geom_point(data = buys, aes(y = Low - 0.02 * span), shape = 24, size = 3,
                            fill = col_up, colour = "white") +
        geom_label(data = buys, aes(y = Low - 0.07 * span, label = "BUY"), colour = col_up,
                   size = 3, fontface = "bold", linewidth = 0.2)
    }
    if (nrow(sells)) {
      pr <- pr + geom_point(data = sells, aes(y = High + 0.02 * span), shape = 25, size = 3,
                            fill = col_down, colour = "white") +
        geom_label(data = sells, aes(y = High + 0.07 * span, label = "SELL"), colour = col_down,
                   size = 3, fontface = "bold", linewidth = 0.2)
    }
  }
  pr <- pr + x_scale + base_theme +
    labs(title = paste0(symbol, " - ", opts$chart_type, " chart (", opts$time_frame, ")"),
         subtitle = paste("Rule:", names(trading_rules)[trading_rules == opts$rule]),
         x = NULL, y = "Price (USD)")

  panels <- list(pr); heights <- 3

  # ---- separate indicator panels ----------------------------------------
  if ("Volume" %in% ind) {
    d$VolDir <- ifelse(d$Close >= d$Open, col_up, col_down)
    pv <- ggplot(d, aes(X, Volume / 1e6)) + geom_col(aes(fill = I(VolDir)), alpha = 0.7) +
      x_scale + base_theme + labs(x = NULL, y = "Volume (M)")
    panels <- c(panels, list(pv)); heights <- c(heights, 0.8)
  }
  if ("RSI" %in% ind) {
    prsi <- ggplot(d, aes(X, RSI)) +
      annotate("rect", xmin = xlim[1], xmax = xlim[2], ymin = p$rsi_low, ymax = p$rsi_high,
               fill = "#9ecae1", alpha = 0.15) +
      geom_hline(yintercept = c(p$rsi_low, p$rsi_high), linetype = "dashed", colour = "grey40") +
      geom_line(colour = "#7b3294", linewidth = 0.7, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, p$rsi_low, 50, p$rsi_high, 100)) +
      x_scale + base_theme + labs(x = NULL, y = paste0("RSI ", p$rsi_period))
    panels <- c(panels, list(prsi)); heights <- c(heights, 1)
  }
  if ("MACD" %in% ind) {
    pm <- ggplot(d, aes(X)) +
      geom_col(aes(y = MACD_hist, fill = I(ifelse(MACD_hist >= 0, col_up, col_down))), alpha = 0.6, na.rm = TRUE) +
      geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.7) +
      geom_line(aes(y = MACD_signal, colour = "Signal"), linewidth = 0.7) +
      geom_hline(yintercept = 0, colour = "grey50") +
      scale_colour_manual(values = c(MACD = "#2166ac", Signal = "#e08214"), name = NULL) +
      x_scale + base_theme + theme(legend.position = "right") +
      labs(x = NULL, y = paste0("MACD ", p$macd_fast, "/", p$macd_slow, "/", p$macd_signal))
    panels <- c(panels, list(pm)); heights <- c(heights, 1)
  }

  wrap_plots(panels, ncol = 1, heights = heights)
}

# Full pipeline for one symbol: fetch -> time frame -> indicators -> signals -> window
prepare_data <- function(symbol, date_range, time_frame, rule, p) {
  from <- as.Date(date_range[1]); to <- as.Date(date_range[2])
  warm_up <- if (time_frame == "Monthly") 1500 else if (time_frame == "Weekly") 500 else 200
  fetched <- fetch_stock(symbol, from - warm_up, to)     # extra history so indicators
  if (is.null(fetched$data)) return(fetched)              # are ready at the start date
  df <- as_frame(to_timeframe(fetched$data, time_frame))
  df <- add_indicators(df, p)
  df$Signal <- generate_signals(df, rule, p)
  fetched$df <- df[df$Date >= from & df$Date <= to, ]
  fetched
}


# =============================================================================
# STEP 2 (continued) - SHINY USER INTERFACE
# =============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Portfolio Dashboard - Technical Analysis (BDA400, Mathew Alleyne)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("symbol", "Stock symbol:", choices = portfolio_symbols,
                     selected = portfolio_symbols[1],
                     options = list(create = TRUE, placeholder = "Type any Yahoo ticker")),
      dateRangeInput("date_range", "Select Date Range:", start = "2025-01-01", end = "2025-12-31"),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      radioButtons("chart_type", "Chart type:", choices = c("Candlestick", "Line", "Area"), inline = TRUE),
      checkboxGroupInput("technical_indicators", "Technical indicators (on/off):",
                         choices = c("Moving Averages", "EMA", "Bollinger Bands",
                                     "Trend line (linreg)", "RSI", "MACD", "Volume"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      selectInput("rule", "Trading rule:", choices = trading_rules),
      checkboxInput("show_signals", "Annotate Buy / Sell bars", value = TRUE),
      tags$details(
        tags$summary(strong("Indicator & rule parameters")),
        sliderInput("ma_short", "Short SMA period", 5, 100, 20),
        sliderInput("ma_long", "Long SMA period", 20, 250, 50),
        sliderInput("ema_period", "EMA period", 5, 100, 12),
        sliderInput("bb_period", "Bollinger period", 10, 50, 20),
        sliderInput("bb_sd", "Bollinger width (SD)", 1, 3, 2, step = 0.5),
        sliderInput("rsi_period", "RSI period", 5, 30, 14),
        sliderInput("rsi_levels", "RSI oversold / overbought", 10, 90, c(30, 70)),
        sliderInput("macd_fast", "MACD fast EMA", 5, 20, 12),
        sliderInput("macd_slow", "MACD slow EMA", 20, 50, 26),
        sliderInput("macd_signal", "MACD signal EMA", 3, 15, 9)
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Chart",
                 uiOutput("status"),
                 plotOutput("stock_chart", height = "760px")),
        tabPanel("Signals",
                 br(), uiOutput("backtest_summary"), br(),
                 tableOutput("signal_table")),
        tabPanel("Portfolio overview",
                 br(), p("Latest values for every symbol in portfolio.txt, using the selected date range, time frame and trading rule."),
                 tableOutput("portfolio_table")),
        tabPanel("Data",
                 br(), tableOutput("data_table")),
        tabPanel("About",
                 br(),
                 p("Data: Yahoo Finance via quantmod::getSymbols(); falls back to stock_data_2025.csv when offline."),
                 p("Indicators: SMA, EMA, MACD, standard deviation (Bollinger Bands), linear regression,",
                   "RSI and crossover/crossunder are the base-R functions from Assignment 5 (folder 'indicators')."),
                 p("Signals: a Buy or Sell label marks the bar where the selected rule's condition starts;",
                   "all other bars are Hold. The back-test is a simple long-only illustration, not investment advice."))
      )
    )
  )
)


# =============================================================================
# STEPS 2-4 - SHINY SERVER
# =============================================================================
server <- function(input, output, session) {

  params <- reactive({
    validate(need(input$ma_short < input$ma_long, "Short SMA period must be smaller than the long SMA period."),
             need(input$macd_fast < input$macd_slow, "MACD fast period must be smaller than the slow period."),
             need(input$date_range[1] < input$date_range[2], "Start date must be before end date."))
    list(ma_short = input$ma_short, ma_long = input$ma_long, ema_period = input$ema_period,
         bb_period = input$bb_period, bb_sd = input$bb_sd,
         rsi_period = input$rsi_period, rsi_low = input$rsi_levels[1], rsi_high = input$rsi_levels[2],
         macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal)
  })

  # Step 1 + 3 + 4: fetch, filter by date range / time frame, add indicators and signals
  chart_data <- reactive({
    req(input$symbol)
    withProgress(message = paste("Loading", toupper(input$symbol)), {
      prepare_data(toupper(trimws(input$symbol)), input$date_range, input$time_frame,
                   input$rule, params())
    })
  })

  output$status <- renderUI({
    res <- chart_data()
    if (is.null(res$data)) return(div(class = "alert alert-danger", res$note))
    d <- res$df
    msgs <- paste0("Source: ", res$source, " | ", nrow(d), " ", tolower(input$time_frame), " bars | ",
                   sum(d$Signal == "Buy"), " Buy / ", sum(d$Signal == "Sell"), " Sell signals")
    warn <- NULL
    if (!is.null(res$note)) warn <- res$note
    if ("Moving Averages" %in% input$technical_indicators && all(is.na(d$SMA_long)))
      warn <- c(warn, paste("Not enough bars for the", input$ma_long, "period SMA in this time frame."))
    tagList(div(class = "text-muted", msgs),
            if (length(warn)) div(class = "alert alert-warning py-1", paste(warn, collapse = " ")))
  })

  # Steps 2-4: the chart with overlays and annotated signals
  output$stock_chart <- renderPlot({
    res <- chart_data()
    validate(need(!is.null(res$data), res$note),
             need(nrow(res$df) >= 2, "Not enough data in the selected date range."))
    build_chart(res$df, toupper(input$symbol),
                list(indicators = input$technical_indicators, chart_type = input$chart_type,
                     time_frame = input$time_frame, rule = input$rule,
                     show_signals = input$show_signals),
                params())
  }, res = 96)

  output$signal_table <- renderTable({
    res <- chart_data(); req(!is.null(res$data))
    s <- res$df[res$df$Signal != "Hold", c("Date", "Signal", "Close", "RSI", "MACD", "MACD_signal")]
    validate(need(nrow(s) > 0, "No Buy or Sell signals in this period for the selected rule."))
    s$Date <- format(s$Date)
    s
  }, digits = 2)

  output$backtest_summary <- renderUI({
    res <- chart_data(); req(!is.null(res$data), nrow(res$df) >= 2)
    bt <- backtest(res$df)
    tags$table(class = "table table-sm w-auto",
      tags$tr(tags$th("Completed / open trades"), tags$td(bt$trades)),
      tags$tr(tags$th("Strategy return (long only)"), tags$td(fmt_pct(bt$strategy))),
      tags$tr(tags$th("Buy-and-hold return"), tags$td(fmt_pct(bt$buy_hold))),
      tags$tr(tags$th("Winning trades"), tags$td(if (is.na(bt$win_rate)) "-" else sprintf("%.0f%%", 100 * bt$win_rate))))
  })

  output$portfolio_table <- renderTable({
    p <- params()
    rows <- lapply(portfolio_symbols, function(sym) {
      res <- prepare_data(sym, input$date_range, input$time_frame, input$rule, p)
      if (is.null(res$data) || nrow(res$df) < 2) {
        return(data.frame(Symbol = sym, Last = NA, Change = "no data", RSI = NA,
                          `Last signal` = "-", `Strategy` = "-", `Buy & hold` = "-", check.names = FALSE))
      }
      d <- res$df; bt <- backtest(d)
      last_sig <- tail(d[d$Signal != "Hold", ], 1)
      data.frame(Symbol = sym, Last = round(tail(d$Close, 1), 2),
                 Change = fmt_pct(tail(d$Close, 1) / d$Close[1] - 1),
                 RSI = round(tail(d$RSI, 1), 1),
                 `Last signal` = if (nrow(last_sig)) paste(last_sig$Signal, format(last_sig$Date)) else "none",
                 Strategy = fmt_pct(bt$strategy),
                 `Buy & hold` = fmt_pct(bt$buy_hold), check.names = FALSE)
    })
    do.call(rbind, rows)
  })

  output$data_table <- renderTable({
    res <- chart_data(); req(!is.null(res$data))
    d <- tail(res$df[, c("Date", "Open", "High", "Low", "Close", "Volume", "SMA_short",
                         "SMA_long", "RSI", "MACD", "Signal")], 15)
    d$Date <- format(d$Date)
    d
  }, digits = 2)
}

shinyApp(ui, server)
