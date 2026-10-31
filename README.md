# BDA400 Assignment 6 — Technical Analysis using R, Visualization Phase

**Student:** Sabouh Mohamed  
**Repository:** https://github.com/mohamedsabouh851-sketch/Sabouh_Mohamed_CA_BDA400_A06

Interactive stock dashboard built in R Shiny. Retrieves OHLCV prices from Yahoo Finance via quantmod.

## Run in RStudio

Download the repository, open its folder in RStudio, and run:

```r
install.packages(c("shiny","ggplot2","quantmod","TTR","xts","zoo"))
shiny::runApp(".")
```

Requires an internet connection and Yahoo Finance availability.

## Functionality

- Stock selector (AAPL, MSFT, GOOG, NVDA, TSLA) and custom ticker
- Date range and daily, weekly or monthly timeframe
- Line, area and OHLC candlestick charts
- Short/long SMA overlays, RSI and MACD panes with indicator toggles
- Buy / Sell labels at actual SMA crossover events; Hold otherwise
- Recent observations and CSV export
- Input validation and insufficient-history handling

## Suggested verification

1. Open with AAPL, two years of daily history.
2. Check all three chart types and switch between daily, weekly and monthly.
3. Toggle SMA, RSI, MACD, and Buy/Sell labels.
4. Change SMA lengths and examine crossover annotations.
5. Test an invalid ticker and export results.

## Limitations

No trading is executed. The dashboard uses unadjusted closing prices. Yahoo Finance can fail or rate-limit requests, and indicators need sufficient history. RSI/MACD panels are separate from the price pane to preserve readable scales.

**Execution status:** The application has not been run in RStudio in this environment; live Yahoo Finance fetching and chart rendering remain to be verified on a computer with R installed.

**LMS submission:** Submit the cover page with this public repository URL after verification.
