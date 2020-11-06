source("R/kuant.R")

# ==> Step 1. get tickers
tickers <- get_tickers()
write_tickers(tickers)
# tickers <- read_parquet("obs/tickers.parquet")

# ==> Step 2. get symbols
symbols <- tickers[['종목코드']]
symbols <- c(symbols, "kospi", "kosdaq")

df <- get_symbols(symbols)
write_symbols(df, "index")
