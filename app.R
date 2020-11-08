source("R/kuant.R")

# ==> Step 1. get tickers
tickers <- get_tickers()
write_tickers(tickers)
# tickers <- read_parquet("obs/tickers.parquet")

# ==> Step 2. get symbols
symbols <- tickers[['종목코드']]
symbols <- c(symbols[c(1:5)], "kospi", "kosdaq")

df <- get_symbols(symbols, count=100)
write_symbols(df, "index")
