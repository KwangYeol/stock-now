source("R/kuant.R")

# ==> Step 1. get tickers
tickers <- get_tickers()
write_tickers(tickers)
# tickers <- read_parquet("obs/tickers.parquet")

# ==> Step 2. get symbols
symbols <- tickers[['종목코드']]
symbols <- c(symbols, "kospi", "kosdaq")

df <- get_symbols(symbols, count=100)
write_symbols(df, "index")

# ==> Step 3. get wise index
get_wise_index()

# ==> Step 4. get financial data
ret <- get_guide_crawl(tickers)
get_guide(tickers, ret$value, ret$fs)
