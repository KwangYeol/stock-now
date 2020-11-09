source("R/kuant.R")


# ==> Step 3. get wise index
get_wise_index()

# ==> Step 1. get tickers
tickers <- get_tickers()
write_tickers(tickers)
# tickers <- read_parquet("obs/tickers.parquet")

# ==> Step 4. get financial data
ret <- get_guide_crawl(tickers)
# fs_list.Rds
# value_list.csv
get_guide(tickers, ret$value, ret$fs)


# ==> Step 2. get symbols
symbols <- tickers[['종목코드']]
symbols <- c(symbols, "kospi", "kosdaq")

df <- get_symbols(symbols, count=100)
write_symbols(df, "index")

