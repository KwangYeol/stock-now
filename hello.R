source("R/kuant.R")

# ==> Step 1. get tickers
tickers <- get_tickers()
print(head(tickers))
print(getwd())
print(list.files(getwd()))

write_tickers(tickers)
# tickers <- read_parquet("obs/tickers.parquet")
