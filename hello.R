source("R/kuant.R")

print(getwd())
print(list.files(getwd()))
print(list.dirs(getwd()))

# ==> Step 1. get tickers
tickers <- get_tickers()
print(head(tickers))

write_tickers(tickers)
# tickers <- read_parquet("obs/tickers.parquet")
