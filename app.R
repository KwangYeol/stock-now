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

glimpse(df)

name <- "005930"
ds <- df[[name]] %>% 
        mutate(`Symbol` = name, `Date` = rownames(.))
ds <- ds[,c(6,7,1,2,3,4,5)]

glimpse(ds)

library(arrow)

write_dataset(ds, "obs/index", format="csv")

x_dir <- file.path("obs", "index", name)
dir.create(x_dir, showWarnings = FALSE)
write_parquet(ds, file.path(x_dir, "part.gz.parquet"), compression="gzip")
