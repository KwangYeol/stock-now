suppressMessages(library(httr))
suppressMessages(library(rvest))
suppressMessages(library(data.table))
suppressMessages(library(stringr))
suppressMessages(library(xts))
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
suppressMessages(library(lubridate))
suppressMessages(library(timetk))
suppressMessages(library(dplyr))
suppressMessages(library(arrow))

.libPaths( c(Sys.getenv("RENV_PATHS_ROOT"), .libPaths()))
if (!arrow::arrow_available()) {
  arrow::install_arrow()
}

#                                                             #
# <-----------           naver finance           -----------> #
#                                                             #
get_symbol_ <- function(name, count=2500, timeframe="day") {
  url = paste0(
    'https://fchart.stock.naver.com/sise.nhn?symbol=', name,
    '&timeframe=', timeframe,
    '&count=', count,
    '&requestType=0')

  # get records
  GET(url) %>%
    read_html(., encoding='EUC-KR') %>%
    html_nodes('item') %>%
    html_attr('data') -> 
  records

  # parse record: transform to xts format
  records %>%
    read_delim(., delim='|', col_names=FALSE) %>%
    data.frame ->
    df

  # set meaningful column name
  colnames(df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume')
  # as Date format
  df %<>% mutate(`Date` = ymd(`Date`))
  # set index from Date column
  rownames(df) <- df$Date
  # return except duplicated column
  df %>% select(-c(`Date`))
}

get_symbols <- function(names) {
  symbol_list = list()
  i = 1
  for (name in names) {
    df <- get_symbol_(name)
    symbol_list[[name]] <- df
    # sleep
    cat(".")
    if (i %% 100 == 0) print(paste0(" : ", i))
    i = i + 1
    Sys.sleep(sample(5:9, 1)/10)
  }
  print("")
  symbol_list
}

write_symbols <- function(symbols, filename) {
  sym_list = list()
  i = 1
  for (name in names(symbols)) {
    symbols[[name]] %>%
      mutate(`symbol` = name, `Date` = rownames(.)) -> 
      sym
    sym_list[[i]] <- sym
    i = i + 1
  }
  sym_list <- do.call(rbind, sym_list)

  tkpath <- file.path("obs", paste0(filename, ".parquet"))
  if (codec_is_available("gzip")) {
    write_parquet(sym_list, tkpath, compression = "gzip")
  }
  else {
    write_parquet(sym_list, tkpath)
  }
}

#                                                             #
# <-----------            KIND  (KRX)            -----------> #
#                                                             #
gen_otp_url <- 'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
ticker_download_url <- 'http://file.krx.co.kr/download.jspx'

gen_otp <- function(utype = 'index') {
  gen_otp_data = list(name = 'fileDown', filetype = 'csv')

  yyyymmdd <- Sys.Date()
  yyyymmdd <- format(yyyymmdd, format="%Y%m%d")

  if (utype=='index') {
    c_url="MKD/13/1302/13020401/mkd13020401"
    gen_otp_data <- c(gen_otp_data,
                      market_gubun = 'ALL',
                      gubun = '1',
                      schdate = yyyymmdd,
                      url=c_url,
                      pagePath = paste0("/contents/", c_url, ".jsp"))
  } else if (utype =='sector') {
    c_url='MKD/03/0303/03030103/mkd03030103'
    gen_otp_data <- c(gen_otp_data,
                      tp_cd='ALL',
                      date=yyyymmdd,
                      lang='ko',
                      url=c_url,
                      pagePath = paste0("/contents/", c_url, ".jsp"))
  } else {
    return (NULL)
  }

  otp = POST(gen_otp_url, query = gen_otp_data) %>%
    read_html() %>%
    html_text()
  otp
}

get_ticker_by_type <- function(utype='index') {
  otp <- gen_otp(utype)

  POST(ticker_download_url, 
       query = list(code = otp),
       add_headers(referer = gen_otp_url)) %>%
    read_html() %>%
    html_text() %>%
    read_csv() ->
    down_csv

  down_csv
}

get_tickers <- function() {
  by_index <- get_ticker_by_type("index")
  by_sector <- get_ticker_by_type("sector")

  join_key = intersect(names(by_index), names(by_sector))
  krx_tikers = merge(by_sector,
                     by_index,
                     by = join_key,
                     all = FALSE)

  # 상품명. 대신밸런스제7호스팩, 하이제4호스팩, 등
  # 우선주. 이름이 한글자만 다르다. xxx우B, xxx우C가 있다.
  krx_tikers[1:16] %>%
    filter(str_trim(`관리여부`) == "-") %>%
    filter(!grepl('스팩', (.)[, '종목명'])) %>%
    filter(str_sub((.)[, '종목코드'], -1, -1) == 0) ->
    krx_tikers

  krx_tikers = krx_tikers[order(-krx_tikers['시가총액(원)']), ]

  krx_tikers$EPS <- parse_number(krx_tikers$EPS)
  krx_tikers$PER <- parse_number(krx_tikers$PER)
  krx_tikers$BPS <- parse_number(krx_tikers$BPS)
  krx_tikers$PBR <- parse_number(krx_tikers$PBR)

  rownames(krx_tikers) = NULL
  krx_tikers
}

write_tickers <- function(tickers) {
  tkpath <- file.path("obs", "tickers.parquet")
  if (file.exists(tkpath)) {
    tickers_saved <- read_parquet(tkpath, compression="gzip")
    tickers <- rbind(tickers, tickers_saved) %>% unique
  }
  if (codec_is_available("gzip")) {
    write_parquet(tickers, tkpath, compression="gzip")
  }
  else {
    write_parquet(tickers, tkpath)
  }

}
