#' Get all stocks available at TradingView
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @importFrom purrr insistently
#' @export
get_all_stocks <-
  insistently(
    function() {
      json_string <- '{"filter":[{"left":"market_cap_basic","operation":"nempty"},{"left":"type","operation":"in_range","right":["stock","dr","fund"]},{"left":"subtype","operation":"in_range","right":["common","","etf","unit","mutual","money","reit","trust"]},{"left":"exchange","operation":"in_range","right":["AMEX","NASDAQ","NYSE"]}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["logoid","name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","industry","sector","SMA50","SMA100","SMA200","RSI","Perf.Y","Perf.3M","Perf.6M","Perf.1M","Perf.W","High.3M","High.6M","price_52_week_high","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2","SMA50","close","SMA100","SMA200","RSI","RSI[1]"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,8000]}'
      res <- POST(url = 'https://scanner.tradingview.com/america/scan', body = json_string)
      t <- fromJSON(content(res, 'text'))
      df_data <-
        rbindlist(lapply(t$data$d, function(x){
          data.frame(t(data.frame(x)), stringsAsFactors = F)
        }))
      names(df_data) <-  fromJSON(json_string)$columns
      final_data <- cbind( data.table('exchange' = sapply(strsplit(t$data$s, ':'), '[[', 1)),  df_data)
      change_numeric(final_data)
    },
    quiet = FALSE)



#' Get cashed stock df
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @importFrom purrr insistently
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @export
cashed_stock_df <- memoise::memoise(
  insistently(
    function() {
      json_string <- '{"filter":[{"left":"market_cap_basic","operation":"nempty"},{"left":"type","operation":"in_range","right":["stock","dr","fund"]},{"left":"subtype","operation":"in_range","right":["common","","etf","unit","mutual","money","reit","trust"]},{"left":"exchange","operation":"in_range","right":["AMEX","NASDAQ","NYSE"]}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["logoid","name","close","change","change_abs","Recommend.All","volume","market_cap_basic","price_earnings_ttm","earnings_per_share_basic_ttm","number_of_employees","industry","sector","SMA50","SMA100","SMA200","RSI","Perf.Y","Perf.3M","Perf.6M","Perf.1M","Perf.W","High.3M","High.6M","price_52_week_high","description","name","type","subtype","update_mode","pricescale","minmov","fractional","minmove2","SMA50","close","SMA100","SMA200","RSI","RSI[1]"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,8000]}'
      res <- POST(url = 'https://scanner.tradingview.com/america/scan', body = json_string)
      t <- fromJSON(content(res, 'text'))
      df_data <-
        rbindlist(lapply(t$data$d, function(x){
          data.frame(t(data.frame(x)), stringsAsFactors = F)
        }))
      names(df_data) <-  fromJSON(json_string)$columns
      final_data <- cbind( data.table('exchange' = sapply(strsplit(t$data$s, ':'), '[[', 1)),  df_data)
      change_numeric(final_data)
    },
    quiet = FALSE),
  cache = cachem::cache_mem(max_age = 20*60*60)
)



#' Get S&P500 stocks
#' @importFrom rtsdata ds.getSymbol.yahoo
#' @importFrom purrr insistently
#' @import data.table
#' @export
get_spy_index <- insistently(function() {
  df <- suppressMessages( data.frame(ds.getSymbol.yahoo('SPY')))
  names(df) <- tolower(sapply(strsplit(names(df), '.', fixed = T), '[[', 2))
  df$date <- as.Date(row.names(df))
  row.names(df) <- 1:nrow(df)
  return(change_numeric(df))
})

#' Changing text column to numeric if it possible
#' @import data.table
#' @import data.table
change_numeric <- function (df) {
  df <- df[, !duplicated(names(df)), with = F]
  nevek <- names(df)
  for (i in nevek) {
    if (suppressWarnings(sum(is.na(as.numeric(df[[i]][!is.na(df[[i]])]))) == 0)) {
      df[[i]] <- as.numeric(df[[i]])
    }
  }
  return(data.table(df))
}


#' Download data from the tradingView site with the passed json string
#' @export
#' @param json_string json string from inspect view at the bottom of TradingView
#' @importFrom data.table data.table rbindlist
#' @importFrom jsonlite fromJSON
#' @importFrom httr content POST
#' @importFrom purrr insistently
#' @import data.table
tradingview_data_from_json_string <- insistently(function(json_string) {

  res <- httr::POST(url = 'https://scanner.tradingview.com/america/scan',  body = json_string)

  t <- fromJSON(content(res, 'text'))
  df_data <-
    rbindlist(lapply(t$data$d, function(x){
      data.frame(t(data.frame(x)), stringsAsFactors = F)
    }))

  names(df_data) <-  fromJSON(json_string)$columns
  final_data <- cbind( data.table('exchange' = sapply(strsplit(t$data$s, ':'), '[[', 1)),  df_data)
  return(change_numeric(df))
})




