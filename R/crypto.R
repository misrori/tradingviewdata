#' Get all coins available at TradingView
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr insistently
#' @import data.table
#' @examples
#' @export
get_all_coins <- insistently(function() {
  data = '{"columns":["base_currency","base_currency_desc","base_currency_logoid","update_mode","type","typespecs","exchange","crypto_total_rank","close","pricescale","minmov","fractional","minmove2","currency","24h_close_change|5","market_cap_calc","fundamental_currency_code","24h_vol_cmc","circulating_supply","crypto_common_categories","market_cap_diluted_calc","24h_vol_change_cmc","Perf.W","Perf.1M","Perf.3M","Perf.6M","Stoch.RSI.K","MACD.macd","github_commits"],"ignore_unknown_fields":false,"range":[0,7000],"sort":{"sortBy":"crypto_total_rank","sortOrder":"asc"},"markets":["coin"]}'

  res <- httr::POST(url = "https://scanner.tradingview.com/coin/scan", body = data)
  t <- fromJSON(content(res, 'text'), simplifyDataFrame = F)

  coins <- rbindlist(lapply(t$data, function(x){
    t_coin <- x$d
    values <- sapply(t_coin, function(x)paste0(x,collapse = ';'))
    data.frame(t(data.frame(values)), stringsAsFactors = F)
  }))
  names(coins) <- fromJSON(data)$columns
  return(change_numeric(coins))
})


