getFXdata = function(pair, granularity, count = 500) {
  
  key = "ddf460006591c0443c0cd21bd47acc9f-7e1e904e1f8dc4422e94c611a2e3f0af"
  address = paste0("https://api-fxtrade.oanda.com/v1/candles?",
                   "instrument=", pair,"&count=", count,
                   "&candleFormat=midpoint&granularity=", granularity,
                   "&dailyAlignment=0&alignmentTimezone=America%2FNew_York")
  test = GET(address, add_headers("Authorization" = paste("Bearer", key)))
  parsedResult = fromJSON(toString(test))$candles
  parsedResult$pair = pair
  parsedResult %<>%
    rename(open = openMid,
           high = highMid,
           low = lowMid,
           close = closeMid) %>%
    mutate(time = ymd_hms(time))
  
  return(parsedResult)
}