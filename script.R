library(dplyr)
library(zoo)
library(RCurl)
library(httr)
library(jsonlite)
library(magrittr)
library(caret)
library(ggplot2)
library(lubridate)
library(caret)

data = getFXdata("EUR_USD", "H4", 500)
train = data[1:400,]
test = data[401:500,]

# Use caret package process
# Write function to identify candle types
# Add MAs and MACD to signify trend to pair with candle types 

train = calcSandR(train)
train %<>%
  mutate(sigTurningPoint = ifelse(weight > 3, turningPoint, NA))

ggplot(train, aes(time, close)) +
  geom_line() +
  geom_hline(aes(yintercept = sigTurningPoint))

calcSandR = function(fx) {
  fx %<>%
    mutate(support = ifelse(low < lag(low) & low < lead(low) &
                            low < lag(low,2) & low < lead(low,2),TRUE,FALSE),
           resistance = ifelse(high > lag(high) & high > lead(high) &
                               high > lag(high,2) & high > lead(high,2),TRUE,FALSE),
           turningPoint = ifelse(support, low, ifelse(resistance, high, NA)))
  fx$runningSR = createLaggedSR(fx$turningPoint)
#   weights = fx %>%
#     select(time, turningPoint) %>%
#     filter(!is.na(turningPoint))
#   
#   temp = weights$turningPoint
#   weights$weight = sapply(weights$turningPoint, calcWeight, temp)
#   fx = left_join(fx, weights)
#   
  return(fx)
}

createLaggedSR = function(turningPoint) {
  result = data.frame(SR = list(NULL))#, times = length(turningPoint)))

  for(i in 2:length(turningPoint)) {
    if(!is.na(turningPoint[i])) {
      result$SR[i] = list(result$SR[i-1], turningPoint[i])
    }
    else {result$SR[i] = result$SR[i-1]}
  }
  return(result)
}

calcWeight = function(value, temp) {
  result = ifelse(abs(temp - value) < .0005, 1, 0)
  return(sum(result))
}

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