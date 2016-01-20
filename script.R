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
source("IDcandles.R")
source("SupportAndResistance.R")
source("getFXdata.R")

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



