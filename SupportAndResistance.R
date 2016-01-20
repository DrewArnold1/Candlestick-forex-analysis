calcSandR = function(fx) {
  fx %<>%
    mutate(support = ifelse(low < lag(low) & low < lead(low) &
                              low < lag(low,2) & low < lead(low,2),TRUE,FALSE),
           resistance = ifelse(high > lag(high) & high > lead(high) &
                                 high > lag(high,2) & high > lead(high,2),TRUE,FALSE),
           turningPoint = ifelse(support, low, ifelse(resistance, high, NA)))
  #fx$runningSR = createLaggedSR(fx$turningPoint)
    weights = fx %>%
      select(time, turningPoint) %>%
      filter(!is.na(turningPoint))
    
    temp = weights$turningPoint
    weights$weight = sapply(weights$turningPoint, calcWeight, temp)
    fx = left_join(fx, weights)
    
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