naiveForecast <-function(sampl, forecastingSteps, verbose=F){
  if (verbose) print(paste("Naive; training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))
  forecast <- as.numeric(sampl[nrow(sampl),])
  res <- matrix(rep(forecast,each=forecastingSteps),nrow=forecastingSteps)
  colnames(res) <- colnames(sampl)
  return(res)
}
xModel.naive <- list(
  name="naive",
  run = naiveForecast,
  functions = c(),
  packages = c()
)

zeroForecast <-function(sampl, forecastingSteps){
  forecast <- rep(0, ncol(sampl))
  res <- matrix(rep(forecast,each=forecastingSteps),nrow=forecastingSteps)
  colnames(res) <- colnames(sampl)
  return(res)
}

xModel.zero <- list(
  name="HA",
  run = zeroForecast,
  functions = c(),
  packages = c()
)

meanForecast <-function(sampl, forecastingSteps){
  forecast <- apply(sampl, 2, mean)
  res <- matrix(rep(forecast,each=forecastingSteps),nrow=forecastingSteps)
  colnames(res) <- colnames(sampl)
  return(res)
}

xModel.mean <- list(
  name="simpleMean",
  run = meanForecast,
  functions = c(),
  packages = c()
)


maForecast <-function(sampl, forecastingSteps, steps=10){
  n<-nrow(sampl)
  forecast <- apply(sampl[(n-steps+1):n,], 2, mean)
  res <- matrix(rep(forecast,each=forecastingSteps),nrow=forecastingSteps)
  colnames(res) <- colnames(sampl)
  return(res)
}
xModel.ma <- list(
  name="MA",
  run = maForecast,
  functions = c(),
  packages = c()
)



autoArimaForecast <-function(sampl, forecastingSteps, stationary=T, allowdrift =F, allowmean = F, verbose=T){
  if (verbose) print(paste("Auto ARIMA; training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))
  m <- matrix(nrow = forecastingSteps, ncol = ncol(sampl))
  for (i in 1:ncol(sampl)){
    dat <- sampl[,i]
    fit <- auto.arima(dat, stationary=stationary, allowdrift = allowdrift, allowmean = allowmean)
    f <- as.vector(forecast::forecast(fit,forecastingSteps)$mean)
    m[,i] <- f
  }
  res <- data.frame(m)
  colnames(res) <- colnames(sampl)
  return(res)
}
xModel.autoarima <- list(
  name="autoarima",
  run = autoArimaForecast,
  functions = c(),
  packages = c('forecast')
)