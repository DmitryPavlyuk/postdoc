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


estimate.HA<-function(params, cv, results.file, models.estimated.file){
  modelid <- paste(xModel.zero$name,collapse = '')
  models.estimated <- readRDS(models.estimated.file)
  if (!(modelid %in% models.estimated)){
    results <- readRDS(results.file)
    print(paste("Estimating model",modelid))
    res<-do.call(rollingWindow,
                 c(params,list(xModel=xModel.zero)),
                 envir=environment())
    results<-bind_rows(results,res)
    models.estimated<-c(models.estimated,modelid)
    saveRDS(results, results.file)
    saveRDS(models.estimated, models.estimated.file)
  }else{
    print(paste("Model",modelid,"already estimated; skipped"))
  }
}


estimate.arima<-function(params, cv, results.file, models.estimated.file){
  
  #Auto arima
  modelid <- paste(xModel.autoarima$name,cv$trainingMinutes,
                   cv$include.mean,cv$stationary,cv$allowdrift,collapse = '')
  models.estimated <- readRDS(models.estimated.file)
  if (!(modelid %in% models.estimated)){
    results <- readRDS(results.file)
    print(paste("Estimating model",modelid))
    res<-do.call(rollingWindow,
                 c(params,list(xModel=xModel.autoarima,
                               allowmean=cv$include.mean,
                               stationary=cv$stationary,
                               allowdrift=cv$allowdrift)),
                 envir=environment())
    results<-bind_rows(results,res%>%mutate(trainingMinutes=cv$trainingMinutes,include.mean=cv$include.mean,
                                            stationary=cv$stationary,
                                            allowdrift=cv$allowdrift))
    models.estimated<-c(models.estimated,modelid)
    saveRDS(results, results.file)
    saveRDS(models.estimated, models.estimated.file)
  }else{
    print(paste("Model",modelid,"already estimated; skipped"))
  }
}


estimate.MA<-function(params, cv, results.file, models.estimated.file){
  modelid <- paste(xModel.ma$name,collapse = '')
  models.estimated <- readRDS(models.estimated.file)
  if (!(modelid %in% models.estimated)){
    results <- readRDS(results.file)
    print(paste("Estimating model",modelid))
    res<-do.call(rollingWindow,
                 c(params,list(xModel=xModel.ma)),
                 envir=environment())
    results<-bind_rows(results,res)
    models.estimated<-c(models.estimated,modelid)
    saveRDS(results, results.file)
    saveRDS(models.estimated, models.estimated.file)
  }else{
    print(paste("Model",modelid,"already estimated; skipped"))
  }
}

estimate.simpleMean<-function(params, cv, results.file, models.estimated.file){
  modelid <- paste(xModel.mean$name,cv$trainingMinutes,collapse = '')
  models.estimated <- readRDS(models.estimated.file)
  if (!(modelid %in% models.estimated)){
    results <- readRDS(results.file)
    print(paste("Estimating model",modelid))
    res<-do.call(rollingWindow,
                 c(params,list(xModel=xModel.mean)),
                 envir=environment())
    results<-bind_rows(results,res%>%mutate(trainingMinutes=cv$trainingMinutes))
    models.estimated<-c(models.estimated,modelid)
    saveRDS(results, results.file)
    saveRDS(models.estimated, models.estimated.file)
  }else{
    print(paste("Model",modelid,"already estimated; skipped"))
  }
}