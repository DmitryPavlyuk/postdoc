rollingWindow <- function(data, seriesNames,xModel, trainingWindowSize,
                          forecastingSteps, forecastEvery, clusterNumber=detectCores()-1,
                          outfile="movingWindow.txt",...){
  df<-as.data.frame(data)
  rownames(df)<-data$datetime
  df <- as.data.frame(df[,c(seriesNames)])
  n <- nrow(df)
  validationSize <- n - trainingWindowSize - forecastingSteps + 1
  forecasts <- list()
  if (clusterNumber>1){
    cl <- makeCluster(clusterNumber, outfile=outfile)
    registerDoParallel(cl)
  }
  seqVals <- seq(1,validationSize, by=forecastEvery)
  print(paste("Models to estimate:", length(seqVals),"Number of clusters:",clusterNumber))
  xModelF<-xModel$run
  res <- foreach (i = seqVals,.export=xModel$functions,
                  .packages=xModel$packages) %dopar%
    xModelF(df[i:(i+trainingWindowSize-1),], forecastingSteps, ...)
  if (clusterNumber>1) stopCluster(cl)
  count <- 1
  result <- tibble()
  for (i in seqVals){
    hash <- randomStr()
    actual <- as.tibble(data[(i+trainingWindowSize):(i+trainingWindowSize-1+forecastingSteps),])
    actual$forecast_horizon<-1:forecastingSteps
    ldate <- data[i+trainingWindowSize-1,]$datetime
    actual$last_date <- ldate
    actual$v<-"actual"
    actual$model_hash<-hash
    f<-as.tibble(res[[count]])
    f$datetime <- data[(i+trainingWindowSize):(i+trainingWindowSize-1+forecastingSteps),]$datetime
    f$forecast_horizon<-1:forecastingSteps
    f$v<-"forecasted"
    f$last_date <- ldate
    f$model_hash<-hash
    x<-bind_rows(actual,f) %>% gather(key="detector", value="value", series)%>%spread(key=v, value=value)
    
    result<-bind_rows(result,x)
    count <- count+1
  }
  return(result%>%mutate(model=xModel$name))
}


cvSummary <- function(results,fun, cumulative=F, params=c()){
  n <- toupper(substitute(fun))
  g <- c('model',params)
  if (cumulative){
    results%>%
      group_by(model_hash,detector)%>%
      arrange(model_hash,detector,forecast_horizon)%>%
      mutate(cumactual=cumsum(actual),cumforecasted=cumsum(forecasted))%>%
      group_by(.dots=c(g,'forecast_horizon','detector'))%>%
      summarise(v=fun(cumactual,cumforecasted))%>%
      group_by(.dots=c(g,'forecast_horizon'))%>%summarise(!!n :=mean(v), q95=quantile(v,0.95))
  }else{
    results%>%
      group_by(.dots=c(g,'forecast_horizon','detector'))%>%
      summarise(v=fun(actual,forecasted))%>%
      group_by(.dots=c(g,'forecast_horizon'))%>%
      summarise(!!n :=mean(v), q95=quantile(v,0.95))
  }
}

randomStr <- function(length=12){
  paste(sample(c(0:9, letters, LETTERS), length, replace=TRUE),collapse="")
}