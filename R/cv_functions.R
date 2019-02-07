needs(foreach)
needs(doParallel)
needs(matrixStats)

movingWindow <- function(data, seriesNames,xModel, trainingWindowSize,
                         forecastingSteps, forecastEvery, clusterNumber=detectCores()-1,
                         outfile="movingWindow.txt",...){
  df<-as.data.frame(data)
  df <- as.data.frame(df[,c(seriesNames)])
  n <- nrow(df)
  validationSize <- n - trainingWindowSize - forecastingSteps + 1
  forecasts <- list()
  if (clusterNumber>1){
    cl <- makeCluster(clusterNumber, outfile=outfile)
    registerDoParallel(cl)
  }
  seqVals <- seq(1,validationSize, by=forecastEvery)
  print(paste("Models to estimate: ", length(seqVals),", from ",data[seqVals[1],]$datetime[1], " to ",data[seqVals[length(seqVals)],]$datetime[1]))
  res <- foreach (i = seqVals,.export=xModel$functions, 
                  .packages=xModel$packages) %dopar%
    xModel$run(df[i:(i+trainingWindowSize-1),], forecastingSteps, ...)
  if (clusterNumber>1) stopCluster(cl)
  count <- 1
  for (i in seqVals){
    for (j in 1:forecastingSteps){
      if (length(forecasts)<j){
        forecasts[[j]] <- data.frame() 
      }
      forecasts[[j]] <- rbind(forecasts[[j]],res[[count]][j,])
      colnames(forecasts[[j]]) <- seriesNames
    }
    count <- count+1
  }
  
  
  RMSEs <- data.frame()
  MAEs <- data.frame()
  MASEs <- data.frame()
  MAPEs <- data.frame()
  cumRMSEs <- data.frame()
  cumMAEs <- data.frame()
  cumMASEs <- data.frame()
  cumMAPEs <- data.frame()
  for (j in 1:forecastingSteps){
    realValues <- df[j+seqVals+trainingWindowSize-1,]
    f <- forecasts[[j]]
    m1 <- c()
    m2 <- c()
    m3 <- c()
    cm2 <- c()
    cm3 <- c()
    for (i in 1:ncol(f)){
      m1 <- c(m1, rmse(realValues[,i], f[,i]))
      m2 <- c(m2, mae(realValues[,i], f[,i]))
      m3 <- c(m3, mase(realValues[,i], f[,i]))
      cumRealValues <- rep(0, length(f[,i]))
      cumFValues <- rep(0, length(f[,i]))
      for (k in 1:j){
        cumRealValues <- cumRealValues + df[k+seqVals+trainingWindowSize-1,i]
        cumFValues <- cumFValues + forecasts[[k]][,i]
      }
      cm2 <- c(cm2, mae(cumRealValues, cumFValues))
    }
    RMSEs <- rbind(RMSEs, m1)
    MAEs <- rbind(MAEs, m2)
    MASEs <- rbind(MASEs, m3)
    cumMAEs <- rbind(cumMAEs, cm2)
    cumMAPEs <- rbind(cumMAPEs, cm3)
    colnames(RMSEs) <- colnames(df)
    colnames(MAEs) <- colnames(df)
    colnames(MASEs) <- colnames(df)
    
    colnames(cumMAEs) <- colnames(df)
  }
  return(list(RMSE = RMSEs,MAE = MAEs,MASE=MASEs,cumMAE = cumMAEs))
}


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
  print(paste("Models to estimate: ", length(seqVals)))
  res <- foreach (i = seqVals,.export=xModel$functions, 
                  .packages=xModel$packages) %do%
    xModel$run(df[i:(i+trainingWindowSize-1),], forecastingSteps, ...)
  if (clusterNumber>1) stopCluster(cl)
  count <- 1
  result <- tibble()
  for (i in seqVals){
    print("Forecasts for ")
    actual <- as.tibble(data[(i+trainingWindowSize):(i+trainingWindowSize-1+forecastingSteps),])
    print(actual$datetime)
    actual$forecast_horizon<-1:forecastingSteps
    actual$v<-"actual"
    f<-as.tibble(res[[count]])
    f$datetime <- data[(i+trainingWindowSize):(i+trainingWindowSize-1+forecastingSteps),]$datetime
    f$forecast_horizon<-1:forecastingSteps
    f$v<-"forecasted"
    x<-bind_rows(actual,f) %>% gather(key="detector", value="value", series)%>%spread(key=v, value=value)
    result<-bind_rows(result,x)
    count <- count+1
  }
 return(result)
}