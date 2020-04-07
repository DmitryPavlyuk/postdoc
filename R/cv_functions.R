rollingWindow <- function(data, seriesNames,xModel, trainingWindowSize,
                          forecastingSteps, forecastEvery, clusterNumber=detectCores()-1,
                          outfile="movingWindow.txt",...){
  df<-as.data.frame(data)
  rownames(df)<-data$datetime
  df <- as.data.frame(df[,c(seriesNames)])
  n <- nrow(df)
  if (length(forecastingSteps)>1){
    nfs <- max(forecastingSteps)
    lfs <- length(forecastingSteps)
    fs<-forecastingSteps
  }else{
    nfs <- forecastingSteps
    lfs <- forecastingSteps
    fs<-1:forecastingSteps
  } 
  validationSize <- n - trainingWindowSize - nfs + 1
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
    if (length(forecastingSteps)>1){
      act<-(i+trainingWindowSize-1+forecastingSteps)
    }else{
      act<-(i+trainingWindowSize):(i+trainingWindowSize-1+forecastingSteps)
    } 
    hash <- randomStr()
    actual <- as.tibble(data[act,])
    actual$forecast_horizon<-fs
    ldate <- data[i+trainingWindowSize-1,]$datetime
    actual$last_date <- ldate
    actual$v<-"actual"
    actual$model_hash<-hash
    f<-as.tibble(res[[count]])
    f$datetime <- data[act,]$datetime
    f$forecast_horizon<-fs
    f$v<-"forecasted"
    f$last_date <- ldate
    f$model_hash<-hash
    x<-bind_rows(actual,f) %>% gather(key="detector", value="value", series)%>%spread(key=v, value=value)
    
    result<-bind_rows(result,x)
    count <- count+1
  }
  return(result%>%mutate(model=xModel$name))
}

estimateFeatures <- function(fs.function, data, seriesNames,trainingWindowSize,
                          forecastEvery, max.lag,fs.folder,
                          req.functions=c(),req.packages=c(),
                          clusterNumber=detectCores()-1,
                          outfile="estimate-features.log",...){
  df<-as.data.frame(data)
  rownames(df)<-data$datetime
  df <- as.data.frame(df[,c(seriesNames)])
  n <- nrow(df)
  validationSize <- n - trainingWindowSize + 1
  if (clusterNumber>1){
    cl <- makeCluster(clusterNumber, outfile=outfile)
    registerDoParallel(cl)
  }
  opts <- list(...)
  seqVals <- seq(1,validationSize, by=forecastEvery)
  print(paste("Timestamps:", length(seqVals),"Number of clusters:",clusterNumber))
  folder <- file.path(fs.folder,trainingWindowSize, max.lag)
  if (!is.null(opts$rho) & (opts$method=="GLASSO")) folder <- file.path(folder,opts$rho)
  if (!file.exists(folder)) dir.create(folder, recursive = T)
  res <- foreach (i = seqVals,.export=req.functions,
                  .packages=req.packages) %dopar% {
                    mts <- as.matrix(df[i:(i+trainingWindowSize-1),])
                    filename<-paste0(rownames(mts)[nrow(mts)],".rds")
                    filename <-gsub(" ","_", filename)
                    filename <-gsub(":","", filename)
                    filename <- file.path(folder,filename)
                    if (file.exists(filename)){
                      print(paste("FS file exists: ", filename, ". Feature selection skipped"))
                    }else{
                      fs <- fs.function(mts, max.lag=max.lag, ...)
                      print(paste("Saving", filename))
                      saveRDS(fs,file=filename)
                    }
                  }
  return("OK")
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
      group_by(.dots=c(g,'forecast_horizon'))%>%summarise(!!n :=mean(v,na.rm=T), q95=quantile(v,0.95,na.rm=T))
  }else{
    results%>%
      group_by(.dots=c(g,'forecast_horizon','detector'))%>%
      summarise(v=fun(actual,forecasted))%>%
      group_by(.dots=c(g,'forecast_horizon'))%>%
      summarise(!!n :=mean(v,na.rm=T), q95=quantile(v,0.95,na.rm=T))
  }
}

randomStr <- function(length=12){
  paste(sample(c(0:9, letters, LETTERS), length, replace=TRUE),collapse="")
}

matSmoothing <- function(mlist, alpha){
  res <- NULL
  mult <- alpha
    for (i in (1:length(mlist))){
      A <- mult*mlist[[i]]
      if (is.null(res)){
        res<-A
      }else{
        res <- res + A
      }
      mult <- mult*(1-alpha)
  }
  return(res)
}