
autoArimaXForecast <-function(sampl, forecastingSteps, shortestA,stationary=T, allowdrift =F, allowmean = F,
                              radius1=1, radius2=3, verbose=F,include_ratio=F,
                              spatial_step=0, decay=NA){
  print(paste("autoArimaXForecast training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))
  start_time<-Sys.time()
  if (length(forecastingSteps)>1) 
    nfs <- length(forecastingSteps)
  else nfs <- forecastingSteps
  m <- matrix(nrow = nfs, ncol = ncol(sampl))
  noexp<-0
  for (i in 1:ncol(sampl)){
    fnode <- colnames(sampl)[i]
    f<-c()
    if (length(forecastingSteps)>1) 
      fs <- forecastingSteps
    else fs <- 1:forecastingSteps
    for (fHorizon in fs){
      step<-sum(rep(spatial_step,fHorizon-1))
      res<-prepare_predictors(sampl,fnode,shortestA,radius1+step,radius2+step,include_ratio,decay=decay,verbose=verbose)
      if (verbose) print(paste("Forecasting:", fnode))
      if (verbose) print(paste("Variables", colnames(res)))
      if(ncol(res)>1){
        res$y <- shift(res$y, 1)
        xreg<-(res%>%select(-y))%>%as.matrix
        added<-F
        tryCatch({
          fit <- auto.arima(res$y, xreg=xreg, stationary=stationary, allowdrift = allowdrift, allowmean = allowmean)
          newX<-xreg[(nrow(res)-fHorizon+1),,drop=F]
          newX<-newX[rep(seq_len(nrow(newX)),fHorizon),,drop=F]
          fc<-forecast::forecast(fit,h=fHorizon,xreg=newX)
          f<-c(f,fc$mean[fHorizon])
          added<-T
        },error=function(e){
          print(paste("ARIMA specification failed, forecasting with naive", fnode,i))
        }
        )
        if(!added){
          f<-c(f,res$y[(nrow(res)-fHorizon)])
        }
      }else{
        fit <- auto.arima(res$y, stationary=stationary, allowdrift = allowdrift, allowmean = allowmean)
        fc<-as.vector(forecast::forecast(fit,fHorizon)$mean)
        f <- c(f,fc[length(fc)])
        #f<-c(f,res$y[nrow(res)-fHorizon+1])
        noexp<-noexp+1
      }
    }
    #print(paste("No explanatory variables, forecasting with arima", noexp, "of", ncol(sampl)))
    #if (verbose) print(paste("Forecasts:", f))
    m[,i] <- f
  }
  res <- data.frame(m)
  colnames(res) <- colnames(sampl)
  print(paste("No explanatory variables, forecasting with arima", noexp, "of", ncol(sampl)*length(fs)))
  print(paste("Completed auto arimax",rownames(sampl)[1],"Execution time =",(Sys.time() - start_time)))
  return(res)
}

prepare_predictors<-function(sampl,fnode,shortestA,radius1,radius2,include_ratio=F,verbose=F,decay=NA){
  fname<-gsub(".volume","",fnode)
  prev_nodes<-shortestA[order(shortestA[,fname]),fname]
  
  dists1<-prev_nodes[prev_nodes<radius1]
  if (verbose){
    print(paste("First level neighbours",length(dists1)))
  }
  res<-as.tibble(sampl)
  if (length(dists1)>0){
    if (is.na(decay)){#Linear decay
      w<-as.vector(1/dists1)
    }else{
      w<-exp(-((dists1^2)/(decay^2)))
    }
    circle1<-paste0(names(dists1),".volume")
    #res<-res%>%mutate(x_wmean1=as.matrix(.[circle1])%*%as.vector(1/dists1),
    #                  x_sdev1=0
    #                  )%>%
    #  mutate(x_sdev1=ifelse(is.na(x_sdev1),0,x_sdev1))
    res$x_wmean1<-apply(res, 1, function(x) as.vector(as.numeric(x[circle1]))%*%w)
    res$x_sdev1<-apply(res, 1, function(x) sqrt(((as.vector(as.numeric(x[circle1]))-as.numeric(x["x_wmean1"]))^2)%*%w / length(w)))
    res<-res%>%mutate(x_sdev1=ifelse(is.na(x_sdev1),0,x_sdev1))
    if (length(dists1)==1) res<-res%>%select(-x_sdev1)
  }
  
  dists2<-prev_nodes[prev_nodes<radius2 & prev_nodes>=radius1]
  if (verbose){
    print(paste("Second level neighbours",length(dists2)))
  }
  if (length(dists2)>0){
    if (is.na(decay)){#Linear decay
      w<-as.vector(1/dists2)
    }else{
      w<-exp(-((dists2^2)/(decay^2)))
    }
    circle2<-paste0(names(dists2),".volume")
    #res<-res%>%mutate(x_wmean2=as.matrix(.[circle2])%*%as.vector(1/dists2),
    #                  x_sdev2=rowSds(as.matrix(.[circle2])))%>%
    #  mutate(x_sdev2=ifelse(is.na(x_sdev2),0,x_sdev2))
    res$x_wmean2<-apply(res, 1, function(x) as.vector(as.numeric(x[circle2]))%*%w)
    res$x_sdev2<-apply(res, 1, function(x) sqrt(((as.vector(as.numeric(x[circle2]))-as.numeric(x["x_wmean2"]))^2)%*%w / length(w)))
    res<-res%>%mutate(x_sdev2=ifelse(is.na(x_sdev2),0,x_sdev2))
    
    if (length(dists2)==1) res<-res%>%select(-x_sdev2)
    
    if (include_ratio & length(dists1)>0){
      res<-res%>%mutate(x_ratio=ifelse(x_wmean1==0,1,x_wmean2/x_wmean1))
    }
  }
  res<-res%>%select(y=fnode,matches("x_"))
  return(res)
}


spatialSVMForecast <-function(sampl, forecastingSteps, shortestA,radius1=1, 
                              radius2=3, verbose=F,include_ratio=F,
                              spatial_step=0, model="svm", decay=NA){
  if (verbose) print(paste("spatialSVMForecast training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))
  if (verbose) print(paste("radius1", radius1,"radius2", radius2,"spatial_step",spatial_step,"model",model,"sortestA",sum(shortestA)))
  start_time <- Sys.time()
  if (length(forecastingSteps)>1) 
    nfs <- length(forecastingSteps)
  else nfs <- forecastingSteps
  m <- matrix(nrow = nfs, ncol = ncol(sampl))
  noexp<-0
  for (i in 1:ncol(sampl)){
    fnode <- colnames(sampl)[i]
    f<-c()
    if (length(forecastingSteps)>1) 
      fs <- forecastingSteps
    else fs <- 1:forecastingSteps
    for (fHorizon in fs){
      step<-sum(rep(spatial_step,fHorizon-1))
      res<-prepare_predictors(sampl,fnode,shortestA,radius1+step,radius2+step,include_ratio,decay=decay,verbose=verbose)
      #if (verbose) print(paste("Forecasting:", fnode))
      #if (verbose) print(paste("Variables", colnames(res)))
      
      if(ncol(res)>1){
        res$y <- shift(res$y, 1)
        if (model=="svm"){
          fit <- svm(y~.,data=res)
        }else{
          fit <- lm(y~.,data=res)
        }
        f<-c(f,predict(fit, (res%>%select(-y))[(nrow(res)-fHorizon+1),]))
      }else{
        fit <- auto.arima(res$y, stationary=T, allowdrift = F, allowmean = F)
        fc<-as.vector(forecast::forecast(fit,fHorizon)$mean)
        f <- c(f,fc[length(fc)])
        #f<-c(f,res$y[nrow(res)-fHorizon+1])
        #print(paste("No explanatory variables, forecasting with arima", f))
        noexp<-noexp+1
      }
    }
    #if (verbose) print(paste("Forecasts:", f))
    m[,i] <- f
  }
  res <- data.frame(m)
  colnames(res) <- colnames(sampl)
  print(paste("No explanatory variables, forecasting with arima", noexp, "of", ncol(sampl)*length(fs)))
  print(paste("Completed spatial SVM",rownames(sampl)[1],"Execution time =",(Sys.time() - start_time)))
  return(res)
}


xModel.SpatialARIMAX <- list(
  name="SpatialARIMAX",
  run = autoArimaXForecast,
  functions = c('prepare_predictors','shift'),
  packages = c('forecast','tidyverse','matrixStats')
)

xModel.SpatialSVR <- list(
  name="SpatialSVR",
  run = spatialSVMForecast,
  functions = c('prepare_predictors','shift'),
  packages = c('forecast','tidyverse','matrixStats','e1071')
)



estimate.SpatilARIMAX<-function(params, cv, results.file, models.estimated.file){
modelid <- paste("SP-ARIMAX",cv$trainingMinutes,
                 cv$include.mean,cv$stationary,cv$allowdrift,cv$radius1,cv$radius2,
                 cv$include_ratio,cv$spatial_step,cv$decay,
                 collapse = '')
models.estimated <- readRDS(models.estimated.file)
if (!(modelid %in% models.estimated)){
  results <- readRDS(results.file)
  print(paste("Estimating model",modelid))
  res<-do.call(rollingWindow,
               c(params,list(xModel=xModel.SpatialARIMAX,
                             allowmean=cv$include.mean,
                             stationary=cv$stationary,
                             allowdrift=cv$allowdrift,
                             radius1=cv$radius1,
                             radius2=cv$radius2,
                             include_ratio=cv$include_ratio,
                             spatial_step=cv$spatial_step,
                             decay=cv$decay,
                             shortestA=shortestA)),
               envir=environment())
  results<-bind_rows(results,res%>%mutate(trainingMinutes=cv$trainingMinutes,include.mean=cv$include.mean,
                                          stationary=cv$stationary,
                                          allowdrift=cv$allowdrift,
                                          radius1=cv$radius1,
                                          radius2=cv$radius2,
                                          include_ratio=cv$include_ratio,
                                          decay=cv$decay,
                                          spatial_step=cv$spatial_step))
  models.estimated<-c(models.estimated,modelid)
  saveRDS(results, results.file)
  saveRDS(models.estimated, models.estimated.file)
}else{
  print(paste("Model",modelid,"already estimated; skipped"))
}
}


estimate.SpatilSVR<-function(params, cv, results.file, models.estimated.file){
  modelid <- paste("SP-SVR",cv$trainingMinutes,cv$radius1,cv$radius2,
                 cv$include_ratio,cv$spatial_step, cv$modelsp,cv$decay,
                 collapse = '')
  models.estimated <- readRDS(models.estimated.file)
  if (!(modelid %in% models.estimated)){
    results <- readRDS(results.file)
  print(paste("Estimating model",modelid))
  res<-do.call(rollingWindow,
               c(params,list(xModel=xModel.SpatialSVR,
                             radius1=cv$radius1,
                             radius2=cv$radius2,
                             include_ratio=cv$include_ratio,
                             spatial_step=cv$spatial_step,
                             model=cv$modelsp,
                             decay=cv$decay,
                             shortestA=shortestA)),
               envir=environment())
  results<-bind_rows(results,res%>%mutate(trainingMinutes=cv$trainingMinutes,
                                          radius1=cv$radius1,
                                          radius2=cv$radius2,
                                          include_ratio=cv$include_ratio,
                                          spatial_step=cv$spatial_step,
                                          decay=cv$decay,
                                          modelsp=cv$modelsp))
  models.estimated<-c(models.estimated,modelid)
  saveRDS(results, results.file)
  saveRDS(models.estimated, models.estimated.file)
}else{
  print(paste("Model",modelid,"already estimated; skipped"))
}
}