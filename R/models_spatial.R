
autoArimaXForecast <-function(sampl, forecastingSteps, shortestA,stationary=T, allowdrift =F, allowmean = F,
                              radius1=1, radius2=3, verbose=F,include_ratio=F,
                              spatial_step=0){
  print(paste("Start autoArimaXForecast",rownames(sampl)[1]))
  start_time<-Sys.time()
  m <- matrix(nrow = forecastingSteps, ncol = ncol(sampl))
  for (i in 1:ncol(sampl)){
    fnode <- colnames(sampl)[i]
    f<-c()
    for (fHorizon in 1:forecastingSteps){
      step<-sum(rep(spatial_step,fHorizon-1))
      res<-prepare_predictors(sampl,fnode,shortestA,radius1+step,radius2+step,include_ratio,verbose=verbose)
      if (verbose) print(paste("Forecasting:", fnode))
      if (verbose) print(paste("Variables", colnames(res)))
      if(ncol(res)>1){
        res$y <- shift(res$y, 1)
        xreg<-(res%>%select(-y))
        added<-F
        tryCatch({
          fit <- auto.arima(res$y, xreg=xreg, stationary=stationary, allowdrift = allowdrift, allowmean = allowmean)
          newX<-xreg[(nrow(res)-fHorizon+1),]
          newX<-newX[rep(seq_len(nrow(newX)),fHorizon),]
          fc<-forecast::forecast(fit,h=fHorizon,xreg=newX)
          f<-c(f,fc$mean[fHorizon])
          added<-T
        },error=function(e){
          if (verbose) print(paste("ARIMA specification failed, forecasting with naive", f))
        }
        )
        if(!added){
          f<-c(f,res$y[(nrow(res)-fHorizon)])
        }
      }else{
        f<-c(f,res$y[nrow(res)-fHorizon+1])
        if (verbose) print(paste("No explanatory variables, forecasting with naive", f))
      }
    }
    if (verbose) print(paste("Forecasts:", f))
    m[,i] <- f
  }
  res <- data.frame(m)
  colnames(res) <- colnames(sampl)
  print(paste("Completed auto arimax",rownames(sampl)[1],"Execution time =",(Sys.time() - start_time),"secs"))
  return(res)
}

prepare_predictors<-function(sampl,fnode,shortestA,radius1,radius2,include_ratio=F,verbose=F){
  fname<-gsub(".volume","",fnode)
  prev_nodes<-shortestA[order(shortestA[,fname]),fname]
  
  dists1<-prev_nodes[prev_nodes<radius1]
  if (verbose){
    print("First level neighbours") 
    print(names(dists1)) 
  }
  res<-as.tibble(sampl)
  if (length(dists1)>0){
    circle1<-paste0(names(dists1),".volume")
    res<-res%>%mutate(x_wmean1=as.matrix(.[circle1])%*%as.vector(1/dists1),
                      x_sdev1=rowSds(as.matrix(.[circle1])))%>%
      mutate(x_sdev1=ifelse(is.na(x_sdev1),0,x_sdev1))
  }
  
  dists2<-prev_nodes[prev_nodes<radius2]
  if (verbose){
    print("Second level neighbours") 
    print(names(dists2)) 
  }
  if (length(dists2)>0){
    circle2<-paste0(names(dists2),".volume")
    res<-res%>%mutate(x_wmean2=as.matrix(.[circle2])%*%as.vector(1/dists2),
                      x_sdev2=rowSds(as.matrix(.[circle2])))%>%
      mutate(x_sdev2=ifelse(is.na(x_sdev2),0,x_sdev2))
    
    if (include_ratio & length(dists1)>0){
      res<-res%>%mutate(x_ratio=ifelse(x_wmean1==0,1,x_wmean2/x_wmean1))
    }
  }
  res<-res%>%select(y=fnode,matches("x_"))
  return(res)
}
spatialSVMForecast <-function(sampl, forecastingSteps, shortestA,radius1=1, 
                              radius2=3, verbose=F,include_ratio=F,
                              spatial_step=0, model="svm"){
  print(paste("Start spatial SVM",model,rownames(sampl)[1]))
  start_time <- Sys.time()
  m <- matrix(nrow = forecastingSteps, ncol = ncol(sampl))
  for (i in 1:ncol(sampl)){
    fnode <- colnames(sampl)[i]
    f<-c()
    for (fHorizon in 1:forecastingSteps){
      step<-sum(rep(spatial_step,fHorizon-1))
      res<-prepare_predictors(sampl,fnode,shortestA,radius1+step,radius2+step,include_ratio,verbose=verbose)
      if (verbose) print(paste("Forecasting:", fnode))
      if (verbose) print(paste("Variables", colnames(res)))
      
      if(ncol(res)>1){
        res$y <- shift(res$y, 1)
        if (model=="svm"){
          fit <- svm(y~.,data=res)
        }else{
          fit <- lm(y~.,data=res)
        }
        f<-c(f,predict(fit, (res%>%select(-y))[(nrow(res)-fHorizon+1),]))
      }else{
        f<-c(f,res$y[nrow(res)-fHorizon+1])
        if (verbose) print(paste("No explanatory variables, forecasting with naive", f))
      }
    }
    if (verbose) print(paste("Forecasts:", f))
    m[,i] <- f
  }
  res <- data.frame(m)
  colnames(res) <- colnames(sampl)
  print(paste("Completed spatial SVM",rownames(sampl)[1],"Execution time =",(Sys.time() - start_time),"secs"))
  return(res)
}
