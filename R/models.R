library(Metrics)
library(MTS)
library(e1071)
library(matrixStats)


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


prepareFixed <-function(lagM, ARlags, MAlags, include.mean=T){
  k <- nrow(lagM)
  ARs <- list()
  MAs <- list()
  for (i in 1:ARlags){
    ARs[[i]] <- diag(k)
  }	
  if(MAlags>0){
    for (i in 1:MAlags){
      MAs[[i]] <- diag(k)
    }
  }
  for (i in 1:k){
    for (j in 1:k){
      if (i!=j){
        lag <- lagM[i,j]
        if (lag>0){
          if (lag<=ARlags){
            ARs[[lag]][i,j] <- 1
          }
          if (lag<=MAlags){
            MAs[[lag]][i,j] <- 1
          }
        }
      }
    }
  }
  res <- matrix(0,0,k)
  for (i in 1:ARlags){
    res <- rbind(res,ARs[[i]])
  }	
  if(MAlags>0){
    for (i in 1:MAlags){
      res <- rbind(res,MAs[[i]])
    }
  }
  if (include.mean){
    res <- cbind(rep(1,nrow(res)),res)
  }
  return(res)
}

starForecast <-function(sampl, forecastingSteps, lagMatrix, arLags, matrixMode=NULL, control=list(ccfThreshold=0.5), returnModel=F, refine = F, complete=F,include.mean=F){
  fixed <- NULL
  
  if (!is.null(matrixMode)){
    if (matrixMode == "CCF"){
      print(paste("CCF with threshold",control$ccfThreshold))
      lagMatrix <- constructCorMatrix(sampl,names(sampl),maxLag=arLags,threshold=control$ccfThreshold)
      fixed <- prepareFixed(lagMatrix, arLags, 0, include.mean=include.mean)
    }else if (matrixMode == "glasso"){
      print(paste("Glasso with rho",control$glassoRho))
      fixed <- glassoFixed(sampl,names(sampl),maxLag=arLags,rho=control$glassoRho, include.mean=include.mean)
      print(paste("Number of links",sum(fixed)))
    }else if (matrixMode == "univariate"){
      print("Only own lags")
      fixed <- univariateFixed(sampl,names(sampl),maxLag=arLags, include.mean=include.mean)
    }else if (matrixMode == "travelTime"){
      print("Travel time regularisation")
      fixed <- prepareFixed(control$lagMatrix, arLags, 0, include.mean=include.mean)
    }else if (matrixMode == "ensemble"){
      print("Ensemble learning")
      time.fixed <- prepareFixed(control$lagMatrix, arLags, 0, include.mean=include.mean)
      print(paste("Number of links in time",sum(time.fixed>0)))
      glasso.fixed <- glassoFixed(sampl,names(sampl),maxLag=arLags,rho=control$glassoRho, include.mean=include.mean)
      print(paste("Number of links in glasso",sum(glasso.fixed>0)))
      cor.fixed <- prepareFixed(constructCorMatrix(sampl,names(sampl),maxLag=arLags,threshold=control$ccfThreshold), arLags, 0, include.mean=include.mean)
      print(paste("Number of links in cor",sum(cor.fixed>0)))
      fixed<-time.fixed+glasso.fixed+cor.fixed
      fixed[fixed<2]<-0
      fixed[fixed>=2]<-1
      print(paste("Number of links in ensemble",sum(fixed>0)))
      links <- data.frame(arLags,control$glassoRho,control$ccfThreshold,
                 sum(time.fixed>0),sum(glasso.fixed>0),sum(cor.fixed>0),sum(fixed>0))
      write.table(links, "links.csv", sep = ",", append = TRUE, quote = FALSE,
                  col.names = FALSE, row.names = FALSE)
    }
  }else{
    if(!complete){
      fixed <- prepareFixed(lagMatrix, arLags, 0, include.mean=include.mean)
    }
  }
  sink(tempfile())
  if(arLags==0){
    m1<-VARorder(sampl,output=F)
    arLags <- m1$aicor
  }
  print(paste("Estimating VAR model, arLags=",arLags,", starting from ",rownames(sampl)[1],", n= ",nrow(sampl)))
  model <- VAR(sampl,p=arLags, fixed=fixed,output = F, include.mean=include.mean)
  if (refine){
    model=refVAR(model,thres=2.0)
  }
  print(paste("--- VAR model", rownames(sampl)[1]," completed"))
  pr <- VARpred(model, h=forecastingSteps, Out.level = F)
  res <- as.data.frame(as.matrix(pr$pred))
  if (forecastingSteps==1) res<-t(res)
  rownames(res) <- seq(1, nrow(res))
  colnames(res) <- colnames(sampl)
  
  sink()
  if (!returnModel){
    return(res)
  }else{
    return(list(model=model, forecast=res))
  }
}

constructCorMatrix <- function(data, series, maxLag=3,threshold=0.5){
  iDat <- data[,series]
  n <- ncol(iDat)
  res <- matrix(0, n, n)
  rownames(res)<-colnames(iDat)
  colnames(res)<-colnames(iDat)
  for (i in 1:n){
    for (j in 1:n){
      if (i!=j){
        corVals <- ccf(iDat[,i],iDat[,j], lag.max=maxLag, plot=F)
        nMax <- which.max(corVals$acf)
        if ((nMax <= maxLag)&&(corVals$acf[nMax]>threshold)){
          res[i,j] <- maxLag-nMax+1
        }
      }
    }
  }
  return (res)
}



glassoFixed <- function(data, series, maxLag=3,rho=0.5, include.mean=F){
  univariateF<-univariateFixed(data, series, maxLag,include.mean)
  orig <- data[,series]
  orig.names<-series
  k<-ncol(orig)
  dat<-orig
  for (l in 1:maxLag){
    cn<-colnames(dat)
    dat<-cbind(dat[-nrow(dat),], orig[-c(1:l),])  
    colnames(dat)<-c(cn,paste0(orig.names,"_l",l))
  }
  dat.cov<-cor(dat)
  gl<-glasso(dat.cov, rho=rho)
  dat.cov.reg<-gl$w
  res<-dat.cov.reg[-c(1:k),1:k]
  res[res>0]<-1
  res<-res+univariateF
  if (include.mean){
    res <- cbind(rep(1,nrow(res)),res)
  }
  return (res)
}


univariateFixed <- function(data, series, maxLag=3, include.mean=F){
  orig <- data[,series]
  orig.names<-series
  k<-ncol(orig)
  res<-data.frame()
  for (l in 1:maxLag){
    res<-rbind(res,diag(k))
  }
  if (include.mean){
    res <- cbind(rep(1,nrow(res)),res)
  }
  return (res)
}

xModel.star <- list(
  name="STAR",
  run = starForecast,
  functions = c('prepareFixed','constructCorMatrix','univariateFixed','glassoFixed'),
  packages = c('tidyverse','matrixStats','tseries','e1071','MTS','glasso')
)
