prepareFixed <-function(lagM, ARlags, MAlags,series, include.mean=T){
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
  rnames <- c()
  res <- matrix(0,0,k)
  for (i in 1:ARlags){
    res <- rbind(res,ARs[[i]])
    rnames<-c(rnames,paste0(series,"_arl",i))
  }	
  if(MAlags>0){
    for (i in 1:MAlags){
      res <- rbind(res,MAs[[i]])
      rnames<-c(rnames,paste0(series,"_mal",i))
    }
  }
  cnames <- series
  if (include.mean){
    res <- cbind(rep(1,nrow(res)),res)
    cnames <- c("const",cnames)
  }
  rownames(res) <- rnames
  colnames(res) <- cnames
  return(res)
}

starForecast <-function(sampl, forecastingSteps, lagMatrix, arLags, matrixMode=NULL, control=list(ccfThreshold=0.5), returnModel=F, refine = F, include.mean=F, verbose=T,
                        save_links_file = NULL){
  
  last_date <- rownames(sampl)[nrow(sampl)]
  if (verbose) print(paste("SpVAR [",paste(matrixMode, collapse = ','),"] training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))
  series <- colnames(sampl)
  fixed <- NULL
  complete <- !is.character(matrixMode) | (matrixMode == "complete")
  if (!is.null(matrixMode)){
    if (length(matrixMode)>1){
      print("Ensemble learning")
      nc <- length(series) + ifelse(include.mean,1,0)
      nr <- length(series)*arLags
      fixed <- matrix(rep(0,nr*nc),nr,nc)
      
      if ("travelTime" %in% matrixMode) fixed <- prepareFixed(control$lagMatrix, arLags, 0, 
                                 series,include.mean=include.mean)+fixed
      if ("glasso" %in% matrixMode) fixed <- glassoFixed(sampl,names(sampl),maxLag=arLags,rho=control$glassoRho,
                                  include.mean=include.mean)+fixed
      if ("CCF" %in% matrixMode) fixed <- prepareFixed(constructCorMatrix(sampl,names(sampl),maxLag=arLags,threshold=control$ccfThreshold), arLags, 0,
                                series, include.mean=include.mean)+fixed
      # Majority voting
      fixed[fixed<(length(matrixMode)/2)]<-0
      fixed[fixed>=(length(matrixMode)/2)]<-1
      
      print(paste("Number of links in ensemble",sum(fixed>0)))
    }else if (matrixMode == "CCF"){
      lagMatrix <- constructCorMatrix(sampl,names(sampl),maxLag=arLags,threshold=control$ccfThreshold)
      fixed <- prepareFixed(lagMatrix, arLags, 0, series, include.mean=include.mean)
    }else if (matrixMode == "glasso"){
      fixed <- glassoFixed(sampl,names(sampl),maxLag=arLags,rho=control$glassoRho, include.mean=include.mean)
    }else if (matrixMode == "univariate"){
      fixed <- univariateFixed(sampl,names(sampl),maxLag=arLags, include.mean=include.mean)
    }else if (matrixMode == "travelTime"){
      fixed <- prepareFixed(control$lagMatrix, arLags, 0,
                            series,include.mean=include.mean)
    }
    
    if (!is.null(save_links_file)){
      # tib <- tibble()
      # lck <- lock(paste0(save_links_file,".lock"), timeout = Inf)
      # if (file.exists(save_links_file)) tib <- readRDS(save_links_file)
      # print(paste("Saving features",sum(fixed>0)))
      # tib <- bind_rows(tib, as_tibble(fixed, rownames="x") %>% gather(colnames(fixed), key="y", value="value")%>%
      #                    mutate(fs=ifelse(length(matrixMode)>1,"ensemble",matrixMode),training_minutes=nrow(sampl),
      #                           max_lag=arLags,
      #                           include_mean=include.mean, last_date=last_date,
      #                           glasso_rho=ifelse(is.null(control$glassoRho),NA, control$glassoRho),
      #                           ccf_threshold=ifelse(is.null(control$ccfThreshold),NA, control$ccfThreshold)))
      # saveRDS(tib, save_links_file)
      # unlock(lck)
      saveRDS(as_tibble(fixed, rownames="x") %>% gather(colnames(fixed), key="y", value="value")%>%filter(value>0)%>%
                                   mutate(fs=ifelse(length(matrixMode)>1,"ensemble",matrixMode),training_minutes=nrow(sampl),
                                          max_lag=arLags,
                                          include_mean=include.mean, last_date=last_date,
                                          glasso_rho=ifelse(is.null(control$glassoRho),NA, control$glassoRho),
                                          ccf_threshold=ifelse(is.null(control$ccfThreshold),NA, control$ccfThreshold)),
              paste0(save_links_file,randomStr()))
    }
  }else{
    if(!complete){
      fixed <- prepareFixed(lagMatrix, arLags, 0,series, include.mean=include.mean)
    }
  }
  if(arLags==0){
    m1<-VARorder(sampl,output=F)
    arLags <- m1$aicor
  }
  sink(tempfile())
  model <- VAR(sampl,p=arLags, fixed=fixed,output = F, include.mean=include.mean)
  if (refine){
    model=refVAR(model,thres=2.0)
  }
  pr <- VARpred(model, h=forecastingSteps, Out.level = F)
  sink()
  res <- as.data.frame(as.matrix(pr$pred))
  if (forecastingSteps==1) res<-t(res)
  rownames(res) <- seq(1, nrow(res))
  colnames(res) <- colnames(sampl)
  
  if (verbose) print(paste("Completed training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))
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
  rnames <- c()
  for (l in 1:maxLag){
    cn<-colnames(dat)
    dat<-cbind(dat[-nrow(dat),], orig[-c(1:l),])  
    colnames(dat)<-c(cn,paste0(orig.names,"_l",l))
    rnames<-c(rnames,paste0(series,"_l",l))
  }
  dat.cov<-cor(dat)
  gl<-glasso(dat.cov, rho=rho)
  dat.cov.reg<-gl$w
  res<-dat.cov.reg[-c(1:k),1:k]
  res[res>0]<-1
  res<-res+univariateF
  cnames <- series
  if (include.mean){
    res <- cbind(rep(1,nrow(res)),res)
    cnames <- c("const", cnames)
  }
  
  rownames(res) <- rnames
  colnames(res) <- cnames
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
  functions = c('prepareFixed','constructCorMatrix','univariateFixed','glassoFixed','randomStr'),
  packages = c('tidyverse','matrixStats','tseries','e1071','MTS','glasso','filelock')
)
