starForecast <-function(sampl, forecastingSteps, arLags,threshold, fs.folder=NA, control=list(),
                        returnModel=F, refine = F, include.mean=F, verbose=T, exclude.series=NULL,
                        ownlags=T){
  last_date <- rownames(sampl)[nrow(sampl)]
  if (verbose) print(paste("SpVAR",fs.folder," training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))
  series <- colnames(sampl)
  complete <- is.na(fs.folder)
  fixed <- NULL
  if (!complete){
    fs.folder<-as.character(fs.folder)
    if (file.exists(fs.folder) && !dir.exists(fs.folder)){
      filename<-fs.folder
    }else{
      filename <-gsub(" ","_", last_date)
      filename <-paste0(gsub(":","", filename),".rds")
      filename <- file.path(fs.folder, filename)
      if (!file.exists(filename)){
        ds <- daySec(as.POSIXct(last_date, format="%Y-%m-%d %H:%M:%S"))
        filename <- file.path(fs.folder, paste0(wday(last_date),"_",ds,".rds"))
      } 
    }
    if (!file.exists(filename)) stop(paste("No feature set found: ",filename))
    print(paste("Using ",filename))
    fixed <- readRDS(filename)
    fixed<-fsMTS::cutoff(fixed, threshold)
    if (ownlags) {
      fixed <-fixed+fsMTS::fsMTS(as.matrix(sampl),arLags,method="ownlags")
      fixed[fixed>0]<-1
    }else{
      # m<-matrix(0,arLags*ncol(sampl), ncol(sampl))
      # diag(m)<-1
      # fixed <-fixed+m
      # fixed[fixed>0]<-1
    }
    print(paste(filename,"Number of links",sum(fixed>0)))
  }else{
    print("No feature sets - estimating complete VAR")
  }
  
  if(arLags==0){
    m1<-MTS::VARorder(sampl,maxp = 20, output=F)
    print(paste("Order", m1$aicor))
    arLags <- m1$aicor
  }
  sink(tempfile())
  model <- MTS::VAR(sampl,p=arLags, fixed=fixed,output = F, include.mean=include.mean)
  if (refine){
    model=MTS::refVAR(model,thres=2.0)
  }
  pr <- MTS::VARpred(model, h=forecastingSteps, Out.level = F)
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

xModel.star <- list(
  name="STAR",
  run = starForecast,
  functions = c('randomStr',"shift", "daySec"),
  packages = c('tidyverse','matrixStats','tseries','MTS','tibble', 'fsMTS','lubridate')
)


knnForecast <-function(mts, forecastingSteps, arLags,threshold, fs.folder=NA,control=list(),ownlags=T){
  print(paste("KNN",fs.folder," training sample: ",rownames(mts)[1],"-",rownames(mts)[nrow(mts)]))
  complete <- is.na(fs.folder)
  last_date <- rownames(mts)[nrow(mts)]
  if (!complete){
    fs.folder<-as.character(fs.folder)
    if (file.exists(fs.folder) && !dir.exists(fs.folder)){
      filename<-fs.folder
    }else{
      filename <-gsub(" ","_", last_date)
      filename <-paste0(gsub(":","", filename),".rds")
      filename <- file.path(fs.folder, filename)
      if (!file.exists(filename)){
        ds <- daySec(as.POSIXct(last_date, format="%Y-%m-%d %H:%M:%S"))
        filename <- file.path(fs.folder, paste0(wday(last_date),"_",ds,".rds"))
      } 
    }
    if (!file.exists(filename)) stop(paste("No feature set found: ",filename))
    print(paste("Using ",filename))
    fixed <- readRDS(filename)
    fixed<-fsMTS::cutoff(fixed, threshold)
    if (ownlags) {
      fixed <-fixed+fsMTS::fsMTS(as.matrix(mts),arLags,method="ownlags")
      fixed[fixed>0]<-1
    }else{
        # m<-matrix(0,arLags*ncol(mts), ncol(mts))
        # diag(m)<-1
        # fixed <-fixed+m
        # fixed[fixed>0]<-1
    }
    print(paste(filename,"Number of links",sum(fixed>0)))
  }else{
    print("No feature sets - estimating complete KNN")
  }
  
  mts.ext<-mts
  for (l in 1:arLags){
    sh<-shift.mts(mts, l)
    colnames(sh)<-paste0(colnames(mts),".l",l)
    mts.ext<-cbind(mts.ext,sh)
  }
  mts.ext<-mts.ext[complete.cases(mts.ext), ]
  forecast<-NULL
  k<-ifelse(is.numeric(control$kneighbours),control$kneighbours, 3)
  for (f in 1:forecastingSteps){
    train<-mts.ext[-((nrow(mts.ext)-f+1):nrow(mts.ext)),-seq(ncol(mts))]
    test<-mts.ext[nrow(mts.ext)-f+1,-seq(ncol(mts))]
    y.pred<-c()
    for (i in 1:ncol(mts)){
      if(!complete){
        yname<-colnames(mts)[i]
        fs<-fixed[,yname]
        xnames<-names(fs[fs>0])
        ytrain<-train[,xnames]
        ytest<-test[,xnames]
      }else{
        ytrain<-train
        ytest<-test
      }
      y<-mts.ext[-seq(f),i]
      res<-knn.reg(train=ytrain,test=ytest, y=y, k=k)
      y.pred<-c(y.pred,res$pred)
    }
    names(y.pred)<-colnames(mts)
    forecast<-bind_rows(forecast,y.pred)
  }
  print(paste("Completed training sample: ",rownames(mts)[1],"-",rownames(mts)[nrow(mts)]))
  return(forecast)
}
xModel.knn <- list(
  name="KNN",
  run = knnForecast,
  functions = c('randomStr',"shift.mts", "daySec"),
  packages = c('tidyverse','fsMTS','lubridate','FNN')
)



bigVARForecast <-function(sampl, forecastingSteps, arLags, struct="Basic", verbose=T){  
  if (verbose) print(paste("BigVAR; training sample: ",rownames(sampl)[1],"-",rownames(sampl)[nrow(sampl)]))

  half <- nrow(sampl) %/% 2
  mod1<-constructModel(sampl%>%as.matrix,p=arLags,struct=struct,gran=c(50,10),
                       RVAR=TRUE,h=5,cv="Rolling",MN=FALSE,verbose=FALSE,IC=F, recursive=T,
                       T1=half, T2=(nrow(sampl)-1),
                       intercept = F,window.size=half)
  r<-cv.BigVAR(mod1)
  f<-c()
  for (h in 1:forecastingSteps){
    f<-c(f,predict(r,n.ahead=h))
  }
  res <- matrix(f,nrow=forecastingSteps,byrow=T)
# 
#   mod2<-constructModel(sampl%>%as.matrix,p=arLags,struct="Basic",gran=c(0.0000000001),ownlambdas=T, intercept = F)
#   r<-BigVAR.est(mod2)
#   res<-predictBigVAR(r$B[,,1],sampl%>%as.matrix,p=arLags,h=forecastingSteps)

  colnames(res) <- colnames(sampl)
  return(res)
}

predictBigVAR <- function(beta,data,p, h=1){
  for (i in 1:h){
    n<-nrow(data)
    fcst<-c()
    for (i in 1:3){
      fcst<-c(fcst,as.numeric(as.vector(data[nrow(data)-i+1,])))
    }
    f<-c(beta%*%c(0,fcst))
    data<-rbind(data, f)
  }
  n<-nrow(data)
  return(data[(n-h+1):n,])
}

xModel.bigVAR <- list(
  name="BigVAR",
  run = bigVARForecast,
  functions = c('predictBigVAR'),
  packages = c('tidyverse','BigVAR')
)


estimate.SpVAR<-function(params, cv, results.folder){
  complete<-is.null(cv$fs.folder)
modelid <- paste(suf(xModel.star,cv$fs.folder)$name,cv$trainingMinutes,
                 cv$arLags,cv$include.mean,ifelse(complete,"",cv$threshold),ifelse(complete,"",cv$ownlags),collapse = '')
if (!is.null(cv$exclude.s)) modelid<-paste(modelid,cv$exclude.s,collapse = '')
models.estimated <- readRDS(models.estimated.file)
es <- NA
if (!is.null(cv$exclude.s)) es<-strsplit(as.character(cv$exclude.s),":")[[1]]
filename<-paste0(gsub("[.]","_",gsub(" ","_",gsub("/","_",modelid))),".rds")
filename<-file.path(results.folder,filename)
print(filename)
if (!file.exists(filename)){
  results <- tibble()
  print(paste("Estimating model",modelid))
  res<-do.call(rollingWindow,
               c(params,list(xModel=suf(xModel.star,cv$folder),
                             control=list(),
                             fs.folder=cv$fs.folder,
                             exclude.series=es,
                             arLags=cv$arLags,include.mean=cv$include.mean,
                             threshold=cv$threshold,
                             ownlags=cv$ownlags)),
               envir=environment())
  results<-bind_rows(results,res%>%mutate(trainingMinutes=cv$trainingMinutes,arLags=cv$arLags,
                                          include.mean=cv$include.mean, exclude.series=cv$exclude.s, 
                                          fs.folder = cv$fs.folder,
                                          threshold=ifelse(complete,NA,cv$threshold),
                                          ownlags=ifelse(complete,NA,cv$ownlags)))
  print(paste("Saving",filename))
  saveRDS(results, filename)
  #saveRDS(models.estimated, models.estimated.file)
}else{
  print(paste("Model",modelid,"already estimated; skipped"))
}
}



estimate.KNN<-function(params, cv, results.folder){
  complete<-is.na(cv$fs.folder)
  modelid <- paste(suf(xModel.knn,cv$fs.folder)$name,cv$trainingMinutes,
                   cv$arLags,cv$kneighbours,ifelse(complete,"",cv$threshold),ifelse(complete,"",cv$ownlags),collapse = '')
  models.estimated <- readRDS(models.estimated.file)
  filename<-paste0(gsub("[.]","_",gsub(" ","_",gsub("/","_",modelid))),".rds")
  filename<-file.path(results.folder,filename)
  print(filename)
  if (!file.exists(filename)){
    results <- tibble()
    print(paste("Estimating model",modelid))
    res<-do.call(rollingWindow,
                 c(params,list(xModel=suf(xModel.knn,cv$folder),
                               control=list(kneighbours=cv$kneighbours),
                               fs.folder=cv$fs.folder,
                               arLags=cv$arLags,
                               threshold=cv$threshold,
                               ownlags=cv$ownlags)),
                 envir=environment())
    results<-bind_rows(results,res%>%mutate(trainingMinutes=cv$trainingMinutes,arLags=cv$arLags,
                                            fs.folder = cv$fs.folder,
                                            kneighbours = cv$kneighbours,
                                            threshold=ifelse(complete,NA,cv$threshold),
                                            ownlags=ifelse(complete,NA,cv$ownlags)))
    print(paste("Saving",filename))
    saveRDS(results, filename)
    #saveRDS(models.estimated, models.estimated.file)
  }else{
    print(paste("Model",modelid,"already estimated; skipped"))
  }
}