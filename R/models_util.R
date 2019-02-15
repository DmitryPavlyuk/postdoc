suf<-function(xModel, suffix){
  xModel$name<-paste(xModel$name, suffix)
  return(xModel)
}