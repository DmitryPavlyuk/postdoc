needs("rlang")
needs("dplyr")


add_results <- function(tib,result,modelName,trainingSize, ta,forecastingSteps,...){
  for (indicatorName in names(result)){
    tib<-bind_rows(tib,as.tibble(result[[indicatorName]])%>%
                     mutate(indicator=indicatorName,model=modelName,training_size=trainingSize)%>%
                     mutate(!!! rlang::quos(...))%>%
                     mutate(forecasting_horizon=seq(ta,forecastingSteps*ta,by=ta))%>%
                     gather(series,key="node",value="value"))
  }
  return(tib)
}


shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
mymape <- function(actual,pred){
  d<-abs((actual - pred)/actual)
  d[d==Inf]<-NA
  mape <- mean(d, na.rm = T)*100
  return (mape)
}
