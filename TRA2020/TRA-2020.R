rm(list=ls())

if (!require(devtools)) install.packages('devtools')
if (!require(needs)) install.packages('needs')
devtools::install_github('ropensci/gtfsr')
devtools::install_github("IRSN/smint")
require(needs)
needs(smint)
needs(gtfsr)
needs(magrittr)
needs(dplyr)
needs(tibble)
needs(readr)
needs(lubridate)
needs(ggplot2)
needs(leaflet)
needs(factoextra)
needs(NbClust)
needs(ClusterR)
needs(tidyverse)
options(viewer = NULL)

dir <- file.path(getwd(),"TRA2020")
  
source(file.path(dir,"TRA-2020-functions.R"))

set_api_key()
#4cf79c4e-6afc-4e4e-b8bf-1eb6b0d88d1f
raw.folder<-paste0(dir,"/RK/Validations-Dump_2018/")
raw.rds.folder<-paste0(dir,"/RK/prepared/")
processed.folder<-paste0(dir,"/RK/processed/")
gtfs.folder<-paste0(dir,"/RK/gtfs/")
daily.folder<-paste0(dir,"/RK/daily/")
folder<-paste0(dir,"/RK/")

if (!dir.exists(daily.folder)){
  data.complete.file <- paste0(folder,"complete.rds")
  data.complete <- load_complete_data(data.complete.file, raw.folder, raw.rds.folder)
  day<-as.Date("2018-01-01",format="%Y-%m-%d",tz="UTC")
  end_date<-as.Date("2018-12-31",format="%Y-%m-%d",tz="UTC")
  while(day<=end_date){
    print(paste("---",day,"---"))
    data.day <- data.complete%>%filter(date>=day,date<(day+1))
    if (nrow(data.day)>100000){
      print("saving")
      saveRDS(data.day, paste0(daily.folder,format(day,"%Y%m%d"),".rds"))
    }else{
      print("seems incomplete, skipping")
    }
    day<-day+1
  }
  data.daily<-data.complete%>%group_by(date)%>%summarize(n=n())
  my.travels<-data.complete%>%filter(card_number=="53372409")
  saveRDS(my.travels,paste0(folder,"my-travels.rds"))
}else{
  my.travels<-readRDS(paste0(folder,"my-travels.rds"))
}


my.travels%>%group_by(date)%>%summarize(n=n())%>%print(n=20)
my.travels%>%filter(date==ymd(20180207))%>%arrange(datetime)
#cday<-ymd(20180207)
cday<-ymd(20180129)

if (!dir.exists(processed.folder)) dir.create(processed.folder)

cday<-as.Date("2018-01-01",format="%Y-%m-%d",tz="UTC")
end_date<-as.Date("2018-12-31",format="%Y-%m-%d",tz="UTC")
while(cday<=end_date){ 
  day <- format(cday,"%Y-%m-%d")
  print(paste("---",day,"---"))
  day.d<-as.POSIXct(day,format="%Y-%m-%d",tz="UTC")
  data.file<-paste0(daily.folder,format(day.d,"%Y%m%d"),".rds")
  if (!file.exists(data.file)){
    print("No data, skipping");
    cday<-cday+1
    next;
  }
  data.day <- readRDS(data.file)
  
  #data.day%>%filter(card_number=="53372409")%>%arrange(datetime)
  #data.day%>%filter(card_number=="53372409")%>%arrange(datetime)%>%select(datetime, trip_id, first_reg, last_reg,transport_code)
  #mobility <- process_day(data.day, day, url)
  
  stat<-list()
  stat$ticket_count<-data.day%>%nrow
  
  data.day<-estimate_trips(data.day)
  data.day%>%ungroup%>%summarise(tickets=n(),trips=n_distinct(trip_id))
  stat$trip_count<-data.day%>%ungroup%>%summarise(trips=n_distinct(trip_id))%>%select(trips)%>%pull
  
  
  data<-load_schedule(gtfs.folder, day)
  
  data.day<-match_trips(data.day, data, day)
  data.day%>%ungroup%>%summarise(tickets=n(),trips=n_distinct(trip_id), matched_trips=n_distinct(trip_id.y))
  
  stat$matched_trip_count<-data.day%>%nrow
  
  #data.day%>%filter(card_number=="53372409")%>%arrange(datetime)%>%select(datetime, trip_id, trip_id.y, stop_name)
  
  data.chains<-find_chains(data.day)
  
  #data.chains%>%filter(card_number=="53372409")
  stat$chain_count<-data.chains%>%ungroup%>%summarise(chains=n_distinct(chain_id))%>%select(chains)%>%pull
  
  #mobility%>%filter(card_number=="53372409")
  
  mobility<-find_mobility_vectors(data.chains)
  stat$mobility_vector_count<-mobility%>%nrow
  stat$mobility_total_distance<-mobility%>%ungroup%>%summarise(dist=sum(distance))%>%select(dist)%>%pull
  
 
  df<-mobility%>%filter(card_number=="53372409")%>%mutate(label=ifelse(is.na(datetime),"back",format(datetime,"%H:%M")))
  if (nrow(df)>0){
    m <- leaflet() %>%addTiles()
  for(i in 1:nrow(df)){
    m%<>%addPolylines(label=as.character(df[i, "label"]),labelOptions = labelOptions(noHide = T),
                      lat = as.numeric(df[i, c("stop_lat.x", "stop_lat.y")]), 
                      lng = as.numeric(df[i, c("stop_lon.x", "stop_lon.y")]))%>%
      addCircleMarkers(lat = as.numeric(df[i, c("stop_lat.y")]), 
                       lng = as.numeric(df[i, c("stop_lon.y")]),radius=4,color="#F30")
  }
  print(m)
  }
  
  daily.mobility <- construct_daily_mobility(mobility%>%filter(distance>1000),day,list(c(5,11),c(11,15),c(15,24),c(NA)),10)
  #daily.mobility <- construct_daily_mobility(mobility,day,list(c(5,24),c(NA)),30)
  result<-list(stat=stat, data.chains=data.chains, mobility=mobility, mobility.pattern=daily.mobility)
  saveRDS(result,paste0(processed.folder,format(cday,"%Y%m%d"),".rds"))
  cday<-cday+1
}



source(file.path("TRA-2020-functions.R"))

fs <- gsub(".rds","",sort(list.files(processed.folder, pattern="\\.rds$",full.names=F)))
res<-list()
for (file in fs){
  res[[file]]<-readRDS(paste0(processed.folder,file,".rds"))
}


all.vectors<-data.frame()
invisible(lapply(seq_along(res), function(i){
  all.vectors<<-bind_rows(all.vectors,res[[i]]$mobility.pattern)
}))
norm.vals<-all.vectors%>%as_tibble%>%mutate(dayminutes=hour(datetime)*60+minute(datetime))%>%
  summarise(stop_lat.x.mean=mean(stop_lat.x,na.rm=T),stop_lat.x.sd=sd(stop_lat.x,na.rm=T),
            stop_lon.x.mean=mean(stop_lon.x,na.rm=T),stop_lon.x.sd=sd(stop_lon.x,na.rm=T),
            stop_lat.y.mean=mean(stop_lat.y,na.rm=T),stop_lat.y.sd=sd(stop_lat.y,na.rm=T),
            stop_lon.y.mean=mean(stop_lon.y,na.rm=T),stop_lon.y.sd=sd(stop_lon.y,na.rm=T),
            size.mean=mean(size,na.rm=T),size.sd=sd(size,na.rm=T),
            dayminutes.mean=mean(dayminutes,na.rm=T),dayminutes.sd=sd(dayminutes,na.rm=T))


stat<-data.frame()
invisible(lapply(seq_along(res), function(i){
  stat<<-bind_rows(stat,c(dt=names(res)[[i]], res[[i]]$stat))
}))
stat%<>%as_tibble%>%mutate(date=as.POSIXct(dt,format="%Y%m%d",tz="UTC"))%>%mutate(wd=as.factor(weekdays(date)),wday=wday(date))
stat%>%group_by(wd)%>%summarise(m_ticket_count=mean(ticket_count),
                                m_trip_count=mean(trip_count),
                                m_matched_trip_count=mean(matched_trip_count),
                                m_mobility_vector_count=mean(mobility_vector_count),
                                m_mobility_vector_count/m_ticket_count)%>%write_csv("stat.csv")


dist<-matrix(0,nrow=length(res), ncol=length(res))
rownames(dist)<-names(res)
colnames(dist)<-names(res)
fsize<-0

p1<-res[['20180301']]$mobility.pattern
p2<-res[['20180302']]$mobility.pattern
needs(flexclust)
mdist<-flexclust::dist2(normalise_pattern(p1,norm.vals),normalise_pattern(p2,norm.vals), method="euclidean", p=2)
needs(RcppHungarian)
soln<-RcppHungarian::HungarianSolver(mdist)
soln

p1 <- p1%>%filter(!is.na(datetime))%>%top_n(4, size)
p2<- p2%>%filter(!is.na(datetime))%>%top_n(4, size)

#smint::closest(X=as.matrix(normalise_pattern(p1,norm.vals)),XNew=as.matrix(normalise_pattern(p2,norm.vals)))

source(file.path(dir,"TRA-2020-functions.R"))
pattern_distance(p1,p2,norm.vals,fsize)


for (d1 in names(res)){
  print(d1)
  for (d2 in names(res)){
    if (d1!=d2){
      if (dist[d2,d1]>0){
        dist[d1,d2]<-dist[d2,d1]
      }else{
        val<-pattern_distance(res[[d1]]$mobility.pattern,res[[d2]]$mobility.pattern,norm.vals,fsize)
        dist[d1,d2]<-val
      }
    }
  } 
}



saveRDS(dist,paste0(folder,"distances-all6-0.rds"))
dist<-readRDS(paste0(folder,"distances-all6-0.rds"))

hc <- hclust(as.dist(dist), method = "complete" )
plot(hc)
ncl<-3
d<-cutree(hc, k=ncl)

kc <- kmeans(as.dist(dist), ncl)
d<-kc$cluster

plot_2d(data = dist, clusters = as.vector(kc$cluster), centroids_medoids = as.matrix(kc$withinss))
Optimal_Clusters_KMeans(dist,max_clusters=20)
summary(kc)

fviz_nbclust(dist, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(dist, kmeans, method = "silhouette", k.max=20)+
  labs(subtitle = "Silhouette method")

set.seed(123)
fviz_nbclust(dist, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

Optimal_Clusters_GMM(dist, max_clusters = 30, criterion = "BIC", 
                     dist_mode = "maha_dist", seed_mode = "random_subset",
                     km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                     plot_data = T)

dd<-as_tibble(cbind(dt=names(d),cluster=d))%>%mutate(date=as.POSIXct(dt,format="%Y%m%d",tz="UTC"))%>%
  mutate(wd=as.factor(weekdays(date)))
dd%>%group_by(cluster,wd)%>%summarise(n=n())%>%spread("cluster", "n")


#dd%<>%mutate(cluster=ifelse(wd=="Saturday",1,ifelse(wd=="Sunday",2,ifelse(wd=="Friday",3,4))))

dd%>%filter(cluster==1, wd!="Saturday", wd!="Sunday")
dd%>%filter(cluster==1, wd=="Saturday")
dd%>%filter(cluster==3)

interval<-56
ma<-data.frame()
for (i in seq(from=1,to=nrow(dd)-interval, by=7)){
  mo<-dd[i:(i+interval),]
  dt<-dd[i+interval/2,]$date
  l<-list(date=dt)
  for (cl in 1:ncl){
    dts<-mo%>%filter(cluster==cl)%>%select(dt)%>%pull
    l[[paste0("cluster",cl)]]<-sum(dist[dts,dts])/(length(dts)*(length(dts)-1))
  }
  ma<-bind_rows(ma,l)
}
str<-c(cluster1="Weekends / holidays",cluster2="Weekdays, pattern 1",cluster3="Weekdays, pattern 2")
as_tibble(ma)%>%gather(key="cluster", value="distance", -date)%>%mutate(cluster=str[cluster])%>%
  ggplot(aes(y=distance,x=date, group=cluster, color=cluster))+geom_line(size=2)+ylab("Within-cluster distance")

dd%>%mutate(m=month(date))%>%group_by(cluster,m)%>%summarise(n())%>%print(n=50)

dd%>%mutate(m=month(date))%>%group_by(cluster,m,wd)%>%summarise(n=n())%>%spread("m","n")%>%
  filter(cluster!=1)%>%arrange(wd)

dd%>%mutate(m=as.factor(month(date)))%>%group_by(cluster,m)%>%summarise(n=n())%>%spread("m","n")%>%
  filter(cluster!=1)%>%arrange(cluster)
dd%>%mutate(sea=(month(date)%%12)%/%3)%>%group_by(cluster,sea)%>%summarise(n=n())%>%spread("sea","n")%>%
  arrange(cluster)

dd%>%filter(cluster!=3)%>%ggplot(aes(x=date, y=cluster,group=1))+geom_line()



df<-res[["20180130"]]$mobility.pattern%>%mutate(label=ifelse(size>1500,ifelse(is.na(datetime),"back",format(datetime,"%H:%M")),NA))%>%
  mutate(weight=3+round(10*(size-min(size))/(max(size)-min(size))))
m <- leaflet() %>%addTiles()
for(i in 1:nrow(df)){
  m%<>%addPolylines(label=as.character(df[i, "label"]),labelOptions = labelOptions(noHide = T),
                    lat = as.numeric(df[i, c("stop_lat.x", "stop_lat.y")]), 
                    lng = as.numeric(df[i, c("stop_lon.x", "stop_lon.y")]),
                    weight = as.numeric(df[i,"weight"]))%>%
    addCircleMarkers(lat = as.numeric(df[i, c("stop_lat.y")]), 
                        lng = as.numeric(df[i, c("stop_lon.y")]),radius=4,color="#F30")
}
m


df<-mobility%>%filter(card_number=="53372409")%>%mutate(label=ifelse(is.na(datetime),"back",format(datetime,"%H:%M")))
m <- leaflet() %>%addTiles()
for(i in 1:nrow(df)){
  m%<>%addPolylines(label=as.character(df[i, "label"]),labelOptions = labelOptions(noHide = T),
                    lat = as.numeric(df[i, c("stop_lat.x", "stop_lat.y")]), 
                    lng = as.numeric(df[i, c("stop_lon.x", "stop_lon.y")]))%>%
    addCircleMarkers(lat = as.numeric(df[i, c("stop_lat.y")]), 
                     lng = as.numeric(df[i, c("stop_lon.y")]),radius=4,color="#F30")
}
m





# routes <- data[['routes_df']] %>%
# #  slice(which(grepl('a|b', route_id, ignore.case=TRUE))) %>%
#   '$'('route_id')
# 
# 
# cols<-routes
# cols<-cols%>%replace(startsWith(.,prefix="riga_bus"),"blue")%>%replace(startsWith(.,prefix="riga_tram"),"red")%>%replace(startsWith(.,prefix="riga_trol"),"green")
# 
# m<-data %>% map_gtfs(route_ids = routes, route_colors = cols, include_stops = F, route_opacity = 0.5)
# m$height <- '1000'
# options(viewer = NULL)
# 
# Sys.setlocale(category = "LC_ALL", locale = "") 
# 
# m%>% leaflet::clearControls() %>% leaflet::addCircleMarkers(
#   label = stops$stop_name,
#   radius = 2,
#   stroke = TRUE,
#   opacity = 0.5,
#   weight = 1,
#   color = 'grey',
#   fill = TRUE,
#   fillColor = "pink",
#   fillOpacity = 0.9,
#   lat = stops$stop_lat,
#   lng = stops$stop_lon)
# 
# 
# 
# ids <- data$trips_df %>%
#   select(trip_id,route_id, service_id, shape_id) %>%
#   distinct() %>%
#   filter(route_id %in% routes)
# 
# ids %<>% filter(route_id=="riga_tram_11")
# 
# route_ids <- ids$route_id[1]
# service_ids <- ids$service_id[1]
# shape_ids <- ids$shape_id[1]
# 
# stop_ids<-data[['stop_times_df']]%>%filter(trip_id==ids$trip_id[1])%>%pull(stop_id)
# stops<-data[['stops_df']]%>%filter(stop_id %in% stop_ids)
# # lets map the specific data with some other options enabled.
# m<-data %>%
#   map_gtfs(route_ids = route_ids,
#            service_ids = service_ids,
#            shape_ids = shape_ids,
#            route_colors = 'blue', # set the route color
#            stop_details = TRUE, # get more stop details on click
#            route_opacity = .5, include_stops = F)
# 
# m$height <- '1000'
# 
# m%>% leaflet::clearControls() %>% leaflet::addCircleMarkers(
#   label = stops$stop_name,
#   radius = 2,
#   stroke = TRUE,
#   opacity = 0.5,
#   weight = 1,
#   color = 'grey',
#   fill = TRUE,
#   fillColor = "pink",
#   fillOpacity = 0.9,
#   lat = stops$stop_lat,
#   lng = stops$stop_lon)
# 
# 
# 
# stat <- read_delim(paste0(getwd(),"/RigasKarte/2018-summary.csv"),"\t", col_types = "ciii")
# needs(tidyr)
# needs(ggplot2)
# stat%>%gather(key="mode", value="passengers", -month)
# ggplot(data=stat%>%gather(key="mode", value="passengers", -month), aes(x=month, y=passengers, group=mode, fill=mode)) +
#   scale_x_discrete(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   geom_area(alpha=0.6)+scale_fill_manual(values=c("blue", "red", "green"))
