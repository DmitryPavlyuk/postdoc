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
options(viewer = NULL)

source(file.path("TRA-2020-functions.R"))

set_api_key()
#4cf79c4e-6afc-4e4e-b8bf-1eb6b0d88d1f

raw.folder<-paste0(getwd(),"/RK/Validations-Dump_2018/")
raw.rds.folder<-paste0(getwd(),"/RK/prepared/")
processed.folder<-paste0(getwd(),"/RK/processed/")
gtfs.folder<-paste0(getwd(),"/RK/gtfs/")
daily.folder<-paste0(getwd(),"/RK/daily/")
folder<-paste0(getwd(),"/RK/")

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


my.travels%>%group_by(date)%>%summarize(n=n())%>%print(n=100)

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
  
  stat$matched_trip_count<-data.day%>%ungroup%>%summarise(matched_trips=n_distinct(trip_id.y))%>%select(matched_trips)%>%pull
  
  #data.day%>%filter(card_number=="53372409")%>%arrange(datetime)%>%select(datetime, trip_id, trip_id.y, stop_name)
  
  data.chains<-find_chains(data.day)
  
  #data.chains%>%filter(card_number=="53372409")
  stat$chain_count<-data.chains%>%ungroup%>%summarise(chains=n_distinct(chain_id))%>%select(chains)%>%pull
  
  
  mobility<-find_mobility_vectors(data.chains)
  stat$mobility_vector_count<-mobility%>%nrow
  stat$mobility_total_distance<-mobility%>%ungroup%>%summarise(dist=sum(distance))%>%select(dist)%>%pull
  
  
  #
  mobility%<>%filter(distance>1000)
  #
  
  daily.mobility <- construct_daily_mobility(mobility,day,list(c(5,11),c(11,15),c(15,24),c(NA)),10)
  
  result<-list(stat=stat, mobility.pattern=daily.mobility)
  saveRDS(result,paste0(processed.folder,format(cday,"%Y%m%d"),".rds"))
  cday<-cday+1
}



source(file.path("TRA-2020-functions.R"))

fs <- gsub(".rds","",sort(list.files(processed.folder, pattern="\\.rds$",full.names=F)))
res<-list()
for (file in fs){
  res[[file]]<-readRDS(paste0(processed.folder,file,".rds"))
}
dist<-matrix(0,nrow=length(res), ncol=length(res))
rownames(dist)<-names(res)
colnames(dist)<-names(res)
fsize<-1000
for (d1 in names(res)){
  print(d1)
  for (d2 in names(res)){
    if (d1!=d2){
      if (dist[d2,d1]>0){
        dist[d1,d2]<-dist[d2,d1]
      }else{
        val<-pattern_distance(res[[d1]]$mobility.pattern,res[[d2]]$mobility.pattern,fsize)
        dist[d1,d2]<-val
      }
    }
  } 
}
saveRDS(dist,paste0(folder,"distances.rds"))
hc <- hclust(as.dist(dist), method = "complete" )
plot(hc)
d<-cutree(hc, k=4)
d[d==2]

fviz_nbclust(dist, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(dist, kmeans, method = "silhouette", k.max=10)+
  labs(subtitle = "Silhouette method")

mobility%>%ggplot(aes(distance))+geom_density()
daily.mobility_lim <- construct_daily_mobility(mobility%>%filter(distance>1000),day,list(c(5,11),c(11,15),c(15,24),c(NA)),10)
pattern_distance(daily.mobility,daily.mobility_lim,0)

t1<-df[,c("stop_lat.x","stop_lon.x","stop_lat.y","stop_lon.y")]
t2<-t1
t2[1,]<-t1[4,]
t2[4,]<-t1[2,]
t2[2,]<-t1[1,]
t2<-t2[-c(5),]
t1<-t1[-c(10),]
t2[1,]<-round(t2[1,])
smint::closest(X=as.matrix(t1),XNew=as.matrix(t2))
as.matrix(t2)

bind_rows(res1$stat,res2$stat,res3$stat,res4$stat)
res1<-readRDS(paste0(processed.folder,"20180102.rds"))
res2<-readRDS(paste0(processed.folder,"20180103.rds"))
res3<-readRDS(paste0(processed.folder,"20180106.rds"))
res4<-readRDS(paste0(processed.folder,"20180107.rds"))

source(file.path("TRA-2020-functions.R"))
bind_rows(res1$stat,res2$stat,res3$stat,res4$stat)
fsize<-1000
pattern_distance(res1$mobility.pattern,res2$mobility.pattern,fsize)
pattern_distance(res1$mobility.pattern,res1$mobility.pattern,fsize)

pattern_distance(res1$mobility.pattern,res3$mobility.pattern,fsize)
pattern_distance(res1$mobility.pattern,res4$mobility.pattern,fsize)
pattern_distance(res2$mobility.pattern,res3$mobility.pattern,fsize)
pattern_distance(res2$mobility.pattern,res4$mobility.pattern,fsize)
pattern_distance(res3$mobility.pattern,res4$mobility.pattern,fsize)


res<-readRDS(paste0(processed.folder,"20180103.rds"))

df<-daily.mobility%>%mutate(label=ifelse(is.na(datetime),"return",format(datetime,"%H:%M")))%>%
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


df<-mobility%>%filter(card_number=="53372409")
m <- leaflet() %>%addTiles()
for(i in 1:nrow(df)){
  m%<>%addPolylines(label=as.character(df[i, "datetime"]),labelOptions = labelOptions(noHide = T),
                    lat = as.numeric(df[i, c("stop_lat.x", "stop_lat.y")]), 
                    lng = as.numeric(df[i, c("stop_lon.x", "stop_lon.y")]))%>%
    addCircleMarkers(lat = as.numeric(df[i, c("stop_lat.y")]), 
                     lng = as.numeric(df[i, c("stop_lon.y")]),radius=4,color="#F30")
}
m


needs(factoextra)
needs(NbClust)

fviz_nbclust(vect, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(vect%>%sample_n(1000), kmeans, method = "silhouette", k.max=900)+
  labs(subtitle = "Silhouette method")

#set.seed(123)
#fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

x







needs(dbscan)

# 
# test<-mobility%>%mutate(X1=ifelse(is.na(datetime),0,as.numeric(datetime)))%>%
#   mutate(X2=as.numeric(stop_lat.x))%>%
#   mutate(X3=as.numeric(stop_lon.x))%>%
#   mutate(X4=as.numeric(stop_lat.y))%>%
#   mutate(X5=as.numeric(stop_lon.y))
# 
# vect<-test%>%mutate(X1norm=(X1-mean(X1))/sd(X1))%>%
#   mutate(X2norm=(X2-mean(X2))/sd(X2))%>%mutate(X3norm=(X3-mean(X3))/sd(X3))%>%mutate(X4norm=(X4-mean(X4))/sd(X4))%>%mutate(X5norm=(X5-mean(X5))/sd(X5))%>%
#   select(X1norm,X2norm,X3norm,X4norm,X5norm)
# 
# kNNdistplot(vect, k = 20)
# res <- dbscan(vect, eps = .5, minPts = 20)
# res
# test$cluster<-res$cluster
# test%<>%group_by(cluster)%>%summarize(stop_lat.x=mean(stop_lat.x,na.rm=T),stop_lon.x=mean(stop_lon.x,na.rm=T),
#                                       stop_lat.y=mean(stop_lat.y,na.rm=T),stop_lon.y=mean(stop_lon.y,na.rm=T),
#                                       size=n(),
#                                       datetime=median(datetime))






routes <- data[['routes_df']] %>%
#  slice(which(grepl('a|b', route_id, ignore.case=TRUE))) %>%
  '$'('route_id')


cols<-routes
cols<-cols%>%replace(startsWith(.,prefix="riga_bus"),"blue")%>%replace(startsWith(.,prefix="riga_tram"),"red")%>%replace(startsWith(.,prefix="riga_trol"),"green")

m<-data %>% map_gtfs(route_ids = routes, route_colors = cols, include_stops = F, route_opacity = 0.5)
m$height <- '1000'
options(viewer = NULL)

Sys.setlocale(category = "LC_ALL", locale = "") 

m%>% leaflet::clearControls() %>% leaflet::addCircleMarkers(
  label = stops$stop_name,
  radius = 2,
  stroke = TRUE,
  opacity = 0.5,
  weight = 1,
  color = 'grey',
  fill = TRUE,
  fillColor = "pink",
  fillOpacity = 0.9,
  lat = stops$stop_lat,
  lng = stops$stop_lon)



ids <- data$trips_df %>%
  select(trip_id,route_id, service_id, shape_id) %>%
  distinct() %>%
  filter(route_id %in% routes)

ids %<>% filter(route_id=="riga_tram_11")

route_ids <- ids$route_id[1]
service_ids <- ids$service_id[1]
shape_ids <- ids$shape_id[1]

stop_ids<-data[['stop_times_df']]%>%filter(trip_id==ids$trip_id[1])%>%pull(stop_id)
stops<-data[['stops_df']]%>%filter(stop_id %in% stop_ids)
# lets map the specific data with some other options enabled.
m<-data %>%
  map_gtfs(route_ids = route_ids,
           service_ids = service_ids,
           shape_ids = shape_ids,
           route_colors = 'blue', # set the route color
           stop_details = TRUE, # get more stop details on click
           route_opacity = .5, include_stops = F)

m$height <- '1000'

m%>% leaflet::clearControls() %>% leaflet::addCircleMarkers(
  label = stops$stop_name,
  radius = 2,
  stroke = TRUE,
  opacity = 0.5,
  weight = 1,
  color = 'grey',
  fill = TRUE,
  fillColor = "pink",
  fillOpacity = 0.9,
  lat = stops$stop_lat,
  lng = stops$stop_lon)



stat <- read_delim(paste0(getwd(),"/RigasKarte/2018-summary.csv"),"\t", col_types = "ciii")
needs(tidyr)
needs(ggplot2)
stat%>%gather(key="mode", value="passengers", -month)
ggplot(data=stat%>%gather(key="mode", value="passengers", -month), aes(x=month, y=passengers, group=mode, fill=mode)) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_area(alpha=0.6)+scale_fill_manual(values=c("blue", "red", "green"))
