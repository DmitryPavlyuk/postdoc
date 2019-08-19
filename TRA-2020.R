rm(list=ls())
memory.limit(size=1024*128)

if (!require(devtools)) {
  install.packages('devtools')
}
if (!require(needs)) {
  install.packages('needs')
}
devtools::install_github('ropensci/gtfsr')
require(needs)
needs(gtfsr)
needs(magrittr)
needs(dplyr)
needs(tibble)
needs(readr)


set_api_key()
#4cf79c4e-6afc-4e4e-b8bf-1eb6b0d88d1f

# get the feedlist dataframe
feedlist_df <- get_feedlist() %>%filter(grepl('Riga', id, ignore.case= TRUE))
data <- import_gtfs(feedlist_df$url_d)

data <- import_gtfs("https://openmobilitydata-data.s3-us-west-1.amazonaws.com/public/feeds/rigas-satiksme/333/20180129/gtfs.zip")


#tickets<-read_delim(paste0(getwd(),"/RigasKarte/export2.csv"),";", col_types = "Tccicccic")
folder<-paste0(getwd(),"/RK/Validations-Dump_2018/")
data<-tibble()
c <- 1
for (file in list.files(folder)){
  print(file)
  tickets<-read_delim(paste0(folder,file),";", col_types = "Tcccicccccc", 
                      col_names=c("datetime","tick","route_name", "route_direction","transport_id","ticket_type","mode","card_number","route_number","transport_code","code"))
  data <- bind_rows(data,tickets)
  c<-c+1
  if (c %% 10 == 0){
    saveRDS(data, paste0(folder,"data",c,".rds"))
    data<-tibble()
  }
}
saveRDS(data, paste0(folder,"data",c,".rds"))
data<-tibble()
for (file in list.files(folder, pattern="\\.rds$")){
  print(file)
    d<-readRDS(paste0(folder,file))
    data <- bind_rows(data,d)
}
saveRDS(data, paste0(folder,"complete.rds"))

data <- readRDS(paste0(folder,"complete.rds"))

sample<-data[1:10000,]
saveRDS(sample, paste0(folder,"sample.rds"))

needs(lubridate)
data%<>%mutate(date=ymd(format(datetime-3*60*60, format="%Y%m%d",tz="GMT")))
saveRDS(data, paste0(folder,"complete.rds"))

data.daily<-data%>%group_by(date)%>%summarize(n=n())
data.day <- data%>%filter(date>ymd(20180610),date<ymd(20180618))
data.day%<>%mutate(mode=ifelse(mode=="Train" & as.integer(route_number)>100, "Urban bus", mode))
data.day%<>%filter(mode!="Train")
data.day%>%filter(mode=="Indeterminated")%>%select(route_name, route_number)%>%print(n=1000)
data.day%<>%mutate(mode=ifelse(mode=="Indeterminated", "Tramway", mode))


data.day%<>%mutate(minute=round_date(datetime,"10 mins"))
needs(ggplot2)
ggplot(data=data.day%>%group_by(minute)%>%summarize(n=n()), aes(x=minute, y=n)) +  geom_line()
ggplot(data=data.day%>%group_by(date,minute, mode)%>%summarize(n=n()), aes(x=minute, y=n, group=mode, color=mode)) +  geom_line()



summary(data.day)
mode_ass<-c(Tramway="tram", `Urban bus`="bus", Trolleybus="trol")
data.day$mode<-as.factor(data.day$mode)
data.day%<>%mutate(route_id=paste("riga",mode_ass[mode],route_number,sep="_"))

data.day%>%group_by(card_number)%>%summarise(n=n())%>%arrange(desc(n))%>%filter(n>1)%>%print(n=100)
data.day%>%arrange(datetime)%>%filter(card_number=="2030760589")%>%print(n=50)




data[['routes_df']] <- data[['routes_df']]%>%rename(route_id=1)
data[['trips_df']] <- data[['trips_df']]%>%rename(route_id=1)
data[['stops_df']] <- data[['stops_df']]%>%rename(stop_id=1)
data[['stop_times_df']] <- data[['stop_times_df']]%>%rename(trip_id=1)
data[['shapes_df']] <- data[['shapes_df']]%>%rename(shape_id=1)
data[['calendar_df']] <- data[['calendar_df']]%>%rename(service_id=1)
data[['calendar_dates_df']] <- data[['calendar_dates_df']]%>%rename(service_id=1)
data[['agency_df']] <- data[['agency_df']]%>%rename(agency_id=1)%>%mutate(agency_name="Rigas Satiksme")


tickets%<>%left_join(routes_df,by=c("route_id"))
tickets%>%group_by(produit)%>%summarise(first_validation=min(datetime), last_validation=max(datetime))

tickets%>%filter(produit==31212 )%>%arrange(produit, datetime)%>%mutate(sw=(lag(direction)==direction))%>%print(n=40)


%>%select(datetime,route_name,route_id, date, service_id)


data[['routes_df']]%>%filter(route_id=="riga_tram_11")

routes <- data[['routes_df']] %>%
#  slice(which(grepl('a|b', route_id, ignore.case=TRUE))) %>%
  '$'('route_id')


cols<-routes
cols<-cols%>%replace(startsWith(.,prefix="riga_bus"),"blue")%>%replace(startsWith(.,prefix="riga_tram"),"red")%>%replace(startsWith(.,prefix="riga_trol"),"green")

m<-data %>% map_gtfs(route_ids = routes, route_colors = cols, include_stops = F, route_opacity = 0.5)
m$height <- '1000'
options(viewer = NULL)


data[['stops_df']]%>%inner_join(data[['stop_times_df']],by = "stop_id")%>%inner_join(data[['trips_df']],by = "trip_id")%>%group_by(stop_id, stop_name)%>%
  summarise(n=n())

stops <- data[['stops_df']]

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
