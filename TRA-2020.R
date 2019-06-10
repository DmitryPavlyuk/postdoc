rm(list=ls())
memory.limit(size=1024*128)

if (!require(devtools)) {
  install.packages('devtools')
}
if (!require(needs)) {
  install.packages('needs')
}
devtools::install_github('ropensci/gtfsr')
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
folder<-"C:/Users/dmitr/Desktop/Validations-Dump_2018/"
data<-tibble()
for (file in list.files(filder)){
  tickets<-read_delim(paste0(folder,file),";", col_types = "Tcccicccccc", 
                      col_names=c("datetime","tick","route_name", "route_direction","transport_id","ticket_type","mode","card_number","route_number","transport_code","code"))
  data <- bind_rows(data,tickets)
}
summary(tickets)
mode_ass<-c(Tramway="tram", `Urban Bus`="bus", Trolleybus="trol")
tickets$mode<-as.factor(tickets$mode)
tickets%<>%mutate(date=format(datetime-3*60*60, format="%Y%m%d",tz="GMT"))%>%mutate(route_id=paste("riga",mode_ass[mode],route_number,sep="_"))

tickets%>%group_by(card_number)%>%summarise(n=n())%>%filter(n>1)
tickets%>%filter(card_number=="140873008564683")

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
