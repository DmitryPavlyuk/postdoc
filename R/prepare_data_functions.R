require(needs)
needs(imputeTS)
needs(anomalize)
needs(tidyquant)
needs(tibbletime)
needs(xml2)

LoadConfigData <- function(config_file){
  config <- read_xml(gzfile(config_file))
  
  values <-
    lapply(xml_find_all(config, "//corridor"), function(corridor) {
      nSeq <- 0
      newrows <- lapply(xml_children(corridor), function(r_node) {
        cAttrs <- xml_attrs(corridor)
        names(cAttrs) <- paste("corridor", names(cAttrs), sep = "_")
        rAttrs <- xml_attrs(r_node)
        names(rAttrs) <- paste("node", names(rAttrs), sep = "_")
        nSeq <<- nSeq + 1
        rAttrs <- c(rAttrs, node_seq_number = nSeq)
        dat <- c(cAttrs, rAttrs)
        detectors <- lapply(xml_children(r_node), function(detector) {
          dAttrs <- xml_attrs(detector)
          names(dAttrs) <- paste("detector", names(dAttrs), sep = "_")
          return(as.list(c(dat, dAttrs)))
        })
        if (length(detectors) > 0) {
          return(detectors)
        } else {
          res <- list()
          res[[1]] <- as.list(dat)
          return(res)
        }
      })
      newrows <- unlist(newrows, recursive = F)
      return(newrows)
    })
  config.tibble <- bind_rows(unlist(values, recursive = F)) %>%
    mutate(
      corridor_name = paste(corridor_route, corridor_dir),
      corridor_dir = as.factor(corridor_dir),
      detector_lane = as.integer(detector_lane),
      node_lon = as.double(node_lon),
      node_lat = as.double(node_lat),
      node_lanes = as.integer(node_lanes),
      node_shift = as.integer(node_shift),
      node_s_limit = as.integer(node_s_limit),
      node_seq_number = as.integer(node_seq_number),
      node_n_type = as.factor(node_n_type),
      node_transition = as.factor(node_transition),
      node_attach_side = as.factor(node_attach_side),
      detector_category = as.factor(detector_category),
      node_active = ifelse(node_active == "f", F),
      node_pickable = ifelse(node_pickable == "t", T),
      detector_abandoned = ifelse(detector_abandoned == "t", T)
    )
  return(config.tibble)
}


load_detector_data <- function(detector_name, folder, verbose=F){
  n <- 25*60*2 #maximum - 25 hrs a day
  res <- list()
  if (!is.na(detector_name) & length(detector_name)>0){
    #    print(paste("Loading", detector_name))
    vFile <-paste0(folder,"\\",detector_name,".v30")[1]
    if (file.exists(vFile)){
      res[[paste0(detector_name,"_volume")]] <- 
        readBin(vFile, what="int", size=1, signed=F, n=n)
    }else{
      if (verbose) warning(paste("No volume file for detector", detector_name))
    }
    cFile <- paste0(folder,"\\",detector_name,".c30")[1]
    if (file.exists(cFile)){
      res[[paste0(detector_name,"_occupancy")]] <- 
        readBin(cFile, what="int", size=2, signed=F, n=n,endian = "big")
    }else{
      if (verbose) warning(paste("No occupancy file for detector", detector_name))
    }
  }else{
    if (verbose) ("Empty detector name")
  }
  return(res)
}



combine_detectors<-function(data,detector_data){
  d<-lapply(data$detector_name, function(detector_name){
    return(detector_data[[detector_name]])
  })
  if (length(d)>0){
    tib <- bind_cols(d) 
    if(!is_empty(tib)) {
      tib <- tib%>%na_if(-1)
      d.tib <- bind_cols(
        tib %>% select(matches("_volume")) %>% 
          mutate(volume=rowSums(.))%>%select(volume),
        tib %>% select(matches("_occupancy")) %>% 
          mutate(occupancy=rowSums(.)/(ncol(.)*18)) %>% 
          select(occupancy))
    }
  }
}

load_node_data <- function(config, data_path, datadate, verbose=F){
  if (verbose) print(paste("Loading raw data",datadate))
  data_file <- file.path(data_path,paste0(datadate,".traffic"))
  data_folder <- file.path(data_path,datadate)
  if (!dir.exists(data_folder)){
    if (verbose) print(paste("Unzipping ",data_file))
    unzip(data_file, exdir=data_folder)
  }
  raw.data<-sapply(config$detector_name, load_detector_data, folder=data_folder, verbose=F)
  raw.data[sapply(raw.data, is_empty)] <- NULL
  if (verbose) print(paste("Raw data loaded. Number of detectors:",length(raw.data)))
  if (verbose) print("Combining data to nodes")
  res.by <- by(config,select(config,node_name),combine_detectors,detector_data=raw.data, simplify=T)
  res.list <- unlist(res.by, recursive=F)
  res.list[sapply(res.list, is_empty)] <- NULL
  res.tib<-as_tibble(res.list)
  if (verbose) print(paste("Node data combined. Number of time series",ncol(res.tib)))
  time.labels <- format(seq(as.POSIXct(paste0(datadate,"00:00:30"), format="%Y%m%d %H:%M:%S",tz="GMT"), length.out=2880, by='30 sec'), '%Y-%m-%d %H:%M:%S')
  res.tib<-add_column(res.tib,datetime=time.labels,.before=1)
  return(res.tib)
}

TransformToCSV <- function(config, folder, csv_folder,verbose=F){
  if(!dir.exists(csv_folder)) dir.create(csv_folder)
  files <- list.files(folder, pattern = "\\.traffic$")
  for (file in files){
    datadate<-tools::file_path_sans_ext(file)
    csv_file <- file.path(csv_folder,paste0(datadate, ".csv"))
    if (!file.exists(csv_file)){
      res<-load_node_data(config, folder,datadate, verbose = verbose)
      if(verbose) print(paste("Saving",csv_file))
      write_csv(res, csv_file) 
    }else{
      if(verbose) print(paste("CSV file exists. Skipping", datadate))
    }
  }
}

load_csv_data <- function(csv_path, startdate, days, verbose=F){
  res <- tibble()
  for (day in as.character(seq(as.Date(startdate, format="%Y%m%d"), by = "day", length.out = days))){
    datadate <- gsub('-','',day)
    if (verbose) print(paste("Reading and merging data for ", day))
    x <- read_csv(file.path(csv_path,paste0(datadate,".csv")),col_types = cols(.default = "d", datetime="?"))
    res<-bind_rows(res,x)
  }
  return(res)
}

ggplot_missing <- function(x){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var1,
               y = Var2)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.y=element_blank())+
    #theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(y = "Nodes",
         x = "Time")
}


AggregateData <- function(tib,agg.size){
  agg.id<-sort(rep(seq(from=1, to=nrow(tib)/agg.size),agg.size))
  tib<-add_column(tib, agg.id=agg.id)
  res <-bind_cols(
    tib%>% group_by(agg.id) %>% select(agg.id,datetime)%>% summarise_all(funs(last))%>%select(-agg.id),
    tib%>% group_by(agg.id) %>% select(agg.id,ends_with(".volume"))%>% summarise_all(funs(sum))%>%select(-agg.id),
    tib%>% group_by(agg.id) %>% select(agg.id,ends_with(".occupancy"))%>% summarise_all(funs(mean))%>%select(-agg.id))
  colnames(res)[which(colnames(res) == "datetime_last")]<-"datetime"
  cols<-grepl('_mean',colnames(res))
  colnames(res)[grepl('_mean',colnames(res))] <- gsub("_mean","",colnames(res)[cols])
  cols<-grepl('_sum',colnames(res))
  colnames(res)[cols] <- gsub("_sum","",colnames(res)[cols])
  return(res)
}




load_all_links <-function(config){
  config.node.list<-config%>%split(.$node_name_seq)
  res <- list()
  prev_node <- NULL
  curlimit<-NA
  for(node in config.node.list){
    if (!is.null(prev_node)){
      if (node$corridor_name==prev_node$corridor_name & 
          node$node_seq_number==(prev_node$node_seq_number+1)){
        if (!is.na(prev_node$node_s_limit)) curlimit<-prev_node$node_s_limit
        if (is.na(curlimit)) curlimit<-55 #default limit. Bad
        avspeed<-1609.34*(ifelse(is.na(prev_node$node_s_limit),curlimit,prev_node$node_s_limit)+
                            ifelse(is.na(node$node_s_limit),curlimit,node$node_s_limit))/(2*60) #average speed, m per minute
        d<-distHaversine(c(prev_node$node_lon,prev_node$node_lat),c(node$node_lon,node$node_lat))
        res[[as.character(length(res)+1)]]<-list(from=prev_node$node_name,
                                                 to=node$node_name,
                                                 distance=d,
                                                 time=d/avspeed)
        if(!is.na(node$node_forks)){
          f <- config%>%filter(node_name==node$node_forks)
          if (!is.null(f) & (nrow(f)>0)){
            d<-distHaversine(c(node$node_lon,node$node_lat),c(f$node_lon,f$node_lat))
            res[[as.character(length(res)+1)]]<-list(from=node$node_name,
                                                     to=f$node_name,
                                                     distance=d,
                                                     time=d/avspeed)
          }
        }
      }else{
        curlimit<-NA
      }
    }
    prev_node <- node
  }
  return(bind_rows(res))
}




find_paths <- function(nodes, all.links, verbose=F){
  paths <- list()
  for (node in nodes){
    paths[[as.character(length(paths)+1)]] <-list(from=node, to=NA, distance=0,time=0,intermediate=c(),completed=F)
  }
  foundNew<-T
  c<-0
  while(foundNew){
    c<-c+1
    if (verbose) print(paste("Iteration ",c,", paths", length(paths))) 
    foundNew<-F
    for (ind in names(paths)) {
      path<-paths[[ind]]
      if(!path$completed){
        path.tail<-ifelse(is.na(path$to),path$from, path$to)
        next.con<-all.links%>%filter(from==path.tail)
        if (nrow(next.con)==0){
          path$completed<-T
          path$intermediate<-NULL
          paths[[ind]]<-path
          next
        }
        for (row in 1:nrow(next.con)) {
          to <- next.con[row,]$to
          if (to %in% path$intermediate){
            paths[[ind]]<-NULL
            next
          }
          dist <- next.con[row,]$distance 
          ti <- next.con[row,]$time 
          nextind<-ifelse(row==1,ind,as.character(length(paths)+1))
          completed <- (to %in% nodes)
          paths[[nextind]] <-list(from=path$from, to=to, distance=dist+path$distance, time=ti+path$time,
                                  intermediate=c(path$intermediate,path.tail), completed=completed)
          if (completed) paths[[nextind]]$intermediate<-NULL
          else foundNew<-T
        }
      }
    }
  }
  return(bind_rows(paths)%>%filter(from %in% nodes & to %in% nodes))
}

CombineToNodes <- function(config){
  return(config %>% group_by(node_name) %>% 
           summarise_all(funs(first))%>%select(-starts_with("detector"))%>%
           arrange(corridor_name,node_seq_number)%>%
           mutate(node_name_seq=paste0(corridor_name,"_",sprintf("%04d", node_seq_number))))
}

prepare_matrix<-function(paths,nodes, useTime=T){
  n<-length(nodes)
  A<-matrix(nrow=n,ncol=n)
  rownames(A)<-nodes
  colnames(A)<-nodes
  for (row in 1:nrow(paths)) {
    A[[paths[row,]$from,paths[row,]$to]]<-ifelse(useTime,paths[row,]$time,paths[row,]$distance)
  }
  A<-replace(A, is.na(A),0)
  return(A)
}

DropOrImpute <- function(tib, imputeLimit=0.05,verbose=F){
  res<-tib
  distr.na<-sapply(tib, function(x) mean(is.na(x)))
  ns<-names(distr.na[distr.na>0])
  ns<-gsub(".volume","",ns)
  ns<-gsub(".occupancy","",ns)
  ns<-unique(ns)
  for(node in ns){
    nodeVolume<-paste(node,".volume",sep="")
    nodeOccupancy<-paste(node,".occupancy",sep="")
    if(is.na(distr.na[nodeVolume]) || is.na(distr.na[nodeOccupancy]) || 
       distr.na[nodeVolume]>imputeLimit ||
       distr.na[nodeOccupancy]>imputeLimit){
      if (verbose) print(paste("Dropping ",nodeVolume,"- missing ratio",distr.na[nodeVolume]))
      if (nodeVolume %in% colnames(res)) res<-res%>%select(-c(nodeVolume))
      if (verbose) print(paste("Dropping ",nodeOccupancy,"- missing ratio",distr.na[nodeOccupancy]))
      if (nodeOccupancy %in% colnames(res)) res<-res%>%select(-c(nodeOccupancy))
    }else{
      if (verbose) print(paste("Imputing ",nodeVolume,"- missing ratio",distr.na[nodeVolume]))
      res[[nodeVolume]]<-na.interpolation(c(res[[nodeVolume]]))
      
      if (verbose) print(paste("Imputing ",nodeOccupancy,"- missing ratio",distr.na[nodeOccupancy]))
      res[[nodeOccupancy]]<-na.interpolation(c(res[[nodeOccupancy]]))
    }
  }
  return(res)
}


LoadMassiveData <-function(csv_path, start_date, number_of_days, block_size=4*7){
  tib <- tibble()
  rem <- number_of_days
  read_from <- start_date
  while(rem>0){
    read_block <- ifelse(rem>block_size,block_size,rem)
    sread_from <- format(read_from, format="%Y%m%d",tz="GMT")
    print(paste("Reading date for",read_block,"days, starting from",sread_from))
    tib2 <-load_csv_data(csv_path, sread_from, read_block, verbose=T)
    tib<-bind_rows(tib,tib2)
    rm(tib2)
    gc()
    rem<-rem-read_block
    read_from <- read_from+read_block
  }
  return (tib)
}


submean <-function(x, dow,df){
  name<-quo_name(enquo(x))
  med<-paste0(gsub(".x","",name),".y")
  return(x-df[[med]])
}
subtract_dow_means <- function(data, means){
  data.join <- data %>% left_join(means, by=c("dow_time"="dow_time"))
  data.join%>%mutate_at(vars(contains(".x")),funs(submean(x=.,dow=dow_time,df=data.join)))%>%
    select(-ends_with(".y"))%>%rename_all(. %>% gsub(".x","",.))
}

filterAnomalies <- function(data, frequency, alpha=0.01){
  data%>%as_tbl_time(index=datetime)%>%
    time_decompose(value,frequency = frequency)%>%
    anomalize(remainder,alpha = alpha)%>%
    mutate(prepared=ifelse(anomaly=="Yes",0,remainder))%>%
    select(datetime,node,prepared)
}

prepare_shortest_distances<-function(nodes, all.links, useTime=T){
  g <-  GetGraph(nodes, all.links)
  shortestA <- shortest.paths(g, mode="out")
  shortestA [shortestA==0] <- Inf
  return(shortestA)
}

GetGraph<-function(nodes, all.links){
  res<-find_paths(nodes, all.links)
  A<-prepare_matrix(res,nodes)
  g <-  graph.adjacency(A, mode="directed", weighted=TRUE)
  return(g)
}

GetNetwork<-function(node, shortestA, radius){
  node<-c(node)
  next_nodes<-shortestA[node,order(shortestA[node,])]
  prev_nodes<-shortestA[order(shortestA[,node]),node]
  
  return(c(prev_nodes[prev_nodes<radius],next_nodes[next_nodes<radius]))
}

CalculateShortestDistances <-function(config.nodes){
  all.links<-load_all_links(config.nodes)
  allnodes<-as.vector(config.nodes%>%distinct(node_name)%>%pull)
  shA<-prepare_shortest_distances(allnodes,all.links,useTime=T)
  return(shA)
}

GetSample <- function(central.node,shortest.distances,max.distance.time, data, var, frequency=7){
  res<- GetNetwork(central.node,shortest.distances,max.distance.time)
  series<-paste0(names(res),".",var)
  dat <- data%>%filter(node %in% series)
  series<-dat%>%distinct(node)%>%unlist
  dat <- filterAnomalies(dat,frequency=frequency*24*60/ta)
  series<-dat%>%group_by(node)%>%summarise(sd=sd(prepared))%>%filter(sd>0.1)%>%select(node)%>%pull
  res<-gsub(paste0(".",var),"",series)
  dat <- dat%>%filter(node %in% series)%>%spread(key = node, value = prepared)
  
  return(list(data=dat, shortest.distances=shortest.distances[res,res]))
}