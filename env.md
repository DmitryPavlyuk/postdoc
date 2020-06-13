---
title: "Configure general environment for all markdowns"
author: "Dmitry Pavlyuk"
date: "February 6, 2019"
output: html_document
---

# Configuration



```r
# Downloaded data folder
dir.create(file.path(getwd(),"data"), showWarnings = FALSE)

data.folder.downloaded <- file.path(getwd(),"data","downloaded")
dir.create(data.folder.downloaded, showWarnings = FALSE)
data.folder.tmp <- file.path(getwd(),"data","tmp")
dir.create(data.folder.tmp, showWarnings = FALSE)


# Transformed data folder
data.folder.prepared <- file.path(getwd(),"data","prepared")
dir.create(data.folder.prepared, showWarnings = FALSE)

# Local config folder
config.folder <- file.path(getwd(),"data","config")
dir.create(config.folder, showWarnings = FALSE)

### Configure remote settings
download.website <- "http://www.d.umn.edu/tdrl/traffic/"

# Setup the first date for downloading
download.start.date <- as.Date("20170101", format="%Y%m%d")

# Setup number of days for downloading
download.days <- 7*52

# Define remote file names
download.files <- paste0(format(seq(download.start.date, by = "day", length.out = download.days),
                                format="%Y%m%d", tz = "UTC"),".traffic")

# Setup detector network config url
config.filename <- "metro_config.xml"
config.filename.gz <- paste0(config.filename,".gz")
config.url<-paste0("http://data.dot.state.mn.us/iris_xml/",config.filename.gz)
config.file.gz <- file.path(config.folder, config.filename.gz)
config.file <- file.path(config.folder, config.filename)

# Define local file names
config.rds <- file.path(data.folder.prepared, "config.rds")
data.rds.file <- "working-raw.rds"
block.size <- 7 # save 1 week per block

impute.limit <- 0.2
```
