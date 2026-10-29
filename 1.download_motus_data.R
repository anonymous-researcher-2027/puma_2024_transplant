##################################################################################################
#
# script to download puma motus detection data
#
##################################################################################################

# 1) Load libraries

# not all of libraries are needed now; some hashtagged
library(remotes)
library(motus)
#library(motusData)
library(maps)
library(tidyverse)
#library(rworldmap)
library(ggmap)
library(lubridate)
library(curl)
library(DBI)
library(RSQLite)
library(dbplyr)

##################################################################################################
##################################################################################################

# 2) Load data

# set working directory, data will be stored here
setwd("E:\\Work\\2025\\Motus_download_filtering\\Clean")

# set time to utc
Sys.setenv(TZ = "UTC")

# project number for this work
proj.num <- 783

# enter motus login info
sql.motus <- tagme(projRecv = proj.num, new = T, update = TRUE) 

##################################################################################################
##################################################################################################
# 3) filter data

#motus data filtered:
tbl_motusFilter <- filterByActivity(sql.motus, return = "all")


df.alltags.sub <- tbl_motusFilter %>% 
  filter(probability == 1) %>%
  collect() %>%
  as.data.frame() %>%
  mutate(ts = as_datetime(ts),  # work with dates AFTER transforming to flat file
         tagDeployStart = as_datetime(tagDeployStart),
         tagDeployEnd = as_datetime(tagDeployEnd))

df.alltags.sub_year <- df.alltags.sub %>%
  mutate(ts = as_datetime(ts, tz = "UTC"), # convert ts to POSIXct format
         detectyear = year(ts), # extract year from ts
         detectdoy = yday(ts),
         detecttime=format(ts,"%H:%M:%S"))%>% # extract numeric day of year from ts
  filter(!is.na(recvDeployLat))

write.csv(df.alltags.sub_year,"1.motus_prelim_filters.csv",row.names=F)


saveRDS(df.alltags.sub_year, "1.motus_prelim_filters.rds")


## manual filtering as necessary, e.g., impossible detections