library(tidyverse)
library(lubridate)
library(astroFns)
source("Codes/Functions/Data_preparation.R")
source("Codes/Functions/get_sunmoon_time.R")
sampling_interval=1

record_table=read_rds("Input/recordtable_master_withdistance_270921")
defunct_dates=read.csv("Input/defunct_dates.csv")
sun_moon_times=get_sunmoon_data(start_date=min(read.csv("Input/effort.csv")$effort_start),
                                end_date=max(read.csv("Input/effort.csv")$effort_end),lat=27,long=71)
effort_data=effort(record_table,defunct_dates)%>%
  mutate(effort_start=as.POSIXct(effort_start))%>%
  mutate(effort_end=as.POSIXct(effort_end))%>%
  mutate(hours=as.numeric(effort_end-effort_start,units="hours"))%>%
  group_by(Grid)%>%
  mutate(effort_hours=sum(hours), effort_secs = sum(hours) * 3600)%>%
  ungroup()
write.csv(effort_data,"Input/effort_data.csv")

detection_data=read_rds("Input/data_cleaned_targetanimalsonly")
flatfile=sample_n(detection_data,effort_data,sampling_interval)%>%
  mutate(DateTimeOriginal=as.POSIXct(DateTimeOriginal,tz = "Asia/Calcutta"))%>%
  mutate(Date=as.Date(DateTimeOriginal))%>%
  left_join(sun_moon_times)%>%
  mutate(DN="NA")%>%
  mutate(DN=ifelse(DateTimeOriginal>sunrise & DateTimeOriginal<sunset,"Day","Night"))%>%
  dplyr::select(-sunrise:-set)%>%group_by(Species,DN)%>%
  mutate(fill_na=ifelse(is.na(distance),TRUE,FALSE))%>%
  mutate(distance=ifelse(is.na(distance)==T, mean(distance,na.rm = T), distance))%>%
  ungroup()%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  arrange(Sample.Label,DateTimeOriginal)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(DateTimeOriginal, "%Y-%m-%d %H:%M:%S") - lag(strptime(DateTimeOriginal, "%Y-%m-%d %H:%M:%S"), 
                                                                                 default = strptime(DateTimeOriginal, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()%>%
  filter(!is.na(DateTimeOriginal))
#write.csv(flatfile,"Input/flatfile_1sec.csv")

#record_table=read_rds("Input/data_cleaned_targetanimalsonly")
#record_table%>%filter(Species=="DESERT_CAT")%>%View()
