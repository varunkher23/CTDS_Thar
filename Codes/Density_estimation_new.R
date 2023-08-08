library(tidyverse)
library(lubridate)
library(astroFns)
library(activity)
library(Distance)
source("Codes/Functions/Data_preparation.R")
source("Codes/Functions/get_sunmoon_time.R")
sampling_interval=5

#effort=effort("Input/recordtable_master_withdistance_270921","Input/defunct_dates.csv")
effort=read.csv("Input/effort.csv")%>%
  mutate(effort_start=as.POSIXct(effort_start))%>%
  mutate(effort_end=as.POSIXct(effort_end))%>%
  mutate(hours=as.numeric(effort_end-effort_start,units="hours"))%>%
  group_by(Grid)%>%
  summarise(effort_hours=sum(hours), effort_secs = sum(hours) * 3600)%>%
  ungroup()%>%
  mutate(sampling_effort= as.integer(effort_secs / sampling_interval))
#write.csv(effort,"Input/effort.csv")

sun_moon_times=get_sunmoon_data(start_date=min(read.csv("Input/effort.csv")$effort_start),
                                end_date=max(read.csv("Input/effort.csv")$effort_end),lat=27,long=71)

#flatfile=sample_n("Input/data_cleaned_targetanimalsonly","Input/effort.csv",sampling_interval = 5)
flatfile=read.csv("Input/flatifile_5sec.csv")%>%
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

####detection_function

{detection_matrix=data.frame()
Species = unique(flatfile$Species)}

species = Species[1]
de_data=flatfile%>%filter(Species==species)

breakpoints <- c(0.01,2.5,5,7,10)
hist((de_data%>%filter(fill_na==FALSE))$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

{
  conversion <- convert_units("meter", NULL, "square kilometer")
  trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
  mybreaks <- breakpoints
  
  hn01 <- ds(filter(de_data,fill_na==FALSE & diff > 300), transect = "point", key="hn", adjustment = NULL,
             cutpoints = mybreaks,truncation = trunc.list,formula = ~DN)
  gof_ds(hn01)
}
plot(hn01,pdf=T)
plot(hn01)

{
  detection_matrix_temp=data.frame(Species=species,DN=c("Day","Night"),
                                   p_mean=predict(hn01,data.frame(DN=c("Day","Night")),se=T)$fitted,
                                   p_se=predict(hn01,data.frame(DN=c("Day","Night")),se=T)$se.fit)%>%
    mutate(truncation=list(breakpoints))
  detection_matrix=rbind(detection_matrix,detection_matrix_temp)
}
    
##### Activity correction

{
  activity_correction_data=de_data%>%
    filter(diff>10)%>%
    mutate(timestamp_radian=hms2rad(strftime(DateTimeOriginal,format="%H:%M:%S")))
  activity_species=fitact(activity_correction_data$timestamp_radian,sample="data" )
}
