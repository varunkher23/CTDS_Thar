###Camera trap distance sampling code

#### Calculating effort for each camera trap

effort= function(record_table,defunct_dates){
  #Load packages
  library(tidyverse)
  library(lubridate)
  
  #Load data
  data=record_table
  defunct=defunct_dates%>%
    mutate(defunct_start=as.POSIXct(defunct_start,tryFormats=c("%Y-%m-%d %H:%M:%OS","%d-%m-%Y %H:%M","%d-%m-%Y %H:%M:%OS")))%>%
    mutate(defunct_end=as.POSIXct(defunct_end,tryFormats=c("%Y-%m-%d %H:%M:%OS","%d-%m-%Y %H:%M","%d-%m-%Y %H:%M:%OS")))%>%
    arrange(Grid,defunct_start)%>%
    dplyr::select(Grid:defunct_end)
  
  effort=data.frame()
  for(j in 1:length(unique(data$Grid))){
      grid=unique(data$Grid)[j]
      data2=data%>% filter(Grid==grid)
      defunct2=defunct%>%filter(Grid==grid)
      if (nrow(defunct2)>0) {
          for (k in 1:nrow(defunct2)) {
            if(nrow(defunct2) == 1) {
              effort_start=min(data2$DateTimeOriginal)+3600
              effort_end=defunct2$defunct_start[k] - 3600 ### Buffer
              temp=data.frame(Grid = grid, effort_start = effort_start, effort_end = effort_end)
              effort=rbind(effort,temp)
              effort_start=effort_end + 3600
              effort_end=max(data2$DateTimeOriginal)
              temp=data.frame(Grid = grid, effort_start = effort_start, effort_end = effort_end)
              effort=rbind(effort,temp)
            } else {
              if (k == 1) {
                effort_start=min(data2$DateTimeOriginal)+3600
                effort_end=defunct2$defunct_start[k] - 3600
                temp=data.frame(Grid = grid, effort_start = effort_start, effort_end = effort_end)
                effort=rbind(effort,temp)
              } else if(k == nrow(defunct2)) {
                effort_start=defunct2$defunct_end[k-1] + 3600 ## One hr buffer after restarting camera
                effort_end=defunct2$defunct_start[k] - 3600
                temp=data.frame(Grid = grid, effort_start = effort_start, effort_end = effort_end)
                effort=rbind(effort,temp)
                effort_start=effort_end + 3600
                effort_end=max(data2$DateTimeOriginal)
                temp=data.frame(Grid = grid, effort_start = effort_start, effort_end = effort_end)
                effort=rbind(effort,temp)
              } else {
                effort_start=defunct2$defunct_end[k-1] + 3600 ## One hr buffer after restarting camera
                effort_end=defunct2$defunct_start[k] - 3600
                temp=data.frame(Grid = grid, effort_start = effort_start, effort_end = effort_end)
                effort=rbind(effort,temp)
              } 
            }
          } ### for loop end k
        } else {
        effort_start=min(data2$DateTimeOriginal)+3600
        effort_end=max(data2$DateTimeOriginal)
        temp=data.frame(Grid = grid, effort_start = effort_start, effort_end = effort_end)
        effort=rbind(effort,temp)
      }
  } ### for loop end j
  return(effort)
}

##### Sample from camera trap images using the effort defined above


sample_n=function(detection_data,effort,sampling_interval){
  library(tidyverse)
  library(lubridate)
  #read the input data into the format required and assign data to appropriate variables
  data=detection_data%>%
    mutate(DateTimeOriginal=as.POSIXct(DateTimeOriginal))%>%
    group_by(Region.Label,Grid,Species,DateTimeOriginal)%>%
    summarise(size=max(count),distance=mean(distance))
  effort=effort%>%
    filter(Grid %in% data$Grid)%>%
    dplyr::select(Grid:effort_end)%>%
    mutate(effort_start=as.POSIXct(effort_start))%>%
    mutate(effort_end=as.POSIXct(effort_end))
  effort_matrix=data.frame()
  for (i in 1:length(unique(effort$Grid))) {
    grid=effort$Grid[i]
    effort2=effort%>%filter(Grid==grid)
    for (j in 1:nrow(effort2)) {
      effort_temp=data.frame(Grid = grid,
                             DateTimeOriginal = seq(from=effort2$effort_start[j] + sample(0:(sampling_interval-1),1),
                                                    to = effort2$effort_end[j], 
                                                    by = sampling_interval))%>%
        left_join(data)%>%filter(is.na(Species)==F)
      effort_matrix=rbind(effort_matrix,effort_temp)
      print(paste(grid, "complete",sep=" "))
    }
  }
  return(effort_matrix)
}

##### Effort and sample together


camtrapdis=function(filename,defunct_dates,sampling_interval)
{
  library(tidyverse)
  library(lubridate)
  
  #read the input data into the format required and assign data to appropriate variables
  data=readRDS(filename)
  data$DateTimeOriginal=as.POSIXct(data$DateTimeOriginal)
  
  defunct=read.csv(defunct_dates)
  defunct$Start=as.POSIXct(defunct$Start,tryFormats=c("%Y-%m-%d %H:%M:%OS","%d-%m-%Y %H:%M","%d-%m-%Y %H:%M:%OS"))
  defunct$End=as.POSIXct(defunct$End,tryFormats=c("%Y-%m-%d %H:%M:%OS","%d-%m-%Y %H:%M","%d-%m-%Y %H:%M:%OS"))
  Grid=unique(data$Grid)
  Region.Label=data%>%select(Region.Label,Grid)%>%unique.data.frame()%>%select(Region.Label)%>%as_vector()
  
  # Prepare a blank matrix to store the results.
  # The number of cells is equal to (Number of grids)x(No. of species) - this value is stated as nrow 
  sample=matrix(NA,ncol = 5)%>%
    as.data.frame()
  colnames(sample)=c("Grid","DateTimeOriginal","Species","size","distance")
  effort=matrix(NA,nrow = length(Grid),ncol = 3)%>%
    as.data.frame()
  colnames(effort)=c("Grid","Effort","Defunct_time")
  camtrapdis=list(effort=effort,sample=sample)
  
  for(j in 1:length(Grid)){
    grid=Grid[j]
    region.label=Region.Label[j]
    ## Overall time vector
    data2=data%>% filter(Grid==grid)
    #calculate the duration of time the camera is on - i.e. since the deployment of camera to removal
    start = min(data2$DateTimeOriginal)+3600 ### One hour (buffer period) for calibration
    end = max(data2$DateTimeOriginal)
    total_duration=as.numeric(end-start)*24*60*60
    total_time = seq(start, end, by= sampling_interval)%>%as.POSIXct(tz="Asia/Calcutta")
    
    ## Defunct time vector
    
    defunct2=defunct%>%filter(Grid==grid)%>%mutate(defunct_duration_days=End-Start)
    defunct_duration=as.numeric(sum(defunct2$defunct_duration_days))*24*60*60 ### convert from days to seconds
    defunct_time=NULL ##dummy character
    if (nrow(defunct2)>0) {
      defunct_time=min(defunct2$Start)
      for (k in 1:nrow(defunct2)) {
        defunct_time2=seq(from=defunct2$Start[k],to=defunct2$End[k],by=1)
        defunct_time=c(defunct_time,defunct_time2)%>%unique()%>%as.POSIXct(tz="Asia/Calcutta")
        rm(defunct_time2)
      }
    }
    
    # Remove defunct duration from the total duration to get 'duration when camera was active'
    active_time = setdiff(as.character(total_time),as.character(defunct_time)) %>%as.POSIXct()
    
    data3=data2%>%
      filter(DateTimeOriginal%in%active_time)%>%
      filter(Species!="")%>%
      group_by(DateTimeOriginal,Species)%>%
      summarise(size=max(count_new,na.rm = T),distance=mean(Distance_new))%>%
      mutate(Grid=grid)
    
    camtrapdis$effort[j,"Region.Label"]=region.label
    camtrapdis$effort[j,"Grid"]=grid
    camtrapdis$effort[j,"Effort"]=length(active_time)
    camtrapdis$effort[j,"Defunct_time"]=length(defunct_time)
    
    camtrapdis$sample=merge(data3,camtrapdis$sample,all = T)%>%
      filter(!is.na(DateTimeOriginal))
    paste(grid,"processed")
  }
  return(camtrapdis)
}

#camtrapdist=camtrapdis(filename = "Input/recordtable_master_withdistance_270921",defunct_dates="Input/defunct_dates.csv",
 #                      sampling_interval = 60)
#flatfile=merge(camtrapdist$sample,camtrapdist$effort,all.y=T,by = "Grid")
