get_sunmoon_data=function(start_date,end_date,lat,long) {
  library(suncalc)
  library(tidyverse)
  data_matrix = data.frame(Date = seq(as.Date(start_date),as.Date(end_date),"1 day"))%>%
    cbind(getSunlightTimes(.$Date, lat = lat, lon = long, keep = c( "sunrise", "sunset","dawn", "dusk", "nightEnd", 
                                                                   "night")))%>%
    cbind(getMoonTimes(.$Date, lat = as.numeric(lat), lon = as.numeric(long), keep = c("rise", "set")))%>%
    dplyr::select(-date,-lat,-lon)
  
  return(data_matrix)
}

