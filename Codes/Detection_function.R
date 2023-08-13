library(tidyverse)
library(Distance)

flatfile=read.csv("Input/flatfile_1sec.csv")

{detection_matrix=data.frame()
Species = unique(flatfile$Species)}

species = Species[10]
de_data=flatfile%>%filter(Species==species & fill_na==FALSE & diff > 300)

hist((de_data%>%filter(fill_na==FALSE & diff > 300))$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

breakpoints <- c(0.8,2.9,5,7.5,11.5,15)
{
  conversion <- convert_units("meter", NULL, "square kilometer")
  trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
  mybreaks <- breakpoints
  
  hn01_cov <- ds(de_data, transect = "point", key="hn", adjustment = NULL,
             cutpoints = mybreaks,truncation = trunc.list,formula = ~DN)
  hn01_null<- ds(de_data, transect = "point", key="hn", adjustment = NULL,
                 cutpoints = mybreaks,truncation = trunc.list,formula = ~1)
  plot(hn01_cov)
}
summarize_ds_models(hn01_cov,hn01_null)
plot(hn01_null,pdf=T)

{
  detection_matrix_temp=data.frame(Species=species,DN=c("Day","Night"),
                                   p_mean=predict(hn01_cov,data.frame(DN=c("Day","Night")),se=T)$fitted,
                                   p_se=predict(hn01_cov,data.frame(DN=c("Day","Night")),se=T)$se.fit) %>%
    mutate(truncation=list(breakpoints))
  ### For Hazard rate models, if necessary:
  #detection_matrix_temp=data.frame(p_se=predict(hn01_cov,se=T)$se.fit,p_mean=predict(hn01_cov,se=T)$fitted)%>%unique.data.frame()%>%cbind(data.frame(Species=species,DN=unique(hn01_cov$ddf$data$DN)))%>%mutate(truncation=list(breakpoints))  
  detection_matrix=detection_matrix%>%filter(Species!=species)%>%rbind(detection_matrix_temp)%>%unique.data.frame()
}
write_rds(detection_matrix,"Input/detection_matrix")