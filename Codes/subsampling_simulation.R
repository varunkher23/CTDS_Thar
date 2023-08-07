library(astroFns)
library(activity)
library(tidyverse)
library(reshape2)
library(Distance)
library(photobiology)
library(lubridate)
library(suncalc)

record_table=read_rds("Input/recordtable_master_withdistance_270921")

locations=select(record_table,Area,Enclosure,Grid)%>%unique.data.frame()
data=record_table%>%
  group_by(Area,Enclosure,Grid,Species)%>%
  summarise()


desert_fox=data%>%filter(Species=="INDIAN FOX")%>%right_join(locations)%>%
  separate(Grid,c("Area","Grid"),sep=" ",extra = "merge")


#################

flatfile=read.csv("Output_interim/flatfile_290921.csv")

flatfile$DateTimeOriginal=as.POSIXct(flatfile$DateTimeOriginal,tz = "Asia/Calcutta")
dn= day_night(date = flatfile$DateTimeOriginal,tz=tz(flatfile$DateTimeOriginal),geocode = data.frame(lat=27,lon=71),
              unit.out = "datetime")
moon=getMoonTimes(date = as.Date(flatfile$DateTimeOriginal),tz=tz(flatfile$DateTimeOriginal),lat=27,lon=71,keep=c("rise","set"))
moonillum=getMoonIllumination(date = as.Date(flatfile$DateTimeOriginal),keep=c("fraction","phase"))
DateTimeOriginal_dn=data.frame(DateTimeOriginal=flatfile$DateTimeOriginal)%>%
  cbind(dn)
DateTimeOriginal_dn=DateTimeOriginal_dn%>%
  mutate(DN="NA")%>%
  mutate(DN=ifelse(DateTimeOriginal>sunrise & DateTimeOriginal<sunset,"Day",DN))%>%
  mutate(DN=ifelse(DateTimeOriginal<sunrise | DateTimeOriginal>sunset,"Night",DN))%>%
  select(DateTimeOriginal,DN)

flatfile=merge(flatfile,DateTimeOriginal_dn,by="DateTimeOriginal")%>%
  unique.data.frame()

#sum(!is.na(flatfile$distance))
#View(table(flatfile$Grid))
#temp=record_table_withdistance2%>%select(Grid,Area,Enclosure)%>%unique.data.frame()

traps=flatfile%>%
  select(Region.Label,Grid,Effort,Defunct_time)%>%
  unique.data.frame()
sample=flatfile%>%
  select(-Defunct_time,-Effort)


Ifox=sample%>%
  filter(Species=="INDIAN FOX")%>%
  fill(distance)%>%
  merge(traps,by=c("Region.Label","Grid"),all = T)%>%
  rename(Sample.Label=Grid)%>%
  mutate(Area=as.numeric(1))%>%
  mutate(size=as.numeric(size))%>%
  mutate(DateTimeOriginal=as.POSIXct(DateTimeOriginal))%>%
  arrange(Sample.Label,DateTimeOriginal)%>%
  group_by(Sample.Label)%>%
  mutate(diff = as.numeric(strptime(DateTimeOriginal, "%Y-%m-%d %H:%M:%S") - lag(strptime(DateTimeOriginal, "%Y-%m-%d %H:%M:%S"), 
                                                                                 default = strptime(DateTimeOriginal, "%Y-%m-%d %H:%M:%S")[1])))%>%
  ungroup(Sample.Label)%>%
  as.data.frame()%>%
  filter(Region.Label=="Sudasri_Inside")

activity_Ifox=Ifox%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_Ifox=fitact(hms2rad(strftime(activity_Ifox$DateTimeOriginal,format="%H:%M:%S")),sample="data" )


breakpoints <- c(0.01,2.99,5,7,11)
hist(Ifox$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(filter(Ifox,diff>300), transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)


p_a <- hn01$ddf$fitted[1]
w <- 12
rho <- sqrt(p_a * w^2)

viewangle <- 42 # degrees
samfrac <- viewangle / 360

Ifox.dens <- dht2(hn01, flatfile =Ifox, strat_formula = ~Region.Label,
                  sample_fraction = samfrac*activity_Ifox@act[["act"]], 
                  er_est = "P2", convert_units = conversion)
density_100pc=Ifox.dens$Abundance
se_100pc=Ifox.dens$Abundance_se

density_50pc=as.data.frame(matrix(ncol = 2,nrow = 1000))%>%
  `colnames<-`(c("Density","se"))

for (i in 1:1000) {
  
  traps_50pc=sample(unique(Ifox$Sample.Label),length(unique(Ifox$Sample.Label))/2)
  Ifox_50pc=Ifox%>%filter(Sample.Label%in%traps_50pc)
  Ifox_50pc.dens <- dht2(hn01, flatfile =Ifox_50pc, strat_formula = ~Region.Label,
                    sample_fraction = samfrac*activity_Ifox@act[["act"]], 
                    er_est = "P2", convert_units = conversion)

  density_50pc[i,"Density"]=as.numeric(Ifox_50pc.dens$Abundance)
  density_50pc[i,"se"]=as.numeric(Ifox.dens$Abundance_se)
  }

density_50pc=density_50pc%>%mutate(Number=row_number())
density_plot=ggplot(density_50pc,aes(x=Number,y=Density))+
  geom_jitter()+
  geom_pointrange(aes(min=Density-se,max=Density+se),alpha=0.1)+
  geom_hline(yintercept=density_100pc,color="red")+
  geom_hline(yintercept=density_100pc+se_100pc,color="red",linetype="dashed")+
  geom_hline(yintercept=density_100pc-se_100pc,color="red",linetype="dashed")
  

plot(density_plot)
