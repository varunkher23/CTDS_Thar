library(astroFns)
library(activity)
library(tidyverse)
library(reshape2)
library(Distance)
library(photobiology)
library(lubridate)
library(suncalc)

source("Codes/Data_preparation.R")
record_table_withdistance=read_rds("Input/recordtable_master_withdistance_270921")
camtrapdist=camtrapdis(filename = "Input/recordtable_master_withdistance_270921",defunct_dates="Input/defunct_dates.csv",
                                             sampling_interval = 2)
flatfile=merge(camtrapdist$sample,camtrapdist$effort,all.y=T,by = "Grid")
#write.csv(flatfile,"Output_interim/flatfile_290921.csv")
flatfile=read.csv("D:/WII-Thar_LTEO/Camera_trapping/REM/Output_interim/flatfile_290921.csv")
#flatfile2=read.csv("Output_interim/flatfile_znew.csv")
#flatfile_master=rbind(flatfile,flatfile2)
#flatfile=flatfile_master

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

sum(!is.na(flatfile$distance))
View(table(flatfile$Grid))

temp=record_table_withdistance2%>%select(Grid,Area,Enclosure)%>%unique.data.frame()
traps=flatfile%>%
  select(Region.Label,Grid,Effort,Defunct_time)%>%
  unique.data.frame()
sample=flatfile%>%
  select(-Defunct_time,-Effort)

chinkara=sample%>%
  filter(Species=="CHINKARA")%>%
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
  as.data.frame()

activity_chinkara=chinkara%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_chinkara=fitact(hms2rad(strftime(activity_chinkara$DateTimeOriginal,format="%H:%M:%S")),sample="data" )
    
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
  as.data.frame()

activity_Ifox=Ifox%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_Ifox=fitact(hms2rad(strftime(activity_Ifox$DateTimeOriginal,format="%H:%M:%S")),sample="data" )

Dfox=sample%>%
  filter(Species=="DESERT FOX")%>%
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
  as.data.frame()

activity_Dfox=Dfox%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_Dfox=fitact(hms2rad(strftime(activity_Dfox$DateTimeOriginal,format="%H:%M:%S")),sample="data" )

Dcat=sample%>%
  filter(Species=="DESERT CAT")%>%
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
  as.data.frame()

plot(activity_Ifox)

activity_Dcat=Dcat%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_Dcat=fitact(hms2rad(strftime(activity_Dcat$DateTimeOriginal,format="%H:%M:%S")),sample="data" )

Dog=sample%>%
  filter(Species=="DOG")%>%
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
  filter(Sample.Label!="OUT 732")

activity_Dog=Dog%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_Dog=fitact(hms2rad(strftime(activity_Dog$DateTimeOriginal,format="%H:%M:%S")),sample="data" )

Nilgai=sample%>%
  filter(Species=="NILGAI")%>%
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
  filter(Sample.Label!="OUT 732")

activity_Nilgai=Nilgai%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_Nilgai=fitact(hms2rad(strftime(activity_Nilgai$DateTimeOriginal,format="%H:%M:%S")),sample="data" )

Wildpig=sample%>%
  filter(Species=="WILD PIG")%>%
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
  filter(Sample.Label!="OUT 732")

activity_Wildpig=Wildpig%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_Wildpig=fitact(hms2rad(strftime(activity_Wildpig$DateTimeOriginal,format="%H:%M:%S")),sample="data" )

GIB=sample%>%
  filter(Species=="GIB")%>%
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
  as.data.frame()

activity_GIB=GIB%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>10)
activity_GIB=fitact(hms2rad(strftime(activity_GIB$DateTimeOriginal,format="%H:%M:%S")),sample="data" )

breakpoints <- c(1.1,3.6,5,7,11)
### Indian fox  -> c(0,2.3,4.7,8) // c(0.01,2.99,5,7,11) //c(1.1,3.6,5,7,11)
### Desert fox -> c(0,2.9,4.7,8) // c(0,1.3,5.4,8.2,11)
### Chinkara -> c(0,2.7,7.8,14) /// c(0.8,2.9,5,7.5,11.5,15)
### Dcat -> c(0,0.9,1.89,3.01,6,10) // c(0,0.9,1.89,3.01,6,10,15)

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

hn1 <- ds(chinkara, transect = "point", key="hn", adjustment = "herm",
          cutpoints = mybreaks,truncation = trunc.list)
uni1 <- ds(chinkara, transect = "point", key="unif", adjustment = "cos",
           order=1,
           cutpoints = mybreaks, truncation = trunc.list)
uni2 <- ds(chinkara, transect = "point", key="unif", adjustment = "cos",
           order=c(1,2),
           cutpoints = mybreaks, truncation = trunc.list)
hr0 <- ds(chinkara, transect = "point", key="hr", adjustment = NULL,
          cutpoints = mybreaks, truncation = trunc.list)
hr1 <- ds(chinkara, transect = "point", key="hr", adjustment = "cos",
          order=2,
          cutpoints = mybreaks, truncation = trunc.list)
hr2 <- ds(chinkara, transect = "point", key="hr", adjustment = "cos",
          order=c(2,3),
          cutpoints = mybreaks, truncation = trunc.list)

plot(hn0)
chat <- function(modobj) {
  #  computes c-hat for a dsmodel object using Method 1 of Howe et al. (2018)
  test <- gof_ds(modobj)
  num <- test$chisquare$chi1$chisq
  denom <- test$chisquare$chi1$df
  chat <- num/denom
  return(chat)
}

qaic <- function(modobj, chat) {
  #  computes QAIC for a dsmodel object given a c-hat
  value <- 2* modobj$ddf$ds$value/chat + 2 * (length(modobj$ddf$ds$pars)+1)
  return(value)
}

qaic.pass1 <- function(...) {
  #   Performs Pass 1 model selection based upon Method 1 of Howe et al. (2018)
  #   Arguments are dsmodel objects; assumed all based on same key function
  #    c-hat is computed for the most parameter-rich model in the group
  #    qaic is calculated for each model in group based upon this c-hat
  #   Result returned in the form of a data.frame with model name, npar, aic and qaic
  models <- list(...)
  num.models <- length(models)
  npar <- unlist(lapply(models, function(x) length(x$ddf$ds$par)))  
  modname <-  unlist(lapply(models, function(x) x$ddf$name.message))
  aic <-  unlist(lapply(models, function(x) x$ddf$criterion))
  chat.bigmod <- chat(models[[which.max(npar)]])
  qaic <- vector(mode="numeric", length = num.models)
  for (i in 1:num.models) {
    qaic[i] <- qaic(models[[i]], chat.bigmod)
  }
  nicetab <- data.frame(modname, npar, aic, qaic)
  return(nicetab)
}

knitr::kable(qaic.pass1(hn0, hn1), 
             caption="QAIC values for half normal key models.")

knitr::kable(qaic.pass1(uni1, uni2),
             caption="QAIC values for uniform key models.")

knitr::kable(qaic.pass1(hr0, hr1, hr2),
             caption="QAIC values for hazard rate key models.")

winners <- list(hn0, uni2,hr2)
chats <- unlist(lapply(winners, function(x) chat(x)))
modnames <- unlist(lapply(winners, function(x) x$ddf$name.message))
results <- data.frame(modnames, chats)
results.sort <- results[order(results$chats),]
knitr::kable(results.sort, digits=2, row.names = FALSE,
             caption="Compare with Table S5 of Howe et al. (2018)")

p_a <- hn0$ddf$fitted[1]
w <- 12
rho <- sqrt(p_a * w^2)

viewangle <- 42 # degrees
samfrac <- viewangle / 360
conversion <- convert_units("meter", NULL, "square kilometer")

### Chinkara -> c(0,2.7,7.8,14) /// c(0.8,2.9,5,7.5,11.5,15)

breakpoints <- c(0.6,2.9,5,7.5,11.5,15)
hist(chinkara$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(filter(chinkara,diff>300), transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)

chinkara.dens <- dht2(hn01, flatfile =chinkara, strat_formula = ~Region.Label,
                     sample_fraction = samfrac*activity_chinkara@act[["act"]], 
                     er_est = "P2", convert_units = conversion)
print(chinkara.dens,report="density")

write.csv(chinkara.dens,"Results_temp/chinkara.csv")

### Nilgai -> c(0,2.7,7.8,14) /// c(0.8,2.9,5,7.5,11.5,15)

breakpoints <- c(0.8,2.9,5,7.5,11.5,15)
hist(Nilgai$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(filter(Nilgai,diff>300), transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)

Nilgai.dens <- dht2(hn01, flatfile = Nilgai, strat_formula = ~Region.Label,
                      sample_fraction = samfrac*activity_Nilgai@act[["act"]], 
                      er_est = "P2", convert_units = conversion)
print(Nilgai.dens,report="density")

write.csv(chinkara.dens,"Results_temp/chinkara.csv")

### Wildpig -> c(0,2.7,7.8,14) /// c(0.8,2.9,5,7.5,11.5,15)

breakpoints <- c(0,2.9,5,11.5,20)
hist(Wildpig$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(Wildpig, transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)

Nilgai.dens <- dht2(hn01, flatfile = Nilgai, strat_formula = ~Region.Label,
                    sample_fraction = samfrac*activity_Wildpig@act[["act"]], 
                    er_est = "P2", convert_units = conversion)
print(Nilgai.dens,report="density")

write.csv(chinkara.dens,"Results_temp/chinkara.csv")

### Indian fox  -> c(0,2.3,4.7,8) // c(0.01,2.99,5,7,11) //c(1.1,3.6,5,7,11)

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

Ifox.dens <- dht2(hn01, flatfile =Ifox, strat_formula = ~Region.Label,
                  sample_fraction = samfrac*activity_Ifox@act[["act"]], 
                  er_est = "P2", convert_units = conversion)
print(Ifox.dens, report="density")
write.csv(Ifox.dens,"Results_temp/Ifox.csv")

### Desert fox -> c(0,2.9,4.7,8) // c(0,1.3,5.4,8.2,11)

breakpoints <- c(0.4,2.9,4.9,8.2,11)
hist(Dfox$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(filter(Dfox,diff>300), transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)

Dfox.dens <- dht2(hn01, flatfile =Dfox, strat_formula = ~Region.Label,
                  sample_fraction = samfrac*activity_Dfox@act[["act"]], 
                  er_est = "P2", convert_units = conversion)
print(Dfox.dens, report="density")
write.csv(Dfox.dens,"Results_temp/Dfox.csv")

### Dcat -> c(0,0.9,1.89,3.01,6,10) // c(0,0.9,1.89,3.01,6,10,15)

breakpoints <- c(0.1,0.9,1.89,3.01,6)
hist(Dcat$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(filter(Dcat,diff>300), transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)

Dcat.dens <- dht2(hn01, flatfile =Dcat, strat_formula = ~Region.Label,
                      sample_fraction = samfrac*activity_Dcat@act[["act"]], 
                      er_est = "P2", convert_units = conversion)
print(Dcat.dens, report="density")
write.csv(Dcat.dens,"Results_temp/Dcat.csv")

# Dog 

breakpoints <- c(1.1,3.6,5,7,11)
hist(Dog$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(filter(Dog,diff>300), transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)

Dog.dens <- dht2(hn01, flatfile =Dog, strat_formula = ~Region.Label,
                  sample_fraction = samfrac*activity_Dog@act[["act"]], er_est = "P2", convert_units = conversion)
print(Dog.dens, report="density")

write.csv(Dog,"Results_temp/Dog.csv")

## GIB 

breakpoints <- c(1.1,3.6,5,7,11)
hist(chinkara$distance,  main="Peak activity data set",
     xlab="Radial distance (m)",labels = T)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))
mybreaks <- breakpoints

hn01 <- ds(filter(GIB,diff>300), transect = "point", key="hn", adjustment = NULL,
           cutpoints = mybreaks,truncation = trunc.list)
gof_ds(hn01)
plot(hn01,pdf=T)
plot(hn01)

GIB.dens <- dht2(hn01, flatfile =GIB, strat_formula = ~Region.Label,
                 sample_fraction = samfrac*activity_GIB@act[["act"]], er_est = "P2", convert_units = conversion)
print(GIB.dens, report="density")

chinkara%>%
  group_by(Region.Label,Sample.Label)%>%
  summarise(effort=mean(Effort),ER=sum(size,na.rm = T))%>%
  View()

mysummary <- function(ests, fit){
  return(data.frame(Dhat = ests$individuals$D$Estimate))
}

chinkara.boot.hr <- bootdht(model=hn01, flatfile=chinkara, resample_transects = TRUE,
                          nboot=400, summary_fun=mysummary, sample_fraction = samfrac,
                          convert.units = conversion)

GIB=flatfile%>%
  filter(Species=="GIB")

chinkara%>%
  group_by(DN)%>%
  summarise(mean=median(distance,na.rm=T))


 