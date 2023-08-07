library(tidyverse)
library(camtrapR)

###loading exiftools
exiftool_dir <- "D:/WII-Thar_LTEO/Camera_trapping/REM/Softwares/exiftool-12.29" 
exiftoolPath(exiftoolDir = exiftool_dir)
grepl(exiftool_dir, Sys.getenv("PATH"))
Sys.which("exiftool")
#making the table

Species_list=c("BLANK","CALIBRATION","CHINKARA","CATTLE","OTHERS","NILGAI","DOG","DESERT CAT","HARE","WILD PIG","INDIAN FOX",
               "MONGOOSE","UNIDENTIFIED FOX", "HUMAN-VEHICLE","SHEEP-GOAT","DESERT FOX","HOUBARA","GIB","WOLF","JUNGLE CAT")  

record_table <- recordTable(inDir = "F:/Tagged Photos Checked 25-05-2021/Chouhani/", 
                            IDfrom = "directory", 
                            cameraID="directory", camerasIndependent=T, 
                            removeDuplicateRecords = F,
                            deltaTimeComparedTo ="lastRecord",
                            timeZone = "Asia/Calcutta", stationCol = "station",
                            writecsv = F,
                            additionalMetadataTags = c("MakerNotes:SerialNumber","XMP:Subject",
                                                       "MakerNotes:AmbientTemperature",
                                                       "MakerNotes:MotionSensitivity",
                                                       "MakerNotes:UserLabel"))%>%
  select(-Camera)%>%
  rename(Grid=station)%>%
  rename(Camera=Species)%>%
  separate(XMP.Subject,c("count","Species","Znew","V1","V2"),sep=", ",extra="drop")%>%
  mutate(Distance=ifelse(Species=="C0-2"|Species=="C4-12"|Species=="C4-8"|Species=="C8-12"|Species=="C12-20"|
                           Species=="C2-4"|Species=="C8-20"|Species=="C>20"|Species=="12-20"|Species=="4-8",
                         as.character(Species),NA))%>%
  mutate(Species=ifelse(Species=="C0-2"|Species=="C4-12"|Species=="C4-8"|Species=="C8-12"|Species=="C12-20"|
                          Species=="C2-4"|Species=="C8-20"|Species=="C>20"|Species=="12-20"|Species=="4-8",
                        as.character(Znew),Species))%>%
  mutate(Distance=ifelse(Znew=="D2"|Znew=="D4"|Znew=="D6"|Znew=="D8"|Znew=="D10"|Znew=="D1"|Znew=="D5"|Znew=="D9"|
                           Znew=="D12"|Znew=="D14"|Znew=="D16"|Znew=="D18"|Znew=="D20"|Znew=="D3"|Znew=="D7",
                         as.character(Znew),as.character(Distance)))%>%
  mutate(Species=ifelse(Species=="C0-2"|Species=="C4-12"|Species=="C4-8"|Species=="C8-12"|Species=="C12-20"|
                          Species=="C2-4"|Species=="C8-20"|Species=="C>20"|Species=="12-20"|Species=="4-8",
                        NA,as.character(Species)))%>%
  mutate(Species=ifelse(is.na(Species)==T  & Znew%in%Species_list==T,
                        as.character(Znew),as.character(Species)))%>%
  mutate(Species=ifelse(is.na(Species)==T | Species=="BLANK"& V1%in%Species_list==T,
                        as.character(V1),as.character(Species)))%>%
  mutate(Znew=ifelse(Znew=="D2"|Znew=="D4"|Znew=="D6"|Znew=="D8"|Znew=="D10"|Znew=="D1"|Znew=="D5"|Znew=="D9"|
                       Znew=="D12"|Znew=="D14"|Znew=="D16"|Znew=="D18"|Znew=="D20"|Znew=="D3"|Znew=="D7",
                     NA,as.character(Znew)))%>%
  mutate(Znew=ifelse(Znew%in%Species_list,
                     NA,as.character(Znew)))%>%
  mutate(Znew=ifelse(is.na(Znew) & V1=="ZNEW",as.character(V1),as.character(Znew)))%>%
  mutate(V1=ifelse(Znew=="ZNEW" & V1=="ZNEW",NA,as.character(V1)))%>%
  mutate(Species=ifelse(count=="CHINKARA. 1",as.character("CHINKARA"),as.character(Species)))%>%
  mutate(count=ifelse(count=="CHINKARA. 1",as.character("1"),as.character(count)))

#write_rds(record_table2,"F:/Tagged Photos Checked 25-05-2021/Sudasri_out/recordtable")

#Kanoi, Chouhani, Sudasiri, Gajai_PPC, Gajaimata,Miajlar,Miajlar_out, Ramdeora, Rasla_enclosure,  Rasla_out, RKVY, Sudasiri_out

Kanoi=read_rds("F:/Tagged Photos Checked 25-05-2021/Kanoi/recordtable")%>%
  mutate(Area="Kanoi")%>%
  mutate(Enclosure=as.numeric(1))
Chouhani=read_rds("F:/Tagged Photos Checked 25-05-2021/Chouhani/recordtable")%>%
  mutate(Area="Chouhani")%>%
  mutate(Enclosure=as.numeric(1))
Sudasri=read_rds("F:/Tagged Photos Checked 25-05-2021/Sudasri/recordtable")%>%
  mutate(Area="Sudasri")%>%
  mutate(Enclosure=as.numeric(1))
Sudasri_out=read_rds("F:/Tagged Photos Checked 25-05-2021/Sudasri_out/recordtable")%>%
  mutate(Area="Sudasri")%>%
  mutate(Enclosure=as.numeric(0))
RKVY=read_rds("F:/Tagged Photos Checked 25-05-2021/RKVY/recordtable")%>%
  mutate(Area="RKVY")%>%
  mutate(Enclosure=as.numeric(1))
Gajai_PPC=read_rds("F:/Tagged Photos Checked 25-05-2021/Gajai_PPC/recordtable")%>%
  mutate(Area="Gajaimata_PPC")%>%
  mutate(Enclosure=as.numeric(1))
Gajaimata=read_rds("F:/Tagged Photos Checked 25-05-2021/Gajaimata/recordtable")%>%
  mutate(Area="Gajaimata")%>%
  mutate(Enclosure=as.numeric(1))
Miajlar=read_rds("F:/Tagged Photos Checked 25-05-2021/Miajlar/recordtable")%>%
  mutate(Area="Miajlar")%>%
  mutate(Enclosure=as.numeric(1))
Miajlar_out=read_rds("F:/Tagged Photos Checked 25-05-2021/Miajlar_out/recordtable")%>%
  mutate(Area="Miajlar")%>%
  mutate(Enclosure=as.numeric(0))
Ramdeora=read_rds("F:/Tagged Photos Checked 25-05-2021/Ramdeora/recordtable")%>%
  mutate(Area="Ramdeora")%>%
  mutate(Enclosure=as.numeric(1))
Rasla_enclosure=read_rds("F:/Tagged Photos Checked 25-05-2021/Rasla_enclosure/recordtable")%>%
  mutate(Area="Rasla")%>%
  mutate(Enclosure=as.numeric(1))
Rasla_out=read_rds("F:/Tagged Photos Checked 25-05-2021/Rasla_out/recordtable")%>%
  mutate(Area="Rasla")%>%
  mutate(Enclosure=as.numeric(0))

merged=Kanoi%>%
  full_join(Sudasri)%>%
  full_join(Sudasri_out)%>%
  full_join(Gajai_PPC)%>%
  full_join(Gajaimata)%>%
  full_join(Miajlar)%>%
  full_join(Miajlar_out)%>%
  full_join(Ramdeora)%>%
  full_join(Rasla_enclosure)%>%
  full_join(Rasla_out)%>%
  full_join(RKVY)%>%
  full_join(Chouhani)%>%
  select(-V1,-V2)

### "Start from here" - 08-08-2021

#### Animator

library(readxl)
library(tidyverse)

merged=readRDS("F:/Tagged Photos Checked 25-05-2021/recordtable_master_withoutdistance_080821") %>%
  mutate(DateTimeOriginal=as.POSIXct(DateTimeOriginal,tz="Asia/Calcutta"))

animator= read.csv("F:/Kanoi_Z_news.csv")%>%
  rbind(read.csv("F:/Sudasri_z_new.csv"))%>%
  rbind(read.csv("F:/RKVY_z_new.csv"))%>%
  rbind(read.csv("F:/Gajaimata_z_new.csv"))%>%
  filter(!is.na(DATE.IMAGE.TAKEN))%>%
  rename(Distance=DISTANCE.FROM.CAMERA.MTS..)%>%
  rename(count=ANIMAL.COUNT)%>%
  rename(Species=SPECIES.NAME)%>%
  rename(Grid=CAMERA.TRAP.ID)%>%
  rename(Time=TIME.IMAGE.TAKEN)%>%
  mutate(Photo=as.character(FILE.PATH))%>%
  separate(Photo,c("a","b"),sep = "IMG",extra = "merge", fill = "right")%>%
  select(-a)%>%
  mutate(I="IMG")%>%
  unite("FileName",I:b,sep = "",na.rm=T,remove = T)%>%
  select(-ACTUAL.DISTANCE.MOVED,-MARKING.TYPE,-SPEED,-FLASH,-MARKING.TYPE,-ANIMAL.SIZE,-DISTANCE.TRAVELLED,-DATE.IMAGE.TAKEN,
         -SL.NO.)

Dist_mean=animator%>%
  group_by(Grid,Time,FILE.PATH,FileName)%>%
  mutate(Grid=as.character(Grid))%>%
  mutate(FileName=as.character(FileName))%>%
  summarize(Distance_mean=mean(Distance))%>%
  ungroup()%>%
  select(Grid,Time,FileName,Distance_mean)

record_table_withznew_distance=merged%>%
  left_join(Dist_mean,by=c("Grid","Time","FileName"))

record_table_withdistance=record_table_withznew_distance%>%
  mutate(Distance_new=ifelse(Distance=="C0-2",as.numeric(1),Distance))%>%
  mutate(Distance_new=ifelse(Distance=="C2-4",as.numeric(3),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="C4-8",as.numeric(6),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="C8-12",as.numeric(10),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="C4-12",as.numeric(8),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="C12-20",as.numeric(16),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="C8-20",as.numeric(14),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="C>20",as.numeric(25),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D1",as.numeric(1),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D2",as.numeric(2),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D3",as.numeric(3),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D4",as.numeric(4),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D5",as.numeric(5),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D6",as.numeric(6),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D7",as.numeric(7),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D8",as.numeric(8),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D9",as.numeric(9),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D10",as.numeric(10),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D20",as.numeric(20),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D18",as.numeric(18),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D12",as.numeric(12),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D14",as.numeric(14),Distance_new))%>%
  mutate(Distance_new=ifelse(Distance=="D16",as.numeric(16),Distance_new))%>%
  mutate(Distance_new=ifelse(!is.na(Distance_mean),Distance_mean,Distance_new))%>%
  rename(Distance_znew=Distance_mean)%>%
  rename(Distance_allsticks=Distance)%>%
  filter(Grid%in%c("S101","S102","S103","S129")==F)%>% ### Removing problem cameras
  mutate(Defunct=ifelse(count=="DEFUNCT START"|count=="DEFUNCT END",count,NA))%>%
  mutate(Defunct=ifelse(count=="-",as.character("DEFUNCT"),NA))%>%
  mutate(count=ifelse(count=="DEFUNCT START"|count=="DEFUNCT END"|count=="-",NA,count))%>%
  mutate(count_new=ifelse(count=="10-25",as.numeric(18),count))%>%
  mutate(count_new=ifelse(count=="25-50",as.numeric(38),count_new))%>%
  mutate(count_new=ifelse(count=="50-100",as.numeric(75),count_new))%>%
  mutate(count_new=as.numeric(count_new))%>%
  mutate(Distance_new=as.numeric(Distance_new))

record_table_withdistance=read_rds("Input/recordtable_master_withdistance_270921")

znew2=read.csv("Input/znew2.csv")%>%
  select(Grid,DateTimeOriginal,FileName,Species_znew2,count_znew2,Distance_znew2)%>%
  mutate(DateTimeOriginal=as.POSIXct(DateTimeOriginal,tz="Asia/Calcutta"))

record_table_withdistance2=left_join(record_table_withdistance,znew2)%>%
  mutate(count_znew2=ifelse(is.na(count_znew2),count_new,count_znew2))%>%
  rename(count_old=count_new)%>%
  rename(count_new=count_znew2)%>%
  mutate(Species_znew2=ifelse(is.na(Species_znew2),Species,Species_znew2))%>%
  rename(Species_old=Species)%>%
  rename(Species=Species_znew2)%>%
  mutate(Distance_new=ifelse(is.na(Distance_new),Distance_znew2,Distance_new))

write_rds(record_table_withdistance2,"Input/recordtable_master_withdistance_270921")

znew_withoutdistance=record_table_withdistance%>%
  filter(Enclosure==1)%>%
  filter(Area=="Sudasri"|Area=="RKVY"|Area=="Kanoi"|Area=="Gajaimata"|Area=="Gajaimata_PPC")%>%
  filter(Species!="BLANK")%>%
  filter(Species!="CALIBRATION")%>%
  filter(Species!="MONGOOSE")%>%
  filter(Species!="OTHERS")%>%
  filter(Species!="CATTLE")%>%
  filter(Species!="SHEEP-GOAT")%>%
  filter(Species!="HUMAN-VEHICLE")%>%
  filter(!is.na(Species))%>%
  filter(Znew=="ZNEW")%>%
  unique.data.frame()%>%
  filter(is.na(Distance_new))

allsticks_withoutdistance=record_table_withdistance%>%
  filter(Enclosure==1)%>%
  filter(Species!="BLANK")%>%
  filter(Species!="CALIBRATION")%>%
  filter(Species!="MONGOOSE")%>%
  filter(Species!="OTHERS")%>%
  filter(Species!="CATTLE")%>%
  filter(Species!="SHEEP-GOAT")%>%
  filter(Species!="HUMAN-VEHICLE")%>%
  filter(!is.na(Species))%>%
  filter(Directory%in%znew_withoutdistance$Directory==F)%>%
  filter(delta.time.secs>60)%>%
  unique.data.frame()%>%
  filter(is.na(Distance_new))

distance_summary_specieswise=record_table_withdistance%>%
  mutate(srno=row_number())%>%
  filter(Distance_new!="NA")%>%
  group_by(Species)%>%
  summarise(Photos_withdistance=length(unique(srno)))%>%
  ungroup()%>%
  right_join(record_table_withdistance%>%group_by(Species)%>%summarise(Photos_all=length(Distance_new)))%>%
  left_join(record_table_withdistance%>%filter(delta.time.secs>60)%>%group_by(Species)%>%
              summarise(Photos_independent=length(Distance_new)))

allsticks=read.csv("Input/all_sticks_csv_260321.csv")%>%
  mutate(DateTimeOriginal=as.POSIXct(paste(date,time,sep=" ")))%>%
  select(Grid,DateTimeOriginal,Species,dist_midpt)%>%
  rename(Distance_allsticks2=dist_midpt)

record_table_withdistance2=left_join(record_table_withdistance,allsticks)%>%
  mutate(Distance_new=ifelse(is.na(Distance_new),Distance_allsticks2,Distance_new))