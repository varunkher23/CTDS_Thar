library(tidyverse)
library(Distance)

flatfile=read.csv("Input/flatfile_1sec.csv")
detection_matrix=read_rds("Input/detection_matrix")
activity_matrix=read_rds("Input/activity_matrix")
sampling_interval=1
effort_data=read.csv("Input/effort_data.csv")%>%
  group_by(Grid)%>%
  summarise(Effort=sum(effort_secs/sampling_interval))%>%
  ungroup()%>%
  rename(Sample.Label=Grid)

viewangle <- 42 # degrees
samfrac <- viewangle / 360
conversion <- convert_units("meter", NULL, "square kilometer")

{density_matrix=data.frame()
Species = unique(flatfile$Species)}

species=Species[1]

de_data=flatfile%>%filter(Species==species)%>%left_join(effort_data)%>%rename(object=X)
activity_est=(activity_matrix%>%filter(Species==species))$act
### Not ideal, needs to be based to a bayesian approximation of the Howe et al., 2017 formula
{
breakpoints <- unlist((detection_matrix%>%select(Species,truncation)%>%filter(Species==species)%>%unique.data.frame())$truncation)

conversion <- convert_units("meter", NULL, "square kilometer")
trunc.list <- list(left=min(breakpoints),right=max(breakpoints))

detection_fn <- ds(de_data, transect = "point", key="hn", adjustment = NULL,
           cutpoints = breakpoints,truncation = trunc.list)
}

{density <- dht2(detection_fn, flatfile = de_data, strat_formula = ~Region.Label,
                      sample_fraction = samfrac*activity_est,
                      er_est = "P2", convert_units = conversion)
print(density,report="density")
}


{
  density_matrix_temp=density%>%
    select(Region.Label:n,k,Covered_area,group_mean,group_var,group_CV,ER,ER_var,ER_CV,Abundance,Abundance_se,Abundance_CV,LCI,UCI)%>%
    rename(Density=Abundance,
           Density_se=Abundance_se,
           Density_CV=Abundance_CV)%>%
    filter(Region.Label!="Total")%>%
    mutate(Species=as.character(species))%>%
    relocate(Species)%>%
  mutate(Density=round(Density,4),Density_se=round(Density_se,4),Density_CV=round(Density_CV,4),LCI=round(LCI,4),UCI=round(UCI,4),
         Nc=round(Nc,4),Covered_area=round(Covered_area,4))
  density_matrix=density_matrix%>%rbind(density_matrix_temp)%>%unique.data.frame()%>%
    relocate(Species,Region.Label,Density,Density_se,Density_CV,LCI,UCI)
}

#write_rds(density_matrix,"Results_interim/density_matrix")
#write.csv(density_matrix,"Results_interim/density_matrix.csv")
