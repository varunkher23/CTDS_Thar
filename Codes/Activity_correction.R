library(tidyverse)
library(activity)

flatfile=read.csv("Input/flatfile_1sec.csv")

{activity_matrix=read_rds("Input/activity_matrix")
Species = unique(flatfile$Species)}

#### Estimate activity pattern

species=Species[14]

{activity_data=flatfile%>%
  filter(Species==species)%>%
  filter(!is.na(DateTimeOriginal))%>%
  filter(diff>300 | diff == 0)
activity_estimate=fitact(hms2rad(strftime(activity_data$DateTimeOriginal,format="%H:%M:%S")),sample="data" )
plot(activity_estimate)
}

{activity_matrix_temp=data.frame(Species=species)%>%cbind(t(activity_estimate@act))
activity_matrix=activity_matrix%>%rbind(activity_matrix_temp)%>%unique.data.frame()
activity_matrix
}

write_rds(activity_matrix,"Input/activity_matrix")
