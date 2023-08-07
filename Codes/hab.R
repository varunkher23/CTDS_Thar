setwd("D:/WII-Thar_LTEO/Camera_trapping/REM/Input")
dat = read.csv("cam_hab.csv")
library(dplyr)
library(ggplot2)
names(dat)

dat$totcov = dat$Grs0_25cm+ dat$Grs25_50cm+dat$Grs50_100cm+ dat$Shr0_50cm+ dat$Shr50_100cm+ dat$Shr1_2m+ dat$Tree+ dat$Bare
dat[is.na(dat)] = 0

for(j in 1:nrow(dat)){
  for(i in 12:19){
    dat[j,i+9] = ifelse(dat[j,20] >= 100, (dat[j,i] / dat[j,20])*100, dat[j,i])
  }
}

for(i in 12:19){
  colnames(dat)[i+9] = paste0("sc.",colnames(dat)[i])
}

factdat = dat[,c(10,11,21:28)]
pc = prcomp(factdat)
summary(pc)
plot(pc)

fact = factanal(factdat, factors=6, scores="regression", rotation="none")
fact
fact$loading

dat$fac1 = fact$scores[,1] # larger values indicate more herbaceous cover and less bare area
dat$fac2 = fact$scores[,2] # larger values indicate more shrubbiness and less bare area
dat$fac3 = fact$scores[,3] # larger values indicate more woody habitat
dat$fac4 = fact$scores[,4] # larger values indicate more tall grass and less short grass

# ggplot(dat,aes(x=fac1, y=fac2, color=Enclosure))+
# geom_point()
# ggplot(dat,aes(x=fac1, y=fac3, color=Enclosure))+
# geom_point()
# ggplot(dat,aes(x=fac2, y=fac4, color=Enclosure))+
# geom_point()
# ggplot(dat,aes(x=fac2, y=fac4, color=Enclosure))+
# geom_point()
# ggplot(dat,aes(x=fac1, y=fac4, color=Enclosure))+
# geom_point()
# ggplot(dat,aes(x=fac1, y=fac3, color=Enclosure))+
# geom_point()

temp=dat%>%
  select(Cam.ID,fac1,fac2,fac3)%>%
  rename(Grid=Cam.ID)
write.csv(temp, "cam_fact.csv")
