#Example for the Common Raven
library(rgbif)
library(tidyverse)
library(MuMIn)
library(rnoaa)
library(data.table)
library(ggmap)
library(usmap)
library(magick)#for examples
library(cowplot)#for examples
library(lme4) #for linear mixed effect models
library(car) #for LME anova testing
library(data.table) #for frollmean function (and others)

raven <- occ_data(scientificName = "Corvus corax", stateProvince="Maine", limit=200,year=2018)

#get the state of ME from ggmaps map data
ME<- map_data('state', 'maine')

raven.p <- ggplot(ME, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=raven[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()

#and add image to the map with cowplot
raven.p2 <- ggdraw() +
  draw_image("https://www.allaboutbirds.org/guide/assets/photo/63739491-480px.jpg",scale = 0.3,halign=0,valign=1) +
  draw_plot(raven.p)
print(raven.p2)

names(raven)
lapply(raven,head)

species <- c("Myiarchus crinitus","Icterus galbula","Pheucticus ludovicianus","Coccyzus americanus","Setophaga caerulescens")

y <- paste0("1990",",","2019")
m <- paste0("3",",","6")

dat.l <-list()

for(s in species){
  
  ## setting the limit=0 returns no records but can give you the number of observations if you access the meta data
  n.obs <-  occ_data(scientificName = s,year=y,month=m,limit=0,country="US",basisOfRecord = "HUMAN_OBSERVATION",stateProvince="Massachusetts")$meta$count 
  
  print(n.obs)
  
  
  dat.l[[paste0(s)]] <- occ_data(scientificName = s,year=y,month=m,
                                 limit=n.obs,country="US",
                                 basisOfRecord = "HUMAN_OBSERVATION",
                                 stateProvince="Massachusetts")[[2]]
  
  
}

dat <- rbindlist(dat.l,fill=T)

head(dat)

saveRDS(data,"massbird.data.RDS")

library(tidyverse)
dat <- readRDS("massbird.data.RDS")
