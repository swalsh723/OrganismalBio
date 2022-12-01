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

#RAVEN EXAMPLE
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

#Begining with our specific Species
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
dat%>%
  group_by(year,species)%>%
  summarise(count=sum(individualCount,na.rm = T))%>%
  ggplot(aes(x=year,y=count,col=species))+geom_point()

#Querying NOAA's NCDC API
options(noaakey = "GqNLpDDJwlGNWARNWhtgplTBddekfvYO")
sts <- c(
  "GHCND:USW00013894", #Mobible, AL 2k away about 10 days away @200 km/day
  "GHCND:USW00013881", #Charlotte, NC 1000 km away about 6 days away @200 km/day
  "GHCND:USW00014739" #Boston
)
bos <- ncdc_stations(stationid = "GHCND:USW00014739")

print(bos)

library(maptools)
library(rgdal)
sta.d <- bind_rows( #bind the rows
  lapply(sts,function(x) ncdc_stations(stationid = x)$data ) #use lapply to run through stations
)%>%
  mutate(usmap_transform(.,input_names = c("longitude","latitude"),output_names = c("longitude.1", "latitude.1")))%>% #join transformation of lat/long for projection with usmap
  mutate(name=str_sub(name, -5,-4))%>%#simplify the name column, grab just the state
  mutate(migr.day=c(10,5,0))%>% #so we can look at wind speed 0, 5 or 10 days before arrive in boston
  separate(id,into = c("station.type","id"))%>%#need to cut station type out from station id number
  print()

plot_usmap(
  include = c(.northeast_region,.south_region,.east_north_central)
)+geom_point(data=sta.d,aes(x=longitude.1,y=latitude.1,col=name),size=5)+geom_label(data=sta.d,aes(x=longitude.1,y=latitude.1,col=name,label=name),size=5,nudge_x = 1e6*0.25)+theme(legend.position = "none")

weather.d <- meteo_pull_monitors(sta.d$id,date_min = "2000-01-01")
head(weather.d)

#Data Analysis
#Preparing Ebird
mc<- dat%>%
  filter(species=="Setophaga caerulescens")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>1999)

mc%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

mc.pred <- mc%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))),#predict the logistic curve for each species
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(mc%>%dplyr::select(j.day,date)) ## add date back to tibble

mc%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=mc.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

mc.arrive.date <-mc.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

mc.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Preparing Weather data
weather.d <- weather.d%>%
  mutate(year=as.integer(str_sub(date,1,4)), #add year
         date=as.Date(date))%>%
  group_by(year)%>% #group by year so we can compute julian day
  mutate(j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01"))), #add julian day
         date2=date,
         wdir.rad=(180-abs(wdf2-180))*pi/180, #radians so we can use a trig function to compute wind vector, scale degrees first to 180 scale to 2x pi and subtract from 180 (wind comes out of a direction)
         wvec=cos(wdir.rad)*-1*awnd # we want a negative value for positive value for 2x pi
  )%>% #store day in new column
  dplyr::select(id,year,date2,j.day,tmin,tmax,wvec)%>% #select the rows we need
  left_join(sta.d%>%select(id,name,migr.day))%>% #add the station id info (ie. name)
  mutate(j.day=j.day+migr.day)#make j.day ahead of BOS according to the migration days away so we can join weather along path

#Combining the Two
mc.arr.weath <- mc.arrive.date%>%
  left_join(weather.d)%>%
  left_join(mc%>%dplyr::select(year,date,j.day))
head(mc.arr.weath)

weather.wk <-weather.d %>% 
  group_by(year,name) %>% 
  mutate(wk.tmin = frollmean(tmin, n=14,align="right"),
         wk.tmax = frollmean(tmax, n=14,align="right"),
         wk.wvec = frollmean(wvec, n=14,align="right")
  )%>%
  dplyr::select(j.day,date2,name,wk.tmin,wk.tmax,wk.wvec)

mc.arr.weath2 <- mc.arrive.date%>%
  left_join(weather.wk)
head(mc.arr.weath2)

#Linear Mixed-effect Modeling
#weather at 0, 5, and 10 days away from arrival
mc.lmer <- lmer(j.day~tmin*tmax*wvec+(1|name),mc.arr.weath,na.action = "na.fail")
Anova(mc.lmer) #Anova from the car package
#0Mean two week weather preceding arrival
mc.lmer2 <- lmer(j.day~wk.tmin*wk.tmax*wk.wvec+(1|name),mc.arr.weath2,na.action = "na.fail")
Anova(mc.lmer2) 

mc.arr.aic <- dredge(mc.lmer2,fixed = c("wk.tmin","wk.tmax","wk.wvec"),)

mc.kb <- kable(mc.arr.aic[1:4,],caption = "Fit values for nested models of the most complicated lme model")

kable_styling(mc.kb)

best.lmer <-  lmer(j.day~wk.tmin+wk.tmax+wk.wvec+(1|name),mc.arr.weath2,na.action = "na.fail")

Anova(best.lmer)

