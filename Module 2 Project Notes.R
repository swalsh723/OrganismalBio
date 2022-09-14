library(tidyverse)
library(ggplot2)

setwd("~/Documents/OrganismalBio/OrganismalBio")

#loading the datasets
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#merging the pseed and speeds data tables
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

#Adding Data to Data from its Own Data
pseed.bl%>%
  print()

pseed2%>%
  select(fish)%>%
  unique()

#adding BL to the table
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

#Added Body length speed variable to the table
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
#Last save of pseed2 before pivot

#plotting specific speed by relative amplitude
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()

#Adjusting the transparency of the points
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)

#Plotting just the left pelvic fin in one experiment
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

#Customized Functions
library(features)

expl <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <- features(x=expl$frame,y=expl$amp.bl)->f1
fget(f1)

#plotting the amplitudes paths
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)

#separating troughs and peaks
f2 <- features(x=expl$frame,y=expl$amp.bl*100)
fget(f2)

#pulling out only the peaks
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

#creating a new column for the peaks
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

#finding max amplitude across set of frames
pseed2%>%
  summarize(n=length(unique(date)))

#creating custom function to find peaks
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

#plotting and filtering data into different dates and fins
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

#creating a new filter
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

#plotting data through filter (Checking if amp decreases with speed)
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

#confirming above theory
amp.aov <- aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)

#Plot of means vs. speed for each fish
pseed.max%>%
  group_by(fish,bl.s)%>%
  summarize(mean.max=mean(amp.bl))%>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#Pivoting Data
pseed2

pseed2 <- pseed2%>%
  group_by(date,frame)%>%
  mutate(amp.sum=sum(amp.bl))

pseed2%>%
  filter(fin=="R")

#Pivoting the data wide
pseed.wide <- pseed2%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl)%>%
  mutate(amp.sum=L+R)%>%
  print()
