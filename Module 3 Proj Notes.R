library(tidyverse)
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>% #Omits any missing data, so we are just working with a complete data set
  print()

anole.log <- anole2%>%
  mutate_at(c("SVL","HTotal","PH","ArbPD"),log)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")
   #HTotal=(alpha)*SVL+(beta)

anole.lm <- lm(HTotal~SVL,anole2) 

coef(anole.lm)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2],intercept=coef(anole.lm)[1],col="blue")

SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)

pred.lm <- tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm,newdata = data.frame(SVL=SVL2))
  )

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm,aes(SVL,H.pred),col="blue")

summary(anole.lm)

anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1),data=anole2)

summary(anole.allo)

#AICc from the MuMIn package
anole.aic <- AICc(anole.allo)

#ERROR IN THIS FUNCTION
#aicw from the geiger package
anole.aicw <- aicw(anole.aic$AICc)

#More Complex Visualizations and Models
anole.log%>%
  ggplot(aes(HTotal,SVL,col=Ecomorph2))+geom_point()+geom_smooth(method="lm")

anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2,anole.log)
summary(anole.log.eco.lm)

anova(anole.log.eco.lm)
