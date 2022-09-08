library(ggplot2)
library(tidyverse)

setwd("~/Documents/OrganismalBio/OrganismalBio")

dat <- read.csv("scales.csv")

#seeing how large data set is and first few lines
dim(dat) #shows how many rows and columns
head(dat) #shows column names

#how the data is stored within R
class(dat$N)
class(dat$quadrant)
class(dat$species)
class(dat$specimen)

#averages can only be taken on columns with numeric data
mean(dat[,1])

#faster way to classify all of the columns (applies a function over a vector)
sapply(dat,class)

#displaying species within the dataset
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species

#counting in R
length(species)

#checking how many of each species
dat$species==species[1]
dat$species[dat$species==species[1]]
A.rup<-length(dat$species[dat$species==species[1]])
L.gib<-length(dat$species[dat$species==species[2]])
L.mac<-length(dat$species[dat$species==species[3]])
M.sal<-length(dat$species[dat$species==species[4]])
M.sax<-length(dat$species[dat$species==species[5]])
P.fla<-length(dat$species[dat$species==species[6]])
#combine the results with species
species.obs <- data.frame(sp=species,n=c(A.rup,L.gib,L.mac,M.sal,M.sax,P.fla))
species.obs

#Pipe Functions
species.n <- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

dat %>%
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#loops
for(i in 1:10) print(i)

for(i in species){
  p <- dat %>%
    filter(species==i) %>%
    ggplot()+geom_boxplot(aes(x=quadrant, y=N))+ggtitle(i)
  print(p)
}

#Save PDF of figures
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

list.files(pattern=".pdf")
