#9/12
#What is the pipe(%>%)?
  #Reduces repetitive tasks
library(tidyverse)
library(ggplot2)
iris <- group_by(iris,Species)
summarise(iris,mean_length=mean(Sepal.Length))
#check for error

iris%>%
  group_by(Species)%>%
  summarize(mean_length=mean(Sepal.Length))

#error Below
iris%>%
  group_by(Species)%>%
  summarize(mean_length=mean(Sepal.Length))%>%
  ggplot(aes(x=Species, y=mean_length)+geom_bar(stat="identity"))

         