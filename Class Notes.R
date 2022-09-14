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

         
#9/14
# the ... allows to add additional parameters into the function after 
#within the embedded functions
fun <- function(n=10,...){
  runif(n=n)
}

fun(n=50,max=2)
