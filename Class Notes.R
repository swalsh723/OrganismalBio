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

#9/16
yA <- rnorm(100,mean=10)
yB<- rnorm(100,mean=10)
yC <- rnorm(100,mean=10)



dat <- rbind(tibble(y=yA,spec="A"),
             tibble(y=yB,spec="B"),
             tibble(y=yC,spec="C")
             )%>%mutate(speed=rep(1:5,60))

speed <- expand_grid(
  species=c("A","B","C"),
  speed=1:5,
  date=paste("May-",1:5)
)

dat%>%
  left_join(speed,on=c("spec"="species"))

dat%>%
  group_by(spec)%>%
  summarize(m=mean(y),sd=sd(y),se=sd(y)/sqrt(length(y)))%>%
  print()%>%
  ggplot(aes(spec,m))+geom_point()+geom_errorbar(aes(ymin=m-se,ymax=m+se))

hist(y)

x <- 1:100

plot(x,y)


#9/19
#Module 3: The Whiz and Viz Bang of Data: The Basics of Visualization and Modeling
# ~ this means that left-side is predicted by the right-side
library(tidyverse)
set.seed(123)
x.A=1:50
y.A=x.A*2+runif(50,1,200)
x.B=1:50
y.B=x.B*3.5+runif(50,1,200)
d <- tibble(x=c(x.A,x.B),y=c(y.A,y.B),species=c(rep("A",50),rep("B",50)))
d%>%
  ggplot(aes(x,y,col=species))+geom_point()+geom_smooth(method="lm")

#Species as a factor (different means): lm(y~x+spec)
#Species cause difference in slope: lm(y~x*spec)

spec.lm1 <- lm(y~x+species,data=d)
anova(spec.lm1) #statistical test
summary(spec.lm1)

spec.lm2 <- lm(y~x,d)
summary(spec.lm2)

spec.lm3 <- lm(y~x*species,d)
summary(spec.lm3)
anova(spec.lm3)
AIC(spec.lm1,spec.lm3) #AIC=2k-2ln(L)
#k=Number of model parameters, L=L(theta)=max value of the likelihood function of the model


#9/22
#Brownian motion = High Energy motion (Wobble)

#Phylogenetic Least Squares

#PGLS under BM
gls(y~x, correlation = corBrownian(1,phy = tree, form = ~Species),data=dat,method
    = "ML")
