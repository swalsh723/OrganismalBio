library(tidyverse)
library(Momocs)

f <- list.files("class_out_data",pattern=".txt|.csv",full.names = TRUE)


out <- read_delim(f[1],delim="\t") %>% 
  as.matrix()

out %>% 
  list() %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()

library(vroom)
#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename")

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

outs.l %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()

#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename")

#add wing info
out.df <- out.df %>% 
  mutate(wing=gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(filename))) %>% 
  na.omit()

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

#extract wing info
wings <- gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(names(outs.l)))

outs <-  outs.l %>% 
  Out(fac=list(wing=wings)) %>% 
  coo_flipx()

forewings <- outs %>% 
  filter(wing=="forewing")

hindwings <- outs %>% 
  filter(wing=="hindwing")

forewings %>% 
  stack()

hindwings %>% 
  stack()

fore.min <- forewings %>% 
  coo_nb() %>% 
  min()

forewings %>%
  coo_interpolate(fore.min) %>% 
  fgProcrustes() %>% 
  stack()

hind.min <- hindwings %>% 
  coo_nb() %>% 
  min()

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_slide(id=1) %>% 
  coo_align()  %>%
  fgProcrustes() %>%
  stack()

forewings %>%
  coo_interpolate(fore.min) %>% 
  coo_align()  %>%
  fgProcrustes() %>% 
  efourier(norm=FALSE) 

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  fgProcrustes() %>% 
  efourier(norm=FALSE)

forewing.pca <- forewings %>%
  coo_interpolate(fore.min) %>%
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()

hindwing.pca <-hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  stack

forewing.pca %>% 
  plot_PCA(title = "forewings")

hindwing.pca %>% 
  plot_PCA(title = "hindwings")

#Comparative Analysis
library(ape)

lep.tree <- ape::read.tree("lep_tree2.tre")

plot(lep.tree,cex=0.1)

lep.tree <- ladderize(lep.tree)
plot(lep.tree,cex=0.1)

lep.tree$tip.label <- gsub("_"," ",lep.tree$tip.label)

basename(names(outs))[1:5]
lep.sp <- read_csv("lep_image_data.csv")
head(lep.sp)
head(lep.sp$identifier)

out.data <- tibble(xy.file=basename(names(outs))) %>% 
  mutate(identifier=gsub("XY_|_hindwing|_forewing|.txt","",xy.file)) %>% 
  left_join(lep.sp)
head(out.data)
head(hindwing.pca$x,1)
head(forewing.pca$x,1)
hindwing.pca2 <-  tibble(xy.file=basename(rownames(hindwing.pca$x)),PC1=hindwing.pca$x[,1],PC2=hindwing.pca$x[,2]) %>% 
  left_join(out.data)
forewing.pca2 <-  tibble(xy.file=basename(rownames(forewing.pca$x)),PC1=forewing.pca$x[,1],PC2=forewing.pca$x[,2])%>% 
  left_join(out.data)

#Evolutionary Rates
drops <- lep.tree$tip.label[!lep.tree$tip.label%in%unique(out.data$species)]

lep.tree2 <- drop.tip(lep.tree,drops)

plot(lep.tree2,cex=0.5)

#PC1s
hind.pc1 <- hindwing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull

names(hind.pc1) <-  hindwing.pca2%>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(species)

fore.pc1 <- forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(PC1)

names(fore.pc1) <-  forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC1=mean(PC1)) %>% 
  pull(species)

#PC2s
hind.pc2 <- hindwing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(PC2)

names(hind.pc2) <-  hindwing.pca2%>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>%
  summarize(PC2=mean(PC2)) %>% 
  pull(species)

fore.pc2 <- forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(PC2)

names(fore.pc2) <-  forewing.pca2 %>% 
  filter(species%in% lep.tree2$tip.label) %>% 
  group_by(species) %>% 
  summarize(PC2=mean(PC2)) %>% 
  pull(species)

library(phytools)
forePC1.BM<-brownie.lite(lep.tree2,fore.pc1*10)
hindPC1.BM<-brownie.lite(lep.tree2,hind.pc1*10)

forePC2.BM<-brownie.lite(lep.tree2,fore.pc2*10)
hindPC2.BM<-brownie.lite(lep.tree2,hind.pc2*10)
forePC1.BM$sig2.single

#Shifts in Evolutionary Rates
library(RRphylo)
#ERROR FOUND
hindPC1.RR <- RRphylo(tree=lep.tree2,y=hind.pc1)
hindPC1.RR$rates
