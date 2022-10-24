library(tidyverse)
library(Momocs)

f <- list.files("lep_examples",pattern=".txt",full.names = TRUE)

out <- read_delim(f[1],delim="\t") %>% 
  as.matrix()

out %>% 
  list() %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()

outs.l <- list()

for(i in f){
  outs.l[[i]] <- read_delim(i,delim="\t") %>% 
    as.matrix()
}

outs.l %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()

outs.l <- list()
wing.l <- list()
for(i in f){
  outs.l[[i]] <- read_delim(i,delim="\t") %>% 
    as.matrix()
  wing.l[[i]] <- gsub(".+_(hindwing|forewing).txt","\\1",i)
}

outs <- outs.l %>% 
  Out(fac=list(wing=wing.l %>% unlist)) %>% 
  coo_flipx()

forewings <- outs %>% 
  filter(wing=="forewing")

hindwings <- outs %>% 
  filter(wing=="hindwing")

forewings %>% 
  stack()

hindwings %>% 
  stack()

#Making all the outlines the same size for comparisons
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
  fgProcrustes() %>% 
  stack()

forewings %>%
  coo_interpolate(fore.min) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) 

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE)

forewing.pca <- forewings %>%
  coo_interpolate(fore.min) %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE,
           smooth.it=0) %>% 
  PCA()

hindwing.pca <-hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE,
           smooth.it=0) %>% 
  PCA()

forewing.pca %>% 
  plot_PCA(title = "forewings")

hindwing.pca %>% 
  plot_PCA(title = "hindwings")
