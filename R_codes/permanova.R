library(vegan)
library(dplyr)
library(tidyr)
##PERMANOVA pelagic and benthic
gc<-read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/benthic_pelagic.csv")
gc.env<-read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/gc_env1.csv")
adonis2(gc ~ Type, data = gc.env, method="bray")
gc.env%>%
  group_by(Type)%>%
  summarise(N=length(Type))

##PERMANOVA chiloscyllium griesum and arabicum
gc1<-read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/chilo.csv")
gc.env1<-read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/gc_env2.csv")
adonis2(gc1 ~ Shark, data = gc.env1, method="bray")
gc.env1%>%
  group_by(Shark)%>%
  summarise(N=length(Shark))

##PERMANOVA ontogenetic shift scoliodon
gc2<-read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/onto.csv")
gc.env2<-read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/gc_env3.csv")

