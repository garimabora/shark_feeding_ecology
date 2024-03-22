library(dplyr)
library(magrittr)
gc<-read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/gc_data.csv")
IRI <- gc%>%
  filter(Prey.Family != "")%>%
  filter(Shark.Species == "Scoliodon laticaudus" | Shark.Species == "Carcharhinus limbatus" | Shark.Species == "Chiloscyllium arabicum" | Shark.Species == "Chiloscyllium griseum")%>%
  group_by(Shark.Species)%>%
  mutate(Fr = length(unique(Shark_ID)), # total number of snakes sampled
         W = sum(Weight_gms, na.rm = T), # total weight of prey sampled
         N = n())%>% # total number of prey sampled
  group_by(Shark.Species, Prey.Family)%>%
  summarise(f = length(unique(Shark_ID)), # number snakes in which prey family occured
            Fr = last(Fr),
            w = sum(Weight_gms, na.rm = T), # weight of prey family
            W = last(W),
            n = n(), # number of individuals of prey family
            N = last(N))%>%
  group_by(Shark.Species, Prey.Family)%>%
  # caluclating percentages
  summarise(per.F = f*100/Fr,
            per.W = w*100/W,
            per.N = n*100/N,
            # caluclating IRI
            IRI = (per.N + per.W)*per.F)
library(writexl)
write_xlsx(IRI, "C:/Users/garim/Desktop/MISC/dissertation/primary data/IRI_R.xlsx")
 

library("ggplot2")
ca=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/IRIgraphs_ca.csv")
cl=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/IRIgraphs_cl.csv")
cg=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/IRIgraphs_cg.csv")
sl=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/IRIgraphs_sl.csv")

ggp1 <- ggplot(ca, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  theme(axis.text = element_text(size = 14))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x ="Prey Family", y = "IRI%")
ggp1                                         # Draw vertical barplot in ggplot2

ggp11<-ggp1 +                                       # Horizontal barplot in ggplot2
  coord_flip()

ggp2 <- ggplot(cl, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  theme(axis.text = element_text(size = 14))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x ="Prey Family", y = "IRI%")
ggp2                                         # Draw vertical barplot in ggplot2

ggp21<-ggp2 +                                       # Horizontal barplot in ggplot2
  coord_flip()
ggp21

ggp3 <- ggplot(cg, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x ="Prey Family", y = "IRI%")
ggp3                                        # Draw vertical barplot in ggplot2

ggp31<-ggp3 +                                       # Horizontal barplot in ggplot2
  coord_flip()

ggp4 <- ggplot(sl, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  theme(axis.text = element_text(size = 14))+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))+
  labs(x ="Prey Family", y = "IRI%")
ggp4                                        # Draw vertical barplot in ggplot2

ggp41<-ggp4 +                                       # Horizontal barplot in ggplot2
  coord_flip()


library(grid)
library(gridExtra)
grid.arrange(ggp21, ggp41,ggp31,ggp11,ncol = 2)
