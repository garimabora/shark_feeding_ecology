install.packages("ggplot2")                 # Install & load ggplot2
library("ggplot2")
ca=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/New folder/IRIgraphs_ca.csv")
cl=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/New folder/IRIgraphs_cl.csv")
cg=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/New folder/IRIgraphs_cg.csv")
sl=read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/New folder/IRIgraphs_sl.csv")

ggp1 <- ggplot(ca, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  labs(x ="Prey Family", y = "IRI%")
ggp1                                         # Draw vertical barplot in ggplot2

ggp11<-ggp1 +                                       # Horizontal barplot in ggplot2
  coord_flip()

ggp2 <- ggplot(cl, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  labs(x ="Prey Family", y = "IRI%")
ggp2                                         # Draw vertical barplot in ggplot2

ggp21<-ggp2 +                                       # Horizontal barplot in ggplot2
  coord_flip()
ggp21

ggp3 <- ggplot(cg, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  labs(x ="Prey Family", y = "IRI%")
ggp3                                        # Draw vertical barplot in ggplot2

ggp31<-ggp3 +                                       # Horizontal barplot in ggplot2
  coord_flip()

ggp4 <- ggplot(sl, aes(reorder(Prey.Family,IRI.), IRI.)) +    # Create vertical barplot in ggplot2
  geom_bar(stat = "identity")+
  labs(x ="Prey Family", y = "IRI%")
ggp4                                        # Draw vertical barplot in ggplot2

ggp41<-ggp4 +                                       # Horizontal barplot in ggplot2
  coord_flip()


library(grid)
library(gridExtra)
grid.arrange(ggp21, ggp41,ggp31,ggp11,ncol = 2)