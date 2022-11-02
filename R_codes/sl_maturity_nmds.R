library(vegan)
library(dplyr)
library(ggplot2)

pc = read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/onto.csv")
com <- pc%>% 
  filter_all(any_vars(. != 0))
m_com = as.matrix(com)
set.seed(123)
nmds = metaMDS(m_com, distance = "bray",autotransform = FALSE,k=2)
nmds
plot(nmds)

data.scores = as.data.frame(scores(nmds)$sites)
pc1 = read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/gc_env3.csv")
data.scores$Maturity = pc1$Maturity
head(data.scores)

species.scores <- as.data.frame(scores(nmds, "species"))
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

grp.a <- data.scores[data.scores$Maturity == "Neonate", ][chull(data.scores[data.scores$Maturity == 
                                                                   "Neonate", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$Maturity == "Juvenile", ][chull(data.scores[data.scores$Maturity == 
                                                                   "Juvenile", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
grp.c <- data.scores[data.scores$Maturity == "Adult", ][chull(data.scores[data.scores$Maturity == 
                                                                   "Adult", c("NMDS1", "NMDS2")]), ]  # hull values for grp B

hull.data <- rbind(grp.a, grp.b,grp.c)  #combine grp.a and grp.b
hull.data 

ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Maturity,group=Maturity),alpha=0.30) + # add the convex hulls
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),cex=3,alpha=0.8) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=Maturity),size=2) + # add the point markers
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=12), # remove x-axis labels
        axis.title.y = element_text(size=12), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

ggplot(hull.data, aes(x=NMDS1,y=NMDS2,fill=Maturity,group=Maturity, color = Maturity)) +
  geom_point() +
  stat_ellipse()
