library(vegan)
library(dplyr)
library(ggplot2)

shark_diet = read.csv("C:/Users/garim/Desktop/MISC/dissertation/primary data/csp_gc_obj2.csv")

diet_matrix <- as.matrix(shark_diet[,2:12])
# Calculate Bray-Curtis dissimilarity matrix
dissimilarity_matrix <- vegdist(diet_matrix, method = "bray")

# Perform NMDS analysis
nmds_results <- metaMDS(dissimilarity_matrix, distance = "bray", k = 2)
nmds_stress <- nmds_results$stress
# Create data frame of NMDS coordinates and species information
nmds_df <- data.frame(nmds_results$points, Species = shark_diet$Carpetshark.species)

# Create NMDS plot
plot<-ggplot(nmds_df, aes(x = MDS1, y = MDS2, color = Species))+
  geom_point(size=3)+
  stat_ellipse(type="t",level=0.95,
               geom="polygon",
               alpha=0.25)+
  stat_ellipse(type="norm",linetype=2)+
  scale_color_manual(values = c("#FFA500", "#008080")) +
  labs(x = "NMDS1", y = "NMDS2", color = "Species")+
  ggtitle(paste0("NMDS plot with stress value = ", round(nmds_stress, 3)))
plot + theme(legend.title = element_text(size = 14),legend.text = element_text(size = 12))