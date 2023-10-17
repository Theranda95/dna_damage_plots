install.packages("beeswarm")
install.packages("ggbeeswarm")
library(ggbeeswarm)
library(beeswarm)
library(ggplot2) #needed for graphing 
library(ggpubr) #needed for stats tests 

getwd()
setwd("~/Documents/experiments_2023/11_oct_23_dna_damage/datasheets_and_plotting")
Foci_nuclei <- read.csv("oct_23_foci_per_nuclei.csv", stringsAsFactors = T) 

head(Foci_nuclei)
str(Foci_nuclei)
summary(Foci_nuclei)

genotype_colors <- c("#7FB3D5", "#0059b3")

# beeswarm(X53BP1 ~ genotype, data=Foci_nuclei,
# col =c("#7FB3D5", "#7FB3D5","#0059b3","#0059b3"), # Specify color for the points
# method = "swarm",
# pch = 16,       # Specify point shape (16 for filled circles)
# main = "53BP1 Foci",  # Specify plot title
# xlab = NULL,       # Specify x-axis label
# ylab = "53BP1 foci number",
# cex = 0.5 # Specify the overlap between point
# )

#ggplot beeswarm plots from Victoria's code --------------------------------------

  ##Prep for plotting 
order <- c("wt25","wt26","ko1","ko2")
label <- c("WT A", "WT B", "CFL2 KO1", "CFL2 KO2")
Foci_nuclei$genotype <- factor(Foci_nuclei$genotype, levels = order,
                               labels = c("WT A", "WT B", "CFL2 KO1", "CFL2 KO2"))
comparisons <- list( c("WT A", "WT B"), c("WT A", "CFL2 KO1"), c("WT A", "CFL2 KO2"), c("WT B", "CFL2 KO1"), 
                     c("WT B", "CFL2 KO2"), c("CFL2 KO1", "CFL2 KO2")) 

palette <- c("wt25" = "#7FB3D5", "ko1" = "#0059b3", "wt26" = "#7FB3D5", "ko2" = "#0059b3")
# Plot 
P53BP1_nuclei <- ggplot(Foci_nuclei,
                        aes(x = genotype, y = X53BP1)) +
  #indicate what plot you want and define characteristics 
  geom_beeswarm(size = 1.5,
                cex = 0.7,
                alpha =0.6,
                dodge.width=0.9,
                aes(color = genotype)) +
    #Add title 
    labs(x=NULL, 
         y= "53BP1 Foci number per nuclei") +
    #theme that simplifies the grid 
    theme_minimal() +
    #changing the size of x axis value labels, color and theme 
    theme(axis.text.x=element_text(size=13),
          axis.title.y = element_text(size = 13, vjust = 2),
          axis.text.y = element_text(size = 12),
          legend.position = "none") +
  #add the stats to the plot
    #The options mean_sd and mean_sdl represent mean ± standard deviation 
    #or the mean ± a multiple of the standard deviation respectively.
  stat_compare_means(comparisons=comparisons, size=4, label="p.signif", 
                     step.increase = 0.1) +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color="black") +
  stat_summary(fun.data = mean_sd,
               geom = "errorbar",
               width = 0.25,
               color="black") + 
 scale_color_manual(values = c("#7FB3D5", "#0059b3"))

?theme
