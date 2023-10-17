#Load packages 
library(data.table) #needed to run function
library(dplyr) #needed to combine matrices 
library(ggplot2) #needed for graphing 
library(ggpubr) #needed for stats tests 

getwd()
setwd("~/Documents/experiments_2023/11_oct_23_dna_damage/datasheets_and_plotting")
Foci <- read.csv("oct_23_percent_foci.csv", stringsAsFactors = T) 

head(Foci)
str(Foci)
summary(Foci)

darker_blue_palette <- c("#001F3F", "#003366", "#004080", "#0059b3", "#0073e6","#D4E4F6", "#A9CCE3", "#7FB3D5", "#5499C7")

blue_palette <- c("#A9CCE3", "#7FB3D5", "#5499C7", "#0059b3")
desired_order<- c("0", "1-5", "6-9", ">10")
genotype_order<- c("wt25", "wt26", "Cfl2 ko1", "Cfl2 ko2")
custom_labels<- c("WT A", "WT B", "CFL2 KO1", "CFL2 KO2")


Foci$foci <- factor(Foci$foci, levels = desired_order)
Foci$genotype <- factor(Foci$genotype, levels = genotype_order)

#53bp1
p53bp1 <- ggplot(Foci) +
  geom_bar(aes(x = genotype, y = percent_53BP1, fill = foci),
           position = "stack",
           stat = "identity", width=0.8) +
  facet_grid(~ day, labeller=labeller(day=c("0" = "Day 0 Myoblasts", "4" = "Day 4 Myotubes","7" = "Day 7 Myotubes"))) +
  scale_fill_manual(values = blue_palette) +
  labs(x = NULL, y = "% 53BP1 Foci number per cell", fill = "# Foci") +
  theme_minimal() +
  scale_x_discrete(labels = custom_labels) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(strip.text = element_text(size = 11), axis.text.x = element_text(size = 10))

#Plot for black background

p53bp1_b <- ggplot(Foci) +
  geom_bar(aes(x = genotype, y = percent_53BP1, fill = foci),
           position = "stack",
           stat = "identity", width=0.8) +
  facet_grid(~ day, labeller=labeller(day=c("0" = "Day 0", "4" = "Day 4","7" = "Day 7"))) +
  scale_fill_manual(values = blue_palette) +
  labs(x = NULL, y = "% 53BP1 Foci number per cell", fill = "# Foci") +
  theme_minimal() +
  scale_x_discrete(labels = custom_labels) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color="#F0F0F0")) +
  theme(strip.text = element_text(size = 11, color="#F0F0F0"), axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(color="#F0F0F0", size = 13), axis.text.y = element_text(color="#F0F0F0"))+
  theme(legend.text = element_text(color = "#F0F0F0"), legend.title = element_text(color = "#F0F0F0"))

gH2AX_b <- ggplot(Foci) +
  geom_bar(aes(x = genotype, y = percent_γH2AX, fill = foci),
           position = "stack",
           stat = "identity", width=0.8) +
  facet_grid(~ day, labeller=labeller(day=c("0" = "Day 0", "4" = "Day 4","7" = "Day 7"))) +
  scale_fill_manual(values = blue_palette) +
  labs(x = NULL, y = "% γH2AX Foci number per cell", fill = "# Foci") +
  theme_minimal() +
  scale_x_discrete(labels = custom_labels) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color="#F0F0F0")) +
  theme(strip.text = element_text(size = 11, color="#F0F0F0"), axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(color="#F0F0F0", size = 13), axis.text.y = element_text(color="#F0F0F0"))+
  theme(legend.text = element_text(color = "#F0F0F0"), legend.title = element_text(color = "#F0F0F0"))

ggsave(filename = "53bp1.png", plot = p53bp1_b, device = "png", bg = "transparent", width = 4.95, height = 3.32)
ggsave(filename = "gH2AX.png", plot = gH2AX_b, device = "png", bg = "transparent", width = 4.95, height = 3.32)