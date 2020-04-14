#Figure for BIOL812 final assignment
#Want to show the change in gene expression throughout development in different tissues for 6 different genes
#Tissues we are using are the head (H), midgut (G), and fatbody (F)

library(ggplot2)
library(dplyr)
library(cowplot)
library(grid)
library(gridExtra)
library(scales)

load("MeltedData.RData")

mydat <- dat2
mydat$tissue <- as.factor(mydat$tissue)
mydat$life_stage <- as.factor(mydat$life_stage)
#reorder life stage so it is in chronological order
mydat$life_stage <- factor(mydat$life_stage, 
                           levels = c("E", "L1", "L2", "L3", "L4", "L5", "P", "A"))
#reorder tissue
mydat$tissue <- factor(mydat$tissue, levels = c("H", "G", "F"))

#make a colour theme
mycols <- c("#FFCAFE", "#FFA670", "#BDD884") #head (pink), midgut (orange), fat (green)
#vector with gene names to make labelling easier
genes <- c("Msex2.07524", "Msex2.15420", "Msex2.14343", 
           "Msex2.04431", "Msex2.01694", "Msex2.10735")

#First, I make individual graphs for each gene

#gene Msex2.15420 (gene A) **y-axis order of magnitude bigger
Adat <- mydat %>% 
  filter(gene_id == "Msex2.15420", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

A <- ggplot(Adat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none",
        axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 300000), breaks = seq(0, 300000, 50000), labels = comma) +
  scale_colour_manual(labels = c("Head", "Midgut", "Fatbody"), values = mycols)

#gene Msex2.04431 (gene B)
Bdat <- mydat %>% 
  filter(gene_id == "Msex2.04431", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) 

B <- ggplot(Bdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 82120.42), breaks = seq(0, 80000, 10000), labels = comma) +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)

#gene Msex2.14343 (gene C)
Cdat <- mydat %>% 
  filter(gene_id == "Msex2.14343", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) 

C <- ggplot(Cdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = c(0.8, 0.92), 
        legend.title = element_blank(), legend.text = element_text(size = 14),
        axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, 10000), labels = comma) +
  scale_colour_manual(labels = c("Head", "Midgut", "Fatbody"), values = mycols)

#in 3 of the genes, variation in two tissues is masked by large changes in expression in a different tissue
#for these graphs I made a smaller graph showing the smaller changes and superimposed it on the full graph

#gene Msex2.07524 (gene D) 
Ddat <- mydat %>% 
  filter(gene_id == "Msex2.07524", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) 

#small graph with just fatbody and midgut
Ddat_noH <- Ddat %>% filter(tissue != "H")

Ds <- ggplot(Ddat_noH, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), 
        legend.position = "none", axis.ticks.x.bottom = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        axis.line = element_blank()) +
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, 15)) +
  scale_colour_manual(labels = c("Midgut", "Fatbody"), values = mycols[c(2,3)])

#Gene D big graph with small graph on top

Db <- ggplot(Ddat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none") +
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, 10000), labels = comma) +
  scale_x_discrete(labels = c("Second\ninstar\nlarva", "Third\ninstar\nlarva", 
                              "Fourth\ninstar\nlarva", "Fifth\ninstar\nlarva", "Pupa", "Adult")) +
  scale_colour_manual(labels = c("Head", "Midgut", "Fatbody"), values = mycols) +
  annotation_custom(ggplotGrob(As), xmin = 0.6, xmax = 3.1, ymin = 50000, ymax = 80000)

#gene Msex2.01694 (gene E)
Edat <- mydat %>% 
  filter(gene_id == "Msex2.01694", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) 

#small graph with just midgut and head
Edat_noF <- Edat %>% filter(tissue != "F")

Es <- ggplot(Edat_noF, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), 
        legend.position = "none", axis.ticks.x.bottom = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        axis.line = element_blank()) +
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, 15)) +
  scale_colour_manual(labels = c("Head", "Midgut"), values = mycols[c(1,2)])

#Gene E big graph with small graph on top
Eb <- ggplot(Edat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none") +
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, 10000), labels = comma) +
  scale_x_discrete(labels = c("Second\ninstar\nlarva", "Third\ninstar\nlarva", 
                              "Fourth\ninstar\nlarva", "Fifth\ninstar\nlarva", "Pupa", "Adult")) +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols) +
  annotation_custom(ggplotGrob(Es), xmin = 0.6, xmax = 3.1, ymin = 50000, ymax = 80000)


#gene Msex2.10735 (gene F)
Fdat <- mydat %>% 
  filter(gene_id == "Msex2.10735", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) 

#small graph with just midgut and head
Fdat_noF <- Fdat %>% filter(tissue !="F")

Fs <- ggplot(Fdat_noF, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), 
        legend.position = "none", axis.ticks.x.bottom = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        axis.line = element_blank()) +
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, 15)) +
  scale_colour_manual(labels = c("Midgut", "Head"), values = mycols[c(1,2)])

#big graph
Fb <- ggplot(Fdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none") +
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, 10000), labels = comma) +
  scale_x_discrete(labels = c("Second\ninstar\nlarva", "Third\ninstar\nlarva", 
                              "Fourth\ninstar\nlarva", "Fifth\ninstar\nlarva", "Pupa", "Adult")) +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols) +
  annotation_custom(ggplotGrob(Fs), xmin = 0.6, xmax = 3.1, ymin = 50000, ymax = 80000)

#Make composite plot

#save figure
png(filename = "gene_fig.png", width = 10, height = 7, units = "in", 
    pointsize = 12, bg = "white",  res = 300)

plot <- plot_grid(A, B, C, Db, Eb, Fb, ncol = 3, nrow = 2, labels = genes, align = "hv")
#want one common y axis label
y.lab <- textGrob("Mean gene expression (FPKM)", rot = 90)
#combine composite plot and y-axis label
grid.arrange(arrangeGrob(plot, left = y.lab))

dev.off()


