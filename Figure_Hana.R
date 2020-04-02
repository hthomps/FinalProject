#Creating a figure showing change in gene expression throughout development in different tissues
#Tissues we are using are the head (H), midgut (G), and fatbody (F)

library(ggplot2)
library(dplyr)
library(cowplot)
library(grid)
library(gridExtra)

load("MeltedData.RData")

mydat <- dat2
mydat$tissue <- as.factor(mydat$tissue)
mydat$life_stage <- as.factor(mydat$life_stage)
#reorder life stage so it is in chronological order
mydat$life_stage <- factor(mydat$life_stage, 
                           levels = c("E", "L1", "L2", "L3", "L4", "L5", "P", "A"))

#make a colour theme
mycols <- c("#BED985", "#5B503A", "plum2")
#vector with gene names to make labelling easier
genes <- c("Msex2.07524", "Msex2.15420", "Msex2.14343", 
           "Msex2.04431", "Msex2.01694", "Msex2.10735")

#start with gene Msex2.07524 (gene A) 
Adat <- mydat %>% 
  filter(gene_id == "Msex2.07524", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

A <- ggplot(Adat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, 10000), name = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")
#with facet grid - use this one
#can I change relative size of panels? seems like the bottom one should be bigger
Af <- ggplot(Adat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  facet_grid(scales = "free", rows = vars(tissue)) +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none",
        strip.background = element_blank(), strip.text = element_blank()) +
  ylab(label = "Copy number") +
  scale_x_discrete(labels = c("Second \n instar larva", "Third instar \n larva", 
                              "Fourth \n instar larva", "Fifth instar \n larva", "Pupa", "Adult")) +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)



#now gene Msex2.15420 (gene B)
Bdat <- mydat %>% 
  filter(gene_id == "Msex2.15420", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

B <- ggplot(Bdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none",
        axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 300000), breaks = seq(0, 300000, 100000), name = "Copy number") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)

#now gene Msex2.14343 (gene C)
Cdat <- mydat %>% 
  filter(gene_id == "Msex2.14343", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

C <- ggplot(Cdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = c(0.8, 0.92), 
        legend.title = element_blank(), legend.text = element_text(size = 14),
        axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 50000), breaks = seq(0, 50000, 10000), name = "Copy number") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)

#now gene Msex2.04431 (gene D)
Ddat <- mydat %>% 
  filter(gene_id == "Msex2.04431", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

D <- ggplot(Ddat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none") +
  scale_y_continuous(limits = c(0, 83000), breaks = seq(0, 80000, 20000), name = "Copy number") +
  scale_x_discrete(labels = c("Second \n instar larva", "Third instar \n larva", 
                              "Fourth \n instar larva", "Fifth instar \n larva", "Pupa", "Adult"),
                   name = "Life stage") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)

#now gene Msex2.01694 (gene E)
Edat <- mydat %>% 
  filter(gene_id == "Msex2.01694", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

E <- ggplot(Edat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 70000), breaks = seq(0, 70000, 10000), name = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")

#with facet grid
Ef <- ggplot(Edat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  facet_grid(scales = "free", rows = vars(tissue)) +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none",
        strip.background = element_blank(), strip.text = element_blank()) +
  ylab(label = "Copy number") +
  scale_x_discrete(labels = c("Second instar \n larva", "Third \n instar larva", 
                              "Fourth instar \n larva", "Fifth \n instar larva", "Pupa", "Adult"),
                   name = "Life stage") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)


#now gene Msex2.10735 (gene F)
Fdat <- mydat %>% 
  filter(gene_id == "Msex2.10735", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

FF <- ggplot(Fdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 30000), breaks = seq(0, 30000, 5000), name = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")

#with facet grid
Ff <- ggplot(Fdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  facet_grid(scales = "free", rows = vars(tissue)) +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none",
        strip.background = element_blank(), strip.text = element_blank()) +
  ylab(label = "Copy number") +
  scale_x_discrete(labels = c("Second \n instar larva", "Third instar \n larva", 
                              "Fourth \n instar larva", "Fifth instar \n larva", "Pupa", "Adult"),
                   name = "Life stage") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)

#composite plot
plot_grid(Af, B, C, D, Ef, Ff, ncol = 3, nrow = 2, labels = genes)

########
# Going to make mini zoomed in graphs for genes A, E & F

#GENE A
#small graph with just fatbody and midgut
Adat_noH <- Adat %>% filter(tissue != "H")

As <- ggplot(Adat_noH, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 2) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), 
        legend.position = "none", axis.ticks.x.bottom = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        axis.line = element_blank()) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 8)) +
  scale_colour_manual(labels = c("Fatbody", "Midgut"), values = c("#BED985", "#5B503A"))

#Gene A big graph with small graph on top
Ab <- ggplot(Adat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none",
        axis.text.x = element_blank()) +
  scale_y_continuous(limits = c(0, 80000), breaks = seq(0, 80000, 10000), name = "Copy number") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols) +
  annotation_custom(ggplotGrob(As), xmin = 0.6, xmax = 3.1, ymin = 50000, ymax = 80000)

#GENE E
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
  scale_colour_manual(labels = c("Midgut", "Head"), values = c("#5B503A", "plum2"))

#Gene E big graph with small graph on top
Eb <- ggplot(Edat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none") +
  scale_y_continuous(limits = c(0, 70000), breaks = seq(0, 70000, 10000), name = "Copy number") +
  scale_x_discrete(labels = c("Second instar \n larva", "Third \n instar larva", 
                              "Fourth instar \n larva", "Fifth \n instar larva", "Pupa", "Adult"),
                   name = "Life stage") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols) +
  annotation_custom(ggplotGrob(Es), xmin = 0.6, xmax = 3.1, ymin = 42500, ymax = 70000)

#GENE F
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
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  scale_colour_manual(labels = c("Midgut", "Head"), values = c("#5B503A", "plum2"))

#big graph
Fb <- ggplot(Fdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none") +
  scale_y_continuous(limits = c(0, 30000), breaks = seq(0, 30000, 5000), name = "Copy number") +
  scale_x_discrete(labels = c("Second \n instar larva", "Third instar \n larva", 
                              "Fourth \n instar larva", "Fifth instar \n larva", "Pupa", "Adult"),
                   name = "Life stage") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols) +
  annotation_custom(ggplotGrob(Fs), xmin = 0.6, xmax = 3.1, ymin = 17500, ymax = 30000)

plot <- plot_grid(Ab, B, C, D, Eb, Fb, ncol = 3, nrow = 2, labels = genes)

#want one common y axis label
y.lab <- textGrob("Mean gene expression (FPKM)", rot = 90)

grid.arrange(arrangeGrob(plot, left = y.lab))


############ extra stuff I don't want to delete yet just in case #############

### try making one graph per tissue type with each gene a different colour
# genes: Msex2.07524, Msex2.15420, Msex2.14343, Msex2.04431, Msex2.01694, Msex2.10735
#first, separate data into each tissue type including only our 6 genes

head_dat <- mydat %>% 
  filter(gene_id == "Msex2.07524" | gene_id == "Msex2.15420" | gene_id == "Msex2.14343" |
           gene_id == "Msex2.04431" | gene_id == "Msex2.01694" | gene_id == "Msex2.10735",
         tissue == "H") %>% 
  group_by(gene_id, life_stage) %>% 
  summarise(mean_expression = mean(copynumber))

gut_dat <- mydat %>% 
  filter(gene_id == "Msex2.07524" | gene_id == "Msex2.15420" | gene_id == "Msex2.14343" |
           gene_id == "Msex2.04431" | gene_id == "Msex2.01694" | gene_id == "Msex2.10735",
         tissue == "G") %>% 
  group_by(gene_id, life_stage) %>% 
  summarise(mean_expression = mean(copynumber))

fat_dat <- mydat %>% 
  filter(gene_id == "Msex2.07524" | gene_id == "Msex2.15420" | gene_id == "Msex2.14343" |
           gene_id == "Msex2.04431" | gene_id == "Msex2.01694" | gene_id == "Msex2.10735",
         tissue == "F") %>% 
  group_by(gene_id, life_stage) %>% 
  summarise(mean_expression = mean(copynumber))

###not using these

#HEAD
ggplot(head_dat, aes(x = life_stage, y = mean_expression, colour = gene_id, group = gene_id)) +
  geom_point() +
  geom_line() +
  theme_classic()

#MIDGUT
ggplot(gut_dat, aes(x = life_stage, y = mean_expression, colour = gene_id, group = gene_id)) +
  geom_point() +
  geom_line() +
  theme_classic()

#FATBODY
ggplot(fat_dat, aes(x = life_stage, y = mean_expression, colour = gene_id, group = gene_id)) +
  geom_point() +
  geom_line() +
  theme_classic()


#GENE D
#small graph without midgut (G)
Ddat_noG <- Ddat %>% filter(tissue != "G")

ggplot(Ddat_noG, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), axis.text.x = element_blank(), 
        legend.position = "none", axis.ticks.x.bottom = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        axis.line = element_blank()) +
  #scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10)) +
  scale_colour_manual(labels = c("Fatbody", "Head"), values = c("#BED985", "plum2"))

#Gene D big graph with small graph on top
ggplot(Ddat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4) +
  geom_line() +
  theme_classic() +
  theme(axis.title = element_blank(), legend.position = "none") +
  scale_y_continuous(limits = c(0, 83000), breaks = seq(0, 80000, 20000), name = "Copy number") +
  scale_x_discrete(labels = c("Second \n instar larva", "Third instar \n larva", 
                              "Fourth \n instar larva", "Fifth instar \n larva", "Pupa", "Adult"),
                   name = "Life stage") +
  scale_colour_manual(labels = c("Fatbody", "Midgut", "Head"), values = mycols)


















