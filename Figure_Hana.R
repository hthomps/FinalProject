#Creating a figure showing change in gene expression throughout development in different tissues
#Tissues we are using are the head (H), midgut (G), and fatbody (F)

library(ggplot2)
library(dplyr)
library(cowplot)

load("MeltedData.RData")

mydat <- dat2
mydat$tissue <- as.factor(mydat$tissue)
mydat$life_stage <- as.factor(mydat$life_stage)
#reorder life stage so it is in chronological order
mydat$life_stage <- factor(mydat$life_stage, 
                           levels = c("E", "L1", "L2", "L3", "L4", "L5", "P", "A"))

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

#with facet grid
A <- ggplot(Adat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  facet_grid(scales = "free", rows = vars(tissue)) +
  theme_classic() +
  ylab(label = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")


#now gene Msex2.15420 (gene B)
Bdat <- mydat %>% 
  filter(gene_id == "Msex2.15420", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

B <- ggplot(Bdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 300000), breaks = seq(0, 300000, 100000), name = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")

#now gene Msex2.14343 (gene C)
Cdat <- mydat %>% 
  filter(gene_id == "Msex2.14343", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

C <- ggplot(Cdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 50000), breaks = seq(0, 50000, 10000), name = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")

#now gene Msex2.04431 (gene D)
Ddat <- mydat %>% 
  filter(gene_id == "Msex2.04431", tissue == "H" | tissue == "G" | tissue == "F") %>% 
  group_by(tissue, life_stage) %>% 
  summarise(mean_expression = mean(copynumber)) #use mean expression values because some tissue/life stage combos have sub-life stages

D <- ggplot(Ddat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  theme_classic() +
  scale_y_continuous(limits = c(0, 83000), breaks = seq(0, 80000, 20000), name = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")

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
E <- ggplot(Edat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  facet_grid(scales = "free", rows = vars(tissue)) +
  theme_classic() +
  ylab(label = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")


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
FF <- ggplot(Fdat, aes(x = life_stage, y = mean_expression, colour = tissue, group = tissue)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_line() +
  facet_grid(scales = "free", rows = vars(tissue)) +
  theme_classic() +
  ylab(label = "Copy number") +
  scale_x_discrete(labels = c("Second instar larva", "Third instar larva", 
                              "Fourth instar larva", "Fifth instar larva", "Pupa", "Adult"),
                   name = "Life stage")

#composite plot
plot_grid(A, B, C, D, E, FF, ncol = 3, nrow = 2)

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





















