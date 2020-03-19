# Data wrangling for Biol812 final project 
# done by Regan Cross, Hayden Wainwright, Marco Lee, and Hana Thompson
# March 19, 2020


# load in transcriptome data
dat <- read.csv("MothData.csv")
# right now each column is a transcriptome and each row is a gene, 
# with expression levels in the data cells
# the transcriptome names include their tissue type and life stage

# load packages
library(tidyverse)
library(reshape2)

# we need to melt the dataset to each row a unique gene - transcriptome combination
# we'll use the melt function in the reshape2 package
dat2 <- dat %>% melt(id.vars = c("gene_id", "gene_description"), 
                     measure.vars = c("H.L2.D1", "H.L3.D1", "H.L4.12h", "H.L4.Late", 
                                      "H.L5.12hS", "H.L5.D2S", "H.L5.preWS", "H.P.Late", 
                                      "H.A.D1", "H.A.D3", "H.A.D7", "F.L4.Late", "F.L5.D1", 
                                      "F.L5.preW", "F.L5.W", "F.P.D13S", "F.P.D1518", "F.A.D13", 
                                      "F.A.D79", "W.E.3hS", "W.E.LateS", "W.L1.D1S", "W.L2.D1S", 
                                      "W.L3.D1S", "G.L2.unk", "G.L3.LateS", "G.L4.0h", "G.L4.12hS", "G.L4.LateS", 
                                      "G.L5.13hS", "G.L5.D1S", "G.L5.preWS", "G.L5.WS", "G.L5.W", 
                                      "G.P.D1", "G.P.D1518", "G.A.D35", "MT.L5.preW", "MT.A.D1", 
                                      "MT.A.D3", "M.L4.Late", "M.L5.12h", "M.L5.12hS", "M.L5.preW", 
                                      "M.L5.preWS","M.L5.W", "M.L5.WS",  "T.P.D3",  "T.P.D1518", 
                                      "T.A.D13", "O.P.D1518", "O.A.D1", "H.A.D1F1S", "H.A.D1F2S", 
                                      "H.A.D1F3S", "H.A.D1F4S", "H.A.D1M1S", "H.A.D1M2S", "H.A.D1M3S", 
                                      "H.A.D1M4S", "An.L5.1S", "An.L5.2S", "An.L5.3S", "An.A.F1S", "An.A.F2S",  "An.A.F3S", "An.A.MS"))

# let's rename the "variable" and "value" columns to something more informative
colnames(dat2)[3] <- "transcriptome"
colnames(dat2)[4] <- "copynumber"

# let's pull apart the transcriptome names into the tissue type (which is the code before
# the first . ), life stage (the code between .'s), and whatever is on the end
# we use the separate command in dplyr
dat2 <- dat2 %>% separate(transcriptome, into = c("tissue", "life_stage", "note"), 
                          remove = FALSE, extra = "drop")
# remove = FALSE so it keeps the tracnscriptome column

# let's just grab the data for the life stages (adult and 5th instar larva) 
# and tissue types (fatbody, head, and midgut) that we want
subdat <- dat2 %>% filter(life_stage == "L5" | life_stage == "A") %>% filter(tissue == "H" | tissue == "G" | tissue == "F")

# change them to factors for easier graphing later on
subdat$life_stage <- as.factor(subdat$life_stage)
subdat$tissue <- as.factor(subdat$tissue)

# now lets make separate dataesets for each tissue type, and get the mean expression
# level for each lifestage 
headdat <- subdat %>% filter(tissue == "H") %>% group_by(gene_id, life_stage) %>% summarise(mean = round(mean(copynumber), digits = 3))
gutdat <- subdat %>% filter(tissue == "G") %>% group_by(gene_id, life_stage) %>% summarise(mean = round(mean(copynumber), digits = 3))
fatdat <- subdat %>% filter(tissue == "F") %>% group_by(gene_id, life_stage) %>% summarise(mean = round(mean(copynumber), digits = 3))

# now we cast it back out so there is a column for adult (A) and a column for larva (L5)
# and then take the difference between those two columns 
headdat2 <- headdat %>% dcast(gene_id ~ life_stage, mean) %>% mutate(diff = abs(A - L5))
# now if we click the "diff" column in the data frame (opened through the environment)
# it'll order them so we can see the max diff's 

# the head genes with the greatest difference between adult and L5 are: 
# Msex2.07524, Msex2.15420, Msex2.14343

gutdat2 <- gutdat %>% dcast(gene_id ~ life_stage, mean) %>% mutate(diff = abs(A - L5))
# the midgut genes with the greatest difference between adult and L5 are: 
# Msex2.04431, Msex2.15420, Msex2.14343

fatdat2 <- fatdat %>% dcast(gene_id ~ life_stage, mean) %>% mutate(diff = abs(A - L5))
# the fatbody genes with the greatest difference between adult and L5 are: 
# Msex2.15420, Msex2.01694, Msex2.10735

