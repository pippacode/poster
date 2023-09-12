# Trying NMDS ----
library(tidyverse)
library(vegan)

raw <- read.csv("C:/Users/prill/Documents/a_uni/4th Year/semester_1/Field Course and Stats/edge_effects_proj/raw.csv")
# quad vs plant div
raw_plant <- raw[-14,]
raw_plant$quad <- as.factor(raw_plant$quad)
plant_div <- raw_plant[,c(14:26,29:44)]

as.data.frame(colnames(raw_plant))  # see column no.
plant_div[is.na(plant_div)] <- 0

# plant_div %>%
#   summarise(across(everything(), range))
# test <- plant_div %>%
#   rowwise() %>%
#   mutate(sum=sum(across(everything())))
# which(test$sum == 0)

#plant_div <- plant_div[-14,] #VILLIAN = 2A(-15)

#NMDS
set.seed(123)
nmds <- metaMDS(plant_div, distance = "bray", k = 2, trymax=300)
nmds

plot(nmds)
stressplot(nmds)



# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c("#6E4318", "orange", "#F0E442", "#56B4E4", "#009E73")

names(Colours) <- c("-30", "-15", "0", "15", "30")

data.scores <- as.data.frame(scores(nmds)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- as.factor(raw_plant$quad)  #  add the group variable created earlier
head(data.scores)

hull.data <- data.frame()
for(i in 1:length(unique(raw_plant$quad))){
  temp <- data.scores[data.scores$grp == unique(raw_plant$quad)[i], ][chull(data.scores[data.scores$grp ==
                                                                                          unique(raw_plant$quad)[i], c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, temp)
}

(NMDS_plot <- ggplot() +
    geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.60) + # add the convex hulls
    geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
    scale_colour_manual("Position", values=Colours) +
    scale_fill_manual("Position", values=Colours) +
    ggtitle("plants") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_classic())



# INSECT NMDS (Ants & Hymenoptera Separate)
raw_insect <- raw
raw_insect$quad <- as.factor(raw_insect$quad)
as.data.frame(colnames(raw_insect))  # see column no.
insect_div <- raw_insect[,c(47:63)]

insect_div[is.na(insect_div)] <- 0 # Replace NA values with 0


#nmds2
set.seed(123)
nmds2 <- metaMDS(insect_div, distance = "bray", k = 2, trymax=300)
nmds2

plot(nmds2)
stressplot(nmds2)



# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c("#6E4318", "orange", "#F0E442", "#56B4E4", "#009E73")
names(Colours) <- c("-30", "-15", "0", "15", "30")

data.scores_2 <- as.data.frame(scores(nmds2)$sites)  # Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores_2$site <- rownames(data.scores_2)  # create a column of site names, from the rownames of data.scores
data.scores_2$grp <- as.factor(raw_insect$quad)  #  add the group variable created earlier
head(data.scores_2)

hull.data_2 <- data.frame()
for(i in 1:length(unique(raw_insect$quad))){
  temp_2 <- data.scores_2[data.scores_2$grp == unique(raw_insect$quad)[i], ][chull(data.scores_2[data.scores_2$grp ==
                                                                                                   unique(raw_insect$quad)[i], c("NMDS1", "NMDS2")]), ]
  hull.data_2 <- rbind(hull.data_2, temp_2)
}

(NMDS_plot_2 <- ggplot() +
    geom_polygon(data=hull.data_2,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.60) + # add the convex hulls
    geom_point(data=data.scores_2,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
    scale_colour_manual("Position", values=Colours) +
    scale_fill_manual("Position", values=Colours) +
    ggtitle("inverts") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_classic())

NMDS_plot
NMDS_plot_2







# INSECT NMDS (Antless)
raw_insect_antless <- raw
raw_insect_antless$quad <- as.factor(raw_insect_antless$quad)
as.data.frame(colnames(raw_insect_antless))  # see column no.
insect_div_antless <- raw_insect_antless[,c(47:49,51:63)]

insect_div_antless[is.na(insect_div_antless)] <- 0 # Replace NA values with 0


#nmds3
set.seed(123)
nmds3 <- metaMDS(insect_div_antless, distance = "bray", k = 2, trymax=300)
nmds3

plot(nmds3)
stressplot(nmds3)



# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c("#6E4318", "orange", "#F0E442", "#56B4E4", "#009E73")
names(Colours) <- c("-30", "-15", "0", "15", "30")

data.scores_3 <- as.data.frame(scores(nmds3)$sites)  # Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores_3$site <- rownames(data.scores_3)  # create a column of site names, from the rownames of data.scores
data.scores_3$grp <- as.factor(raw_insect_antless$quad)  #  add the group variable created earlier
head(data.scores_3)

hull.data_3 <- data.frame()
for(i in 1:length(unique(raw_insect_antless$quad))){
  temp_3 <- data.scores_3[data.scores_3$grp == unique(raw_insect_antless$quad)[i], ][chull(data.scores_3[data.scores_3$grp ==
                                                                                                           unique(raw_insect_antless$quad)[i], c("NMDS1", "NMDS2")]), ]
  hull.data_3 <- rbind(hull.data_3, temp_3)
}

(NMDS_plot_3 <- ggplot() +
    geom_polygon(data=hull.data_3,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.60) + # add the convex hulls
    geom_point(data=data.scores_3,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
    scale_colour_manual(values=Colours) +
    scale_fill_manual(values=Colours) +
    ggtitle("inverts") +
    theme(plot.title = element_text(hjust = 0.5)))

NMDS_plot
NMDS_plot_2
NMDS_plot_3

#nmds to visualize the differences in community compostion with distance along edge








# NDMS INSECT (combined)

raw_insect_comb <- raw
raw_insect_comb$quad <- as.factor(raw_insect_comb$quad)
as.data.frame(colnames(raw_insect_comb))  # see column no.
insect_div_comb <- raw_insect_comb[,c(47:62)]

insect_div_comb[is.na(insect_div_comb)] <- 0 # Replace NA values with 0

insect_div_comb <- insect_div_comb %>%
  mutate(hymenoptera = hymenoptera + Ants) %>%
  select(-Ants)

#nmds4
set.seed(123)
nmds4 <- metaMDS(insect_div_comb, distance = "bray", k = 2, trymax=300)
nmds4

plot(nmds4)
stressplot(nmds4)



# ggplot version - from https://chrischizinski.github.io/rstats/vegan-ggplot2/
Colours <- c("#6E4318", "orange", "#F0E442", "#56B4E4", "#009E73")
names(Colours) <- c("-30", "-15", "0", "15", "30")

data.scores_4 <- as.data.frame(scores(nmds4)$sites)  # Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores_4$site <- rownames(data.scores_4)  # create a column of site names, from the rownames of data.scores
data.scores_4$grp <- as.factor(raw_insect_comb$quad)  #  add the group variable created earlier
head(data.scores_4)

hull.data_4 <- data.frame()
for(i in 1:length(unique(raw_insect_comb$quad))){
  temp_4 <- data.scores_4[data.scores_4$grp == unique(raw_insect_comb$quad)[i], ][chull(data.scores_4[data.scores_4$grp ==
                                                                                                        unique(raw_insect_comb$quad)[i], c("NMDS1", "NMDS2")]), ]
  hull.data_4 <- rbind(hull.data_4, temp_4)
}

(NMDS_plot_4 <- ggplot() +
    geom_polygon(data=hull.data_4,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.60) + # add the convex hulls
    geom_point(data=data.scores_4,aes(x=NMDS1,y=NMDS2,colour=grp),size=3) + # add the point markers
    scale_colour_manual("Position", values=Colours) +
    scale_fill_manual("Position", values=Colours) +
    ggtitle("inverts") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_classic())


NMDS_plot # Plant Diversity
NMDS_plot_2 # Ants separate of Hymenoptera
NMDS_plot_3 # No Ants
NMDS_plot_4 # Ants with Hymenoptera
