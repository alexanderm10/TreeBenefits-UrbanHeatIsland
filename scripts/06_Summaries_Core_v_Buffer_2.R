# # Script to go through the pixel-level data and calculate the core vs buffer stats
library(ggplot2); library(RColorBrewer); library(cowplot)

###########################################
# Establish file paths etc ----
###########################################
path.google <- file.path("G:/My Drive/northstar2023/1km_modis")
path.cities <- file.path("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/processed_cities")
path.save <- file.path("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/")

path.figs <- file.path(path.google, "figures_exploratory")
dir.create(path.figs, recursive=T, showWarnings=F)

grad.lst <- c("#2c7bb6", "#abd9e9", "#f7f7f7", "#fdae61", "#d7191c") # ends with red
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green


biome.pall.all = c("Taiga"= "#2c5c74", 
                   "Tundra"="#6d8e9d",
                   "Temperate Broadleaf Forest" = "#7f310f",
                   "Temperate Conifer Forest" = "#4d1e10",
                   "Temperate Grassland/Savanna" = "#b09c41",
                   "Montane Grassland/Savanna" = "#a0b8c7",
                   "Mediterranean" = "#bf772e",
                   "Desert" = "#c89948",
                   "Flooded Grassland/Savanna" = "#e0dfa1",
                   "Tropical Grassland/Savanna" = "#a6b39e",
                   "Tropical Dry Broadleaf Forest" = "#7a9c64",
                   "Tropical Conifer Forest" = "#488458",
                   "Tropical Moist Broadleaf Forest"= "#266240",
                   "Mangroves" = "#9c8c94")

world <- map_data("world")
# ##########################################


# ##########################################
# Read in base datasets ----
# ##########################################
# Regional Sumary Stuff ----
cityAll.stats <- read.csv(file.path(path.cities, "city_stats_all2.csv"))
summary(cityAll.stats[!is.na(cityAll.stats$model.R2adj),])

cityAll.stats$biome <- gsub("flodded", "flooded", cityAll.stats$biome) # Whoops, had a typo!  Not going to reprocess now.
summary(as.factor(cityAll.stats$biome))

cityAll.stats$biomeName <- car::recode(cityAll.stats$biome, 
                                       "'boreal forest/taiga'='Taiga';
                                       'tundra'='Tundra';
                                       'montane grassland/savanna'='Montane Grassland/Savanna';
                                       'temperate broadleaf/mixed forest'='Temperate Broadleaf Forest';
                                       'temperate coniferous forest'='Temperate Conifer Forest';
                                       'temperate grassland/savanna'='Temperate Grassland/Savanna';
                                       'mediterranean'='Mediterranean';
                                       'desert/xeric shrublands'='Desert';
                                       'flooded grassland/savanna'='Flooded Grassland/Savanna';
                                       'tropical grassland/savannas'='Tropical Grassland/Savanna';
                                       'tropical dry broadleaf forest'='Tropical Dry Broadleaf Forest';
                                       'tropical coniferous forest'='Tropical Conifer Forest';
                                       'tropical moist broadleaf forest'='Tropical Moist Broadleaf Forest';
                                       'mangroves'='Mangroves'")

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]

fema.order <- aggregate(LST.mean ~ fema.region, data=cityAll.stats, FUN=mean)
fema.order <- fema.order[order(fema.order$LST.mean),]

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)
cityAll.stats$fema.region <- factor(cityAll.stats$fema.region, levels=fema.order$fema.region)
# ##########################################


# ##########################################
# Exploring Core vs. Buffer data -----
# ##########################################
CityBuffStats <- read.csv(file.path(path.cities, "city_stats_core-buffer.csv"))
summary(CityBuffStats)

# need
# library(tidyr)
# spread(dat1, key = numbers, value = value)
valMean <- reshape(CityBuffStats[,c("ISOURBID", "factor", "value.mean.core")], idvar="ISOURBID", timevar="factor", direction="wide")
valMean2 <- reshape(CityBuffStats[,c("ISOURBID", "factor", "value.mean.buffer")], idvar="ISOURBID", timevar="factor", direction="wide")
head(valMean2)
valMean[,names(valMean2)[2:4]] <- valMean2[,2:4]

# Merge in metadata
valMean <- merge(valMean, cityAll.stats[,c("ISOURBID", "NAME",  "LATITUDE", "LONGITUDE", "ES00POP", "biomeName", "model.tree.slope", "model.veg.slope", "fema.region")], all=T)
summary(valMean)

# Looking at the difference in tree cover between the metro core & buffer
valMean$LST.diff <- valMean$value.mean.core.LST-valMean$value.mean.buffer.LST

valMean$Tree.diff <- valMean$value.mean.core.tree-valMean$value.mean.buffer.tree
valMean$Tree.diff.degC <- valMean$Tree.diff*valMean$model.tree.slope

valMean$Veg.diff <- valMean$`value.mean.core.other veg`-valMean$`value.mean.buffer.other veg`
valMean$Veg.diff.degC <- valMean$Veg.diff*valMean$model.veg.slope

summary(valMean)
mean(valMean$Tree.diff); sd(valMean$Tree.diff)
mean(valMean$Tree.diff.degC); sd(valMean$Tree.diff.degC)

# Proportion of the LST diff attributable to Tree Cover ----
valMean$percentTreeCool <- valMean$Tree.diff.degC/valMean$LST.diff

mean(valMean$percentTreeCool); sd(valMean$percentTreeCool)

# some general plots of valMean by fema region
library(ggplot2)
ggplot(data=valMean) +
  geom_histogram(aes(x=Tree.diff, fill=fema.region))

#### Exploring statements from our outline ----
# https://docs.google.com/document/d/1jzRpgNR8pWvmhS0bCULx4iAG5QjX5b0GcW8YPAs01VI/edit?usp=sharing

# Of the cities with showing UHI effects, XX% have decreased tree canopy cover, with a mean differences  XX% between the core metropolitan area and surrounding landscape.
hist(valMean$Tree.diff[valMean$LST.diff>0])
mean(valMean$Tree.diff[valMean$LST.diff>0]); sd(valMean$Tree.diff[valMean$LST.diff>0]) 
quantile(valMean$Tree.diff[valMean$LST.diff>0], c(0.05, 0.5, 0.95))

hist(valMean$Tree.diff.degC[valMean$LST.diff>0])
mean(valMean$Tree.diff.degC[valMean$LST.diff>0]); sd(valMean$Tree.diff.degC[valMean$LST.diff>0]) 
t.test(valMean$Tree.diff[valMean$LST.diff>0])
t.test(valMean$Tree.diff.degC[valMean$LST.diff>0])

summary(valMean$Tree.diff.degC[valMean$LST.diff>0]/valMean$LST.diff[valMean$LST.diff>0])
mean(valMean$Tree.diff.degC[valMean$LST.diff>0]/valMean$LST.diff[valMean$LST.diff>0]); sd(valMean$Tree.diff.degC[valMean$LST.diff>0]/valMean$LST.diff[valMean$LST.diff>0])

mean(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0]/valMean$LST.diff[valMean$LST.diff>0 & valMean$Tree.diff<0])

length(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0])
mean(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0]); sd(valMean$Tree.diff.degC[valMean$LST.diff>0 & valMean$Tree.diff<0])
# t.test(valMean$value.mean.core.tree[valMean$LST.diff>0], valMean$value.mean.buffer.tree[valMean$LST.diff>0], paired=T)

hist(valMean$Veg.diff[valMean$LST.diff>0])
mean(valMean$Veg.diff[valMean$LST.diff>0]); sd(valMean$Veg.diff[valMean$LST.diff>0]) 
mean(valMean$Veg.diff.degC[valMean$LST.diff>0]); sd(valMean$Veg.diff.degC[valMean$LST.diff>0]) 
t.test(valMean$Veg.diff[valMean$LST.diff>0])


nrow(valMean[valMean$LST.diff>0 & valMean$Veg.diff<0,])/nrow(valMean[valMean$LST.diff>0,])
nrow(valMean[valMean$LST.diff>0 & valMean$Veg.diff>0,])/nrow(valMean[valMean$LST.diff>0,])

mean(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0]); sd(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0])

# Conversely -- looking at the cities with negative LST diff
hist(valMean$Tree.diff[valMean$LST.diff<0])
mean(valMean$Tree.diff[valMean$LST.diff<0]); sd(valMean$Tree.diff[valMean$LST.diff<0]) 
quantile(valMean$Tree.diff[valMean$LST.diff<0], c(0.025, 0.5, 0.975))
t.test(valMean$Tree.diff[valMean$LST.diff<0])

nrow(valMean[valMean$LST.diff<0 & valMean$Tree.diff>0,])/nrow(valMean[valMean$LST.diff<0,])

mean(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0]); sd(valMean$Tree.diff[valMean$LST.diff>0 & valMean$Tree.diff<0])




### # Looking at temporal trends in the urban core ----
valTrendCore <- reshape(CityBuffStats[,c("ISOURBID", "factor", "trend.mean.core")], idvar="ISOURBID", timevar="factor", direction="wide")
valTrendCore <- merge(valTrendCore, cityAll.stats[,c("ISOURBID", "NAME",  "LATITUDE", "LONGITUDE", "ES00POP", "biomeName", "model.tree.slope", "model.veg.slope", "fema.region", "state")], all=T)

# Converting the tree trend to deg. C per year ----
valTrendCore$LST.change.Tree <- valTrendCore$trend.mean.core.tree*valTrendCore$model.tree.slope
valTrendCore$LST.change.NoTree <- valTrendCore$trend.mean.core.LST - valTrendCore$LST.change.Tree
summary(valTrendCore)

summary(valTrendCore[ ,c("trend.mean.core.LST", "LST.change.Tree", "LST.change.NoTree")]*20)
summary(valTrendCore$LST.change.NoTree/valTrendCore$LST.change.Tree)

# hist(valTrendCore$LST.change.Tree/valTrendCore$LST.change.NoTree, xlim=-)

# calculating the mean temperature without trees
valMean$no.tree.LST <-  valMean$value.mean.core.LST - (valMean$model.tree.slope*valMean$value.mean.core.tree)
valMean$no.tree.LST.diff <- valMean$no.tree.LST - valMean$value.mean.core.LST
summary(valMean$no.tree.LST.diff)

ggplot(data=valMean) +
  geom_point(aes(y=no.tree.LST.diff, x = value.mean.core.tree))

# plotting to check that it would go up
ggplot(data=valMean) +
  geom_violin(aes(x=1, y= value.mean.core.LST)) +
  stat_summary(aes(x=1, y= value.mean.core.LST), fun.y=mean, geom="point", shape=23, size = 2) +
  geom_violin(aes(x= 2, y = no.tree.LST), col="red") +
  stat_summary(aes(x= 2, y = no.tree.LST), fun.y=mean, geom="point", shape=23, size = 2, col="red") +
  geom_hline(yintercept=mean(valMean$value.mean.core.LST)) +
  geom_hline(yintercept=mean(valMean$no.tree.LST), col="red")



# saving some of the output to be used in the query tool
write.csv(valTrendCore, file.path(path.save, "core_trends.csv"), row.names=F)

# removing some columns before saving for the portal
valMean3 <- valMean[,!names(valMean) %in% c("NAME", "LATITUDE", "LONGITUDE", "ES00POP")]

write.csv(valMean, file.path(path.save, "core_v_buffer.csv"), row.names=F)
