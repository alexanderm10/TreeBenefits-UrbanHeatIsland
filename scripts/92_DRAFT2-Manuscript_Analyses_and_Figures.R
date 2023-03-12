###########################################
# Script Description & Outline ----
###########################################
# Purpose: Stats & figures corresponding to current manuscript outline; cleaned up and organized

# Manuscript Outline
# ---------
# 1. Cooling Contribution of trees & non-tree vegetation
# ---------
# ---------
# 2. Vegetation cover targets to fully offset UHIs and warming trends
# ---------
# ---------



###########################################

library(ggplot2); library(RColorBrewer); library(cowplot)


###########################################
# Establish file paths etc ----
###########################################
user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Library/CloudStorage", user.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v2")
path.cities <- file.path(path.google, "data_processed_final")

path.figs <- file.path(path.google, "figures_manuscript")
dir.create(path.figs, recursive=T, showWarnings=F)

grad.lst <- c("#2c7bb6", "#abd9e9", "#f7f7f7", "#fdae61", "#d7191c") # ends with red
# grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.treeDiff <- c("#8c510a", "#d8b365", "#f6e8c3", "#f5f5f5", "#c7eae5", "#5ab4ac", "#01665e") # ends with teal

grad.tree <- c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679", "#31a354", "#006837") # ends with green

# grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.otherDiff <- c("#c51b7d", "#e0a3c0", "#fde0ef", "#f7f7f7", "#e6f5d0", "#a1d76a", "#4d9221") # Green
grad.other <- c("#f0f9e8", "#ccebc5", "#a8ddb5", "#7bccc4", "#43a2ca", "#0868ac") # ends with Blue

grad.modfit <- c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")

# grad.lstHot <- c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026") # ends with red
# grad.lstHot <- c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026") # ends with red
grad.lstHot <- c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026") # ends with red


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
##########################################

# ##########################################
# Read in base datasets ----
# ##########################################
# Regional Sumary Stuff ----
cityAll.stats <- read.csv(file.path(path.cities, "..", "city_stats_all.csv"))
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

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)
summary(cityAll.stats)

CityBuffStats <- read.csv(file.path(path.cities, "city_stats_core-buffer.csv"))
CityBuffStats$factor <- factor(CityBuffStats$factor, levels=c("LST", "tree", "other veg"))
CityBuffStats <- merge(CityBuffStats, cityAll.stats[,c("ISOURBID", "NAME", "LONGITUDE", "LATITUDE", "biomeName", "ES00POP")], all.x=T)
summary(CityBuffStats)


CityBuffStats[CityBuffStats$value.mean.core==0,] # This is for tree cover, so VERY very low.
summary(CityBuffStats[CityBuffStats$value.mean.core<1,]) # This is for tree cover, so VERY very low.
# ##########################################

# ##########################################
# Step 1: Figure out what our criteria for cities to show is ----
#  -- enough data to do the temporal trend analysis (removes 96 cities)
#  -- R2 cutoff?
#  -- Max Tree & veg cover for any pixel >10% (allow robust parameterization)
# ##########################################
citiesUHI <- CityBuffStats$ISOURBID[CityBuffStats$factor=="LST" & CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.p<0.01]
length(citiesUHI)

nrow(cityAll.stats)

length(which(!cityAll.stats$ISOURBID %in% citiesUHI))
length(which(is.na(cityAll.stats$trend.LST.slope)))
length(which(cityAll.stats$n.pixels<1000))
length(which(cityAll.stats$biome.prop<0.75))
length(which(cityAll.stats$tree.sd<1))
length(which(cityAll.stats$veg.sd<1))

citiesUse <- !is.na(cityAll.stats$trend.LST.slope) & 
  cityAll.stats$n.pixels>=1000 & 
  cityAll.stats$biome.prop>=0.75 &
  # cityAll.stats$tree.max>10 & cityAll.stats$veg.max>10 &
  cityAll.stats$tree.sd >= 1 & cityAll.stats$veg.sd >= 1 

cityStatsAnaly <- cityAll.stats[which(citiesUse & cityAll.stats$ISOURBID %in% citiesUHI),]
cityBuffAnaly <- CityBuffStats[CityBuffStats$ISOURBID %in% cityStatsAnaly$ISOURBID,]
summary(cityAll.stats)
summary(cityStatsAnaly)
summary(cityStatsAnaly$biomeName)

nrow(cityStatsAnaly)


cityTree <- cityBuffAnaly[cityBuffAnaly$factor=="tree", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityTree) <- gsub("mean", "tree", names(cityTree))
names(cityTree)[which(names(cityTree)=="trend.p.core")] <- "trend.tree.p.core"

cityOther <- cityBuffAnaly[cityBuffAnaly$factor=="other veg", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityOther) <- gsub("mean", "other", names(cityOther))
names(cityOther)[which(names(cityOther)=="trend.p.core")] <- "trend.other.p.core"

cityVeg <- merge(cityTree, cityOther, all=T)
summary(cityVeg)

cityLST <- cityBuffAnaly[cityBuffAnaly$factor=="LST", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityLST) <- gsub("mean", "LST", names(cityLST))
names(cityLST)[which(names(cityLST)=="trend.p.core")] <- "trend.LST.p.core"

summary(cityStatsAnaly[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "model.R2adj", "model.tree.slope", "model.tree.p", "model.veg.slope", "model.veg.p")])

StatsCombined <- merge(cityStatsAnaly[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "model.R2adj", "model.tree.slope", "model.tree.p", "model.veg.slope", "model.veg.p")],
                       cityVeg, all=T)
StatsCombined <- merge(StatsCombined, cityLST, all=T)
summary(StatsCombined)


# SUPPLEMENTAL TABLE ----
# Global + Biome:
# -- Cities Considered
# -- Cities exlcuded for each criteria
# ##########################################

# ##########################################
# 1. Cooling contribution of trees and non-tree vegetation to cities (4-5 paragraphs)
# Key Result/Messages: 
#   - Trees cool at 5.5x the rate of other vegetation
#   - On average, 37% of the urban heat island effect is attributable to lower in tree cover in the metro core than reference region.
#   - Without trees, the UHI would be double what it currently is
#   - Although trees cool more than other veg, the relative dominance of other veg mean that they have similar cooling effects on cities.
#
# Key Figure/Table Components (needs to show):
#   - Fig 1: Map of UHI & Urban Veg Covers; latitude bands for UHI & Tree/Veg slopes
#       - Supplement: Break down by biome
#       - Purpose: Comparison of tree vs non-tree slopes across geography/biomes
#   - Fig 2: Total cooling contribution of veg by biome; + warming because of Veg cover difference with buffer
#       - Purpose: Cooling contribution of current tree/other veg cover & deficit → i know the histograms are hard, but maybe have the deficit (core minus reference) negative and show it counteracting the current contribution?  Or have it above showing what it would be if we met that target?
#       - Comparison of tree/non-tree cooling as a function of percent cover?? ← this has been hard to show previously, but maybe it’ll work if we focus on tree vs. non-tree & show biomes as individual panels?
# ##########################################



breaksUHI <- c(0, round(quantile(StatsCombined$value.LST.diff, seq(0.2, 1, length.out=length(grad.lstHot))), 1))
breaksTree <- c(0, round(quantile(StatsCombined$value.tree.core, seq(0.2, 1, length.out=length(grad.tree))), 0))
breaksOther <- c(0, round(quantile(StatsCombined$value.other.core, seq(0.2, 1, length.out=length(grad.other))), 0))


mapLST <- ggplot(data=StatsCombined[,]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cut(value.LST.diff, breaksUHI)), size=0.5) +
  # scale_color_gradientn(name="LST Diff.\n(deg. C)", colors=grad.lstHot) +
  scale_color_manual(name="LST Diff.\n(deg. C)", values=grad.lstHot) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.75, 0.5, "lines"))

mapTree <- ggplot(data=StatsCombined[,]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cut(value.tree.core, breaksTree)), size=0.5) +
  # scale_color_gradientn(name="LST Diff.\n(deg. C)", colors=grad.lstHot) +
  scale_color_manual(name="Tree Cover\n(%)", values=grad.tree) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.75, 0.5, "lines"))

mapOther <- ggplot(data=StatsCombined[,]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cut(value.other.core, breaksOther)), size=0.5) +
  # scale_color_gradientn(name="LST Diff.\n(deg. C)", colors=grad.lstHot) +
  scale_color_manual(name="Other Veg.\nCover (%)", values=grad.other) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(), 
        plot.margin=margin(0.5,0.5, 0.75, 0.5, "lines"))

UHILat <- ggplot(data=StatsCombined[,],) +
  coord_flip(xlim=c(-60,75), expand=F) +
  # coord_cartesian(, ylim=c(-65,80))
  # geom_point(aes(x=LATITUDE, y=exp(model.tree.slope), color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  stat_smooth(aes(x=LATITUDE, y=value.LST.diff), color=rev(grad.lstHot)[1], fill=rev(grad.lstHot)[2], alpha=0.5) +
  scale_y_continuous(limits=c(-2*0.08/0.065, 2)) +
  # geom_bar(aes(x=LATITUDE, y=value.LST.diff), fill=rev(grad.lstHot)[2], stat="summary", fun.y="mean") +
  # geom_errorbar(aes(x=LATITUDE, y=value.LST.diff), fill=rev(grad.lstHot)[2], stat="summary", fun.y="sd") +
  # scale_x_binned(limits=c(-65, 80), breaks=seq(-65, 80, by=5)) +
  labs(y="UHI\n(deg C)", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.title.x=element_text(color="black", face="bold"),
        plot.margin=margin(4.5,2, 0.0, 0.5, "lines"))

TreeSlopeLat <-ggplot(data=StatsCombined,) +
  coord_flip(xlim=c(-60,75), expand=F) +
  # coord_cartesian(, ylim=c(-65,80))
  # geom_point(aes(x=LATITUDE, y=exp(model.tree.slope), color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  stat_smooth(aes(x=LATITUDE, y=model.tree.slope), color=rev(grad.tree)[1], fill=rev(grad.tree)[2], alpha=0.5) +
  # geom_bar(aes(x=LATITUDE, y=model.tree.slope), fill=rev(grad.tree)[2], stat="summary", fun.y="mean") +
  # geom_errorbar(aes(x=LATITUDE, y=model.tree.slope), fill=rev(grad.tree)[2], stat="summary", fun.y="sd") +
  # geom_boxplot(aes(x=LATITUDE, y=model.tree.slope), fill=rev(grad.tree)[2]) +
  # scale_x_binned(limits=c(-65, 80), breaks=seq(-65, 80, by=10)) +
  scale_y_continuous(limits=c(-0.08, 0.065)) +
  labs(y="Cooling Potential\n(deg C / % cover)", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.title.x=element_text(color="black", face="bold"),
        plot.margin=margin(4.5,2, 0.0, 0.5, "lines"))

TreeSlopeLat2 <- ggplot(data=StatsCombined,) +
  coord_flip() +
  # coord_cartesian(, ylim=c(-65,80))
  # geom_point(aes(x=LATITUDE, y=exp(model.tree.slope), color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  # stat_smooth(aes(x=LATITUDE, y=model.tree.slope), color=rev(grad.tree)[1], fill=rev(grad.tree)[2], alpha=0.5) +
  # geom_bar(aes(x=LATITUDE, y=model.tree.slope), fill=rev(grad.tree)[2], stat="summary", fun.y="mean") +
  # geom_errorbar(aes(x=LATITUDE, y=model.tree.slope), fill=rev(grad.tree)[2], stat="summary", fun.y="sd") +
  geom_boxplot(aes(x=cut(LATITUDE, breaks=seq(-65, 80, by=10)), y=model.tree.slope), fill=rev(grad.tree)[2]) +
  scale_x_discrete(drop=F) +
  # scale_y_continuous(limits=c(-0.08, 0.065)) +
  labs(y="Cooling Potential\n(deg C / % cover)", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.title.x=element_text(color="black", face="bold"),
        plot.margin=margin(4.5,2, 0.0, 0.5, "lines"))

OtherSlopeLat <- ggplot(data=StatsCombined,) +
  coord_flip(xlim=c(-60,75), expand=F) +
  # coord_cartesian(, ylim=c(-65,80))
  # geom_point(aes(x=LATITUDE, y=exp(model.tree.slope), color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  stat_smooth(aes(x=LATITUDE, y=model.veg.slope), color=rev(grad.other)[1], fill=rev(grad.other)[2], alpha=0.5) +
  # scale_color_manual(values=biome.pall.all[]) +
  # scale_fill_manual(values=biome.pall.all[]) +
  scale_y_continuous(limits=c(-0.08, 0.065)) +
  labs(y="Cooling Potential\n(deg C / % cover)", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.title.x=element_text(color="black", face="bold"),
        plot.margin=margin(4.5,2, 0.0, 0.5, "lines"))


OtherSlopeLat2 <- ggplot(data=StatsCombined,) +
  coord_flip() +
  # coord_cartesian(, ylim=c(-65,80))
  # geom_point(aes(x=LATITUDE, y=exp(model.tree.slope), color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  # stat_smooth(aes(x=LATITUDE, y=model.tree.slope), color=rev(grad.tree)[1], fill=rev(grad.tree)[2], alpha=0.5) +
  # geom_bar(aes(x=LATITUDE, y=model.tree.slope), fill=rev(grad.tree)[2], stat="summary", fun.y="mean") +
  # geom_errorbar(aes(x=LATITUDE, y=model.tree.slope), fill=rev(grad.tree)[2], stat="summary", fun.y="sd") +
  geom_boxplot(aes(x=cut(LATITUDE, breaks=seq(-65, 80, by=10)), y=model.veg.slope), fill=rev(grad.other)[2]) +
  scale_x_discrete(drop=F) +
  # scale_y_continuous(limits=c(-0.08, 0.065)) +
  labs(y="Cooling Potential\n(deg C / % cover)", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(color="black"),
        axis.title.x=element_text(color="black", face="bold"),
        plot.margin=margin(4.5,2, 0.0, 0.5, "lines"))

# mapLST
# mapTree
# mapOther
# UHILat
# TreeSlopeLat
# OtherSlopeLat

png(file.path(path.figs, "Figure1_UHI_Veg_SlopesLat.png"), height=8, width=6, units="in", res=320)
plot_grid(mapLST, UHILat, mapTree, TreeSlopeLat, mapOther, OtherSlopeLat, nrow=3, rel_widths = c(0.7, 0.3), labels=c("A","B", "C", "D", "E", "F"))
dev.off()


biomeCount <- ggplot(data=StatsCombined,) +
  geom_bar(aes(x=biomeName, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  labs(y="Count", x="Biome") +
  guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title.y=element_text(color="black", face="bold"),
        # axis.text.x=element_text(color="black", angle=-20, hjust=0),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 6.5, 0.5, 0.5), "lines"))

lstEffects <- stack(StatsCombined[,c("EffectLST.tree", "ContribUHI.tree")])
names(lstEffects) <- c("Tree", "Type")
lstEffects$Type <- factor(ifelse(grepl("UHI", lstEffects$Type), "Deficit", "Observed") )
lstEffects$OtherVeg <- stack(StatsCombined[,c("EffectLST.other", "ContribUHI.other")])[,1]
lstEffects[,c("ISOURBID", "biomeName")] <- StatsCombined[,c("ISOURBID", "biomeName")]
summary(lstEffects)

lstTree <- ggplot(data=lstEffects, aes(x=biomeName, y=Tree, fill=biomeName, group=rev(Type))) +
  geom_bar(aes(alpha=Type), position="dodge", stat="summary", fun="mean") +
  stat_summary(geom="errorbar", fun.data=function(x){c("ymin"=mean(x)-sd(x), "y"=mean(x), "ymax"=mean(x)+sd(x))}, position="dodge") +
    # geom_errorbar(stat="summary", position="dodge") +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_alpha_manual(values=c("Deficit"=0.5, "Observed"=1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(x="Biome", y="LST Effect (deg. C)") +
  scale_y_continuous(limits=c(-6,1.5)) +
  guides(fill="none") +
  geom_text(x=rev(levels(StatsCombined$biomeName))[1], y=-5.5, hjust=1, label="cooling due to cover") +
  geom_text(x=rev(levels(StatsCombined$biomeName))[1], y=1.4, hjust=1, label="warming due to cover defficit") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title.y=element_text(color="black", face="bold"),
        # axis.text.x=element_text(color="black", angle=-20, hjust=0),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.75), "lines"))

lstOther <- ggplot(data=lstEffects, aes(x=biomeName, y=OtherVeg, fill=biomeName, group=rev(Type))) +
  geom_bar(aes(alpha=Type), position="dodge", stat="summary", fun="mean") +
  stat_summary(geom="errorbar", fun.data=function(x){c("ymin"=mean(x)-sd(x), "y"=mean(x), "ymax"=mean(x)+sd(x))}, position="dodge") +
  # geom_errorbar(stat="summary", position="dodge") +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_alpha_manual(values=c("Deficit"=0.5, "Observed"=1)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(limits=c(-6,1.5)) +
  labs(x="Biome", y="LST Effect (deg. C)") +
  guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        axis.title=element_text(color="black", face="bold"),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.75), "lines"))


png(file.path(path.figs, "Figure2_VegEffects_LST.png"), height=8, width=6, units="in", res=320)
plot_grid(biomeCount, lstTree, lstOther, nrow=3, rel_heights = c(2, 2, 3), labels=c("A","B", "C"))
dev.off()

# Trees have a clear, consistent cooling potential on global urban surface temperatures, with a global median effect of XXX˚C per percent tree cover (SD XXX˚C/%), and a significant cooling effect in XX% of those cities (Fig. 1)
# hist(cityStatsAnaly$model.tree.slope)
treeCoolMean <- mean(StatsCombined$model.tree.slope)
treeCoolSD <- sd(StatsCombined$model.tree.slope)
round(treeCoolMean, 2); round(treeCoolSD, 2)

round(length(which(StatsCombined$model.tree.p<0.01 & StatsCombined$model.tree.slope<0))/length(StatsCombined$model.tree.slope),2)

# At a global scale, the cooling effect of trees was more than XXX times that of non-tree vegetation (XXX˚C/%; SD XXXX˚C/%).  
otherCoolMean <- mean(StatsCombined$model.veg.slope)
otherCoolSD <- sd(StatsCombined$model.veg.slope)
round(otherCoolMean, 2); round(otherCoolSD, 2)
round(treeCoolMean/otherCoolMean, 1)


# For cities analyzed, lower tree canopy in the metropolitan core than the reference region accounts of a [mean/median] XXX% of the observed UHI.
StatsCombined$EffectLST.tree <- StatsCombined$model.tree.slope*StatsCombined$value.tree.core
StatsCombined$EffectLST.other <- StatsCombined$model.veg.slope*StatsCombined$value.other.core
StatsCombined$ContribUHI.tree <- StatsCombined$model.tree.slope*StatsCombined$value.tree.diff
StatsCombined$ContribUHI.other <- StatsCombined$model.veg.slope*StatsCombined$value.other.diff
summary(StatsCombined)
hist(StatsCombined$EffectLST.tree)

hist(StatsCombined$ContribUHI.tree/StatsCombined$value.LST.diff)

round(mean(StatsCombined$ContribUHI.tree/StatsCombined$value.LST.diff),2); round(sd(StatsCombined$ContribUHI.tree/StatsCombined$value.LST.diff), 2)

# If tree cover in metropolitan cores matched that of the reference region, the observed UHI effect would be reduced by a [mean/median] 0.43˚C (SD 0.53˚C), with the greatest reductions in UHI occurring in biomes with the greatest tree cover deficits.
round(mean(StatsCombined$ContribUHI.tree),2); round(sd(StatsCombined$ContribUHI.tree), 2)

# Globally, trees cool cities an average of XXX˚C (SD XXX˚C) with a only mean cover of XXX% (SD XXX%).  In contrast, non-tree vegetation contributes a mean XXXX˚C (SD XXX˚C) cooling with XX% (SD XX%) cover
round(mean(StatsCombined$EffectLST.tree),2); round(sd(StatsCombined$EffectLST.tree), 2)
round(mean(StatsCombined$value.tree.core),0); round(sd(StatsCombined$value.tree.core), 0)

round(mean(StatsCombined$EffectLST.other),2); round(sd(StatsCombined$EffectLST.other), 2)
round(mean(StatsCombined$value.other.core),0); round(sd(StatsCombined$value.other.core), 0)




# BIOME BREAKDOWNS ----
# On a per-percent cover basis, the cooling potential of trees is greatest in arid and semi-arid biomes where the natural landscape is dominated by grasses and shrubby vegetation (SUPPLEMENT FIG/TABLE)
# SUPPELEMENTAL TABLE 2 BY BIOME---
#  - Cities in analysis; Mean Core [LST/Tree/Veg]; Mean Diff [LST/Tree/Veg]; 
ncitiesAll <- aggregate(ISOURBID ~ biomeName, data=cityAll.stats, FUN = length)
names(ncitiesAll) <- c("Biome", "N.Total")

ncitiesAnaly <- aggregate(ISOURBID ~ biomeName, data=StatsCombined, FUN = length)
ncitiesAnaly[,c("LST.mean", "Tree.mean", "Other.mean")] <- round(aggregate(cbind(value.LST.core, value.tree.core, value.other.core) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:4)], 2)
ncitiesAnaly[,c("LST.sd", "Tree.sd", "Other.sd")] <- round(aggregate(cbind(value.LST.core, value.tree.core, value.other.core) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:4)], 2)
ncitiesAnaly[,c("LSTDiff.mean", "TreeDiff.mean", "OtherDiff.mean")] <- round(aggregate(cbind(value.LST.diff, value.tree.diff, value.other.diff) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:4)], 2)
ncitiesAnaly[,c("LSTDiff.sd", "TreeDiff.sd", "OtherDiff.sd")] <- round(aggregate(cbind(value.LST.diff, value.tree.diff, value.other.diff) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:4)], 2)
# ncitiesAnaly[,c("LST.sd", "Tree.sd", "Other.sd")] <- round(aggregate(cbind(value.LST.core, value.tree.core, value.other.core) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:4)], 2)

ncitiesAnaly

TableCitySummary <- data.frame(Biome = ncitiesAnaly$biomeName,
                               N.Analyzed = ncitiesAnaly$ISOURBID, 
                               LST.mean = paste0(ncitiesAnaly$LST.mean, " (", ncitiesAnaly$LST.sd, ")"),
                               Tree.mean = paste0(round(ncitiesAnaly$Tree.mean, 0), " (", round(ncitiesAnaly$Tree.sd, 0), ")"),
                               OtherVeg.mean = paste0(round(ncitiesAnaly$Other.mean, 0), " (", round(ncitiesAnaly$Other.sd, 0), ")"),
                               LST.diff = paste0(ncitiesAnaly$LSTDiff.mean, " (", ncitiesAnaly$LSTDiff.sd, ")"),
                               Tree.diff = paste0(round(ncitiesAnaly$TreeDiff.mean, 0), " (", round(ncitiesAnaly$TreeDiff.sd, 0), ")"),
                               OtherVeg.diff = paste0(round(ncitiesAnaly$OtherDiff.mean, 0), " (", round(ncitiesAnaly$OtherDiff.sd, 0), ")"))

TableCitySummary <- merge(ncitiesAll, TableCitySummary, all=T)
TableCitySummary$N.Analyzed[is.na(TableCitySummary$N.Analyzed)] <- 0
TableCitySummary

write.csv(TableCitySummary, file.path(path.figs, "SuppTable1_Biome_CitySummaryStats.csv"), row.names=F)

# SUPPELEMENTAL TABLE 3 BY BIOME---
# - Tree/Veg Stats: slope, pSig, Cooling Contrib, UHI due to diff
CoolStats <- aggregate(ISOURBID ~ biomeName, data=StatsCombined, FUN = length)
CoolStats[,c("TreeSlope.mean", "OtherSlope.mean")] <- round(aggregate(cbind(model.tree.slope, model.veg.slope) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:3)], 2)
CoolStats[,c("TreeSlope.sd", "OtherSlope.sd")] <- round(aggregate(cbind(model.tree.slope, model.veg.slope) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:3)], 2)

CoolStats[,c("EffectLST.Tree.mean", "EffectLST.Other.mean")] <- round(aggregate(cbind(EffectLST.tree, EffectLST.other) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:3)], 2)
CoolStats[,c("EffectLST.Tree.sd", "EffectLST.Other.sd")] <- round(aggregate(cbind(EffectLST.tree, EffectLST.other) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:3)], 2)
CoolStats[,c("EffectUHI.Tree.mean", "EffectUHI.Other.mean")] <- round(aggregate(cbind(ContribUHI.tree, ContribUHI.other) ~ biomeName, data=StatsCombined, FUN = mean)[,c(2:3)], 2)
CoolStats[,c("EffectUHI.Tree.sd", "EffectUHI.Other.sd")] <- round(aggregate(cbind(ContribUHI.tree, ContribUHI.other) ~ biomeName, data=StatsCombined, FUN = sd)[,c(2:3)], 2)

CoolStats$TreeCool.sig <- round(aggregate(model.tree.slope ~ biomeName, data=StatsCombined[StatsCombined$model.tree.slope<0 & StatsCombined$model.tree.p<0.01,], FUN = length)[,2], 2)
CoolStats$OtherCool.sig <- round(aggregate(model.veg.slope ~ biomeName, data=StatsCombined[StatsCombined$model.veg.slope<0 & StatsCombined$model.veg.p<0.01,], FUN = length)[,2], 2)
CoolStats$TreeCool.sig.Percent <- round(CoolStats$TreeCool.sig/CoolStats$ISOURBID*100,0)
CoolStats$OtherCool.sig.Percent <- round(CoolStats$OtherCool.sig/CoolStats$ISOURBID*100,0)
CoolStats

CoolStatsSummary <- data.frame(Biome = CoolStats$biomeName,
                               N.Analyzed = CoolStats$ISOURBID, 
                               TreeSlope = paste0(CoolStats$TreeSlope.mean, " (", CoolStats$TreeSlope.sd,")"),
                               PercentTreeCool = paste0(CoolStats$TreeCool.sig.Percent, "%"),
                               OtherSlope = paste0(CoolStats$OtherSlope.mean, " (", CoolStats$OtherSlope.sd,")"),
                               PercentOtherCool = paste0(CoolStats$OtherCool.sig.Percent, "%"),
                               LST.Effect.TreeCover = paste0(CoolStats$EffectLST.Tree.mean, " (", CoolStats$EffectLST.Tree.sd,")"),
                               LST.Effect.OtherCover = paste0(CoolStats$EffectLST.Other.mean, " (", CoolStats$EffectLST.Other.sd,")"),
                               LST.Effect.TreeDiff = paste0(CoolStats$EffectUHI.Tree.mean, " (", CoolStats$EffectUHI.Tree.sd,")"),
                               LST.Effect.OtherDiff = paste0(CoolStats$EffectUHI.Other.mean, " (", CoolStats$EffectUHI.Other.sd,")"))

CoolStatsSummary

write.csv(CoolStatsSummary, file.path(path.figs, "SuppTable2_Biome_CoolingEffects.csv"), row.names=F)

# ##########################################


# ##########################################
# 2. Vegetation cover targets to fully offset UHIs and warming trends
#    Key Result/Messages: 
#       - On average, urban tree cover would need to be double current levels to fully offset the UHI with tree cover alone.  (Presumably the number for non-tree vegetation will be ridiculously high)
#       - In order to have offset the increased warming in cities relative to the reference region, tree cover would need to have increased at more than 6 times the observed median (this would have the UHI rate constant instead of growing)
# 
#   Key Figure/Table Components (needs to show):
#       - [See Fig 2] Tree & other veg targets to fully offset UHI (where things stand relative to an achievable high goal)
#       - Figure 3: 
#           - Purpose: Rate of Tree Cover growth needed to hold UHI effect constant, fully combat recent warming (and compared to what it currently was)
#           - Picture a line showing the current rate of tree growth → the slope would be MUCH steeper to hold the UHI constant; the slope would be even steeper to have held actual mean summer day surface temperature constnat

# ##########################################
# ##########################################
