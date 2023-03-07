###########################################
# Script Description & Outline ----
###########################################
# Purpose: Stats & figures corresponding to current manuscript outline; cleaned up and organized

# Manuscript Outline
# ---------
# 1. Global pattern of Urban Heat Islands & temporal trends
# ---------
#    Key Result/Message: Cities are typically warmer than the surrounding landscape and are getting warmer at a faster rate
#    1.1. Compare mean temperature of urban areas & buffers; break down by biome
#         - Compare mean temperature of urban core vs. buffer; break down by biome → Figure + supplemental table
#        - Which regions show the biggest difference?
#        - Figure: Map of UHI + histogram of differences by biome
#    1.2. Trends in Temperature – difference in rates of change with city versus area
#         - Compare trends in warming over 20 years with city & buffer; break down by biome → Figure + supplemental table w/ Part 1
# ---------
# ---------
# 2. Global patterns of vegetation & temporal trends
# ---------
#    Key Result/Message: Most cities have lower tree cover, although at a global scale we are seeing gains
#    2.1. Correlating differences in UHI with spatial & temporal patterns in vegetation (establish the baseline)
#         - Compare magnitude of mean difference in urban area *relative* to its buffer -- (CITY-Buffer)/Buffer
#         - Need to calculate/present mean tree cover in buffer first; present biome means?
#         - **Present all results as standardized to biome mean**
#    2.2 Temporal Trends 
#         - Calculate City + Buffer mean gain/loss in tree cover relative to time period mean; present as map/biome
#         - Highlight specific cities that are doing much better or worse than expected for their biome
# ---------


# ---------
# 3. *Attribution* of spatio-temporal variation in temperature to vegetation and particularly trees
# ---------
#    Key Result/Message: On average, XX% of the urban heat island effect is attributable to differences in tree cover between urban areas and the surrounding buffer.
#    3.1: Effects of temperature on surface temperature (degrees C per percent cover stats)
#         - Present model slopes (˚C/% cover)
#    3.2. Converting slopes into cooling of surface temperature
#         - Slope x mean Tree Cover = estimated contribution to surface temperature
#         - Change in tree cover relative to temporal trend → what would the UHI be without gains or losses in the urban canopy?
#    3.3. Comparing cooling & tree trends to global warming and other urban warming processes
# ---------

# ---------
# 4. Value of Trees: Direct comparison of trees compared to other vegetation
# ---------
#    Key Result/Message: Trees really are far more effective than other vegetation types in cooling cities
#   [may not need more analysis??]
# ---------


# ---------
# 5. Critical role of trees as green infrastructure to mitigate the effects of climate change [probably no analyses needed]
# ---------
#.   Key Message: Our results show trees playing a key role in cooling cities in every biome on earth
# ---------


###########################################

library(ggplot2); library(RColorBrewer); library(cowplot)


###########################################
# Establish file paths etc ----
###########################################
user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Library/CloudStorage", user.google, "Shared drives", "Urban Ecological Drought/Trees-UHI Manuscript/Analysis_v2")
path.cities <- file.path(path.google, "data_processed_final")

path.figs <- file.path(path.google, "figures_manuscript", "base")
dir.create(path.figs, recursive=T, showWarnings=F)

grad.lst <- c("#2c7bb6", "#abd9e9", "#f7f7f7", "#fdae61", "#d7191c") # ends with red
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.modfit <- c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")


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
# Making the baseline maps of city/biome distribution ----
# ##########################################
biome.hist <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_bar(aes(x=biomeName, fill=biomeName)) +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_discrete(name="Biome") +
  guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle=-30, hjust=0),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"), 
        plot.margin = margin(1, 2, 0.5, 1, "lines"))

biome.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=biomeName), size=0.5) +
  scale_color_manual(name="biome", values=biome.pall.all) +
  guides(color="none") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin = margin(0.5, 2, 1, 3, "lines"))

# biome.map
png(file.path(path.figs, "CityDistribution_Biomes.png"), height=8, width=8, units="in", res=220)
plot_grid(biome.map, biome.hist, ncol=1, rel_heights = c(0.45, 0.55))
dev.off()
# ##########################################


##########################################
# 1. Global pattern of Urban Heat Islands & temporal trends ----
#    Key Result/Message: Cities are typically warmer than the surrounding landscape and are getting warmer at a faster rate
#    1.1. Compare mean temperature of urban areas & buffers; break down by biome
#         - Compare mean temperature of urban core vs. buffer; break down by biome → Figure + supplemental table
#         - Which regions show the biggest difference?
#         - Figure: Map of UHI + histogram of differences by biome
#    1.2. Trends in Temperature – difference in rates of change with city versus area
#         - Compare trends in warming over 20 years with city & buffer; break down by biome → Figure + supplemental table w/ Part 1
##########################################
## ----------
##     1.1. Compare mean temperature of urban areas & buffers; break down by biome -----
#         - Compare mean temperature of urban core vs. buffer; break down by biome → Figure + supplemental table
#        - Which regions show the biggest difference?
#        - Figure: Map of UHI + histogram of differences by biome

png(file.path(path.figs, "LST_Patterns_Difference_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="LST",]) +
  geom_histogram(aes(x=value.mean.diff, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Difference: Metro Core - 10 km Buffer", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "LST_Patterns_Difference_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="LST",]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=value.mean.diff), size=0.5) +
  scale_color_gradientn(name="LST Diff.\n(deg. C)", colors=grad.lst,  limits=c(-5, 5)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

BuffCoreComparison <- stack(CityBuffStats[,c("value.mean.core", "value.mean.buffer")])
names(BuffCoreComparison) <- c("value.mean", "location")
BuffCoreComparison$location <- factor(ifelse(grepl("core", BuffCoreComparison$location), "core", "buffer"), levels=c("core", "buffer"))
# BuffCoreComparison$location <-
BuffCoreComparison[,c("ISOURBID", "factor", "NAME", "biomeName")] <- CityBuffStats[,c("ISOURBID", "factor", "NAME", "biomeName")]

png(file.path(path.figs, "LST_Patterns_Values_boxplot.png"), height=8, width=8, units="in", res=220)
ggplot(data=BuffCoreComparison[BuffCoreComparison$factor=="LST",]) +
  geom_boxplot(aes(x=biomeName, y=value.mean, fill=biomeName, alpha=location), position="dodge") +
  # geom_boxplot(aes(x=biomeName, y=value.mean.buffer, fill=biomeName, alpha="Buffer"), position="dodge") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(name="Mean Summer LST (deg. C)", expand=c(0,0)) +
  scale_x_discrete(name="Biome Type") +
  scale_fill_manual(values=biome.pall.all, guide="none") +
  scale_alpha_manual(values=c("core"=1, "buffer"=0.3)) +
  labs(caption="dark fill = urban core; light fill= 10 km buffer") +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text.x=element_text(angle=-15, hjust=0))
dev.off()

# Creating a summary Tables
LSTbiome <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST",], FUN=length)
names(LSTbiome) <- c("Biome", "N.Cities")
LSTbiome$LST.CITY <- aggregate(value.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST",], FUN=mean)[,2]
LSTbiome$LST.CITY.SD <- aggregate(value.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST",], FUN=sd)[,2]
LSTbiome$LST.Diff <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST",], FUN=mean)[,2]
LSTbiome$LST.Diff.SD <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST",], FUN=sd)[,2]
# LSTbiome$LST.SE <- LSTbiome$LST.SD/sqrt(LSTbiome$N.Cities)
# LSTbiome[c("LST.CITY", "LST.Diff", "LST.SD", "LST.SE")] <- round(LSTbiome[c("LST.Diff", "LST.SD", "LST.SE")], 2)

# Add in percent of cities with stat sig warming 
LSTbiome.Sig <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST" &  CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.p<0.01,], FUN=length)
names(LSTbiome.Sig) <- c("Biome", "N.UHI.sig")
LSTbiome <- merge(LSTbiome, LSTbiome.Sig, all=T)
LSTbiome[is.na(LSTbiome)] <- 0
LSTbiome

tableLSTBiome <- data.frame(Biome=LSTbiome$Biome, N.Cities=LSTbiome$N.Cities, 
                            CityLST=paste0(round(LSTbiome$LST.CITY, 1), " (",round(LSTbiome$LST.CITY.SD, 1), ")"),
                            CityLSTDiff=paste0(round(LSTbiome$LST.Diff, 1), " (",round(LSTbiome$LST.Diff.SD, 1), ")"),
                            pCitiesSigUHI=paste0(round(LSTbiome$N.UHI.sig/LSTbiome$N.Cities*100), "%"))

write.csv(tableLSTBiome, file.path(path.figs, "LST_Patterns_Biome.csv"), row.names=F)


# XX% of cities (XXX out of XXXX) across the globe show a significant difference in temperature between the urban core and 10 km buffer.  Of these, XX% are warmer with a mean difference of YY˚C (Figure)
length(which(CityBuffStats$factor=="LST" &  CityBuffStats$value.mean.diff.p<0.01))/length(which(CityBuffStats$factor=="LST"))
length(which(CityBuffStats$factor=="LST" &  CityBuffStats$value.mean.diff.p<0.01 & CityBuffStats$value.mean.diff>0))/length(which(CityBuffStats$factor=="LST" &  CityBuffStats$value.mean.diff.p<0.01))

mean(CityBuffStats$value.mean.diff[which(CityBuffStats$factor=="LST" &  CityBuffStats$value.mean.diff.p<0.01 & CityBuffStats$value.mean.diff>0)]);
sd(CityBuffStats$value.mean.diff[which(CityBuffStats$factor=="LST" &  CityBuffStats$value.mean.diff.p<0.01 & CityBuffStats$value.mean.diff>0)])
## ----------




## ----------
#    1.2. Trends in Temperature – difference in rates of change with city versus area
#         - Compare trends in warming over 20 years with city & buffer; break down by biome → Figure + supplemental table w/ Part 1
## ----------
summary(CityBuffStats[CityBuffStats$factor=="LST",])

png(file.path(path.figs, "LST_Trends_Cities_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="LST",]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trend.mean.core), size=0.5) +
  scale_color_gradientn(name="Core LST Trend.\n(deg. C / yr)", colors=grad.lst,  limits=c(-0.4, 0.4)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "LST_Trends_Difference_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="LST",]) +
  geom_histogram(aes(x=trend.mean.diff, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Trend Difference: Metro Core - 10 km Buffer", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


LSTbiomeTrend <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.diff),], FUN=length)
names(LSTbiomeTrend) <- c("Biome", "N.Cities")
LSTbiomeTrend$LST.Trend.CITY <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core),], FUN=mean)[,2]
LSTbiomeTrend$LST.Trend.CITY.SD <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core),], FUN=sd)[,2]
LSTbiomeTrend$LST.Trend.Buff <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core),], FUN=mean)[,2]
LSTbiomeTrend$LST.Trend.Buff.SD <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core),], FUN=sd)[,2]
LSTbiomeTrend$LST.Trend.Diff <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST",], FUN=mean)[,2]
LSTbiomeTrend$LST.Trend.Diff.SD <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST",], FUN=sd)[,2]
# LSTbiome$LST.SE <- LSTbiome$LST.SD/sqrt(LSTbiome$N.Cities)
# LSTbiome[c("LST.CITY", "LST.Diff", "LST.SD", "LST.SE")] <- round(LSTbiome[c("LST.Diff", "LST.SD", "LST.SE")], 2)

# Add in percent of cities with stat sig warming 
LSTbiomeTrend.Sig <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST"  & !is.na(CityBuffStats$trend.mean.core) &  CityBuffStats$trend.mean.diff>0 & CityBuffStats$trend.mean.diff.p<0.01,], FUN=length)
names(LSTbiomeTrend.Sig) <- c("Biome", "N.WarmDiff.sig")

LSTbiomeTrend.Sig2 <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST"  & !is.na(CityBuffStats$trend.mean.core) &  CityBuffStats$trend.mean.core>0 & CityBuffStats$trend.p.core<0.01,], FUN=length)
names(LSTbiomeTrend.Sig2) <- c("Biome", "N.WarmCity.sig")
LSTbiomeTrend.Sig2$N.WarmBuff.sig <- aggregate(trend.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="LST"  & !is.na(CityBuffStats$trend.mean.buffer) &  CityBuffStats$trend.mean.buffer>0 & CityBuffStats$trend.p.buffer<0.01,], FUN=length)[,2]

LSTbiomeTrend <- merge(LSTbiomeTrend, LSTbiomeTrend.Sig2, all=T)
LSTbiomeTrend <- merge(LSTbiomeTrend, LSTbiomeTrend.Sig, all=T)
LSTbiomeTrend <- LSTbiomeTrend[order(LSTbiomeTrend$Biome),] # Sorting to make in a consistent order
LSTbiomeTrend[is.na(LSTbiomeTrend)] <- 0
LSTbiomeTrend

tableLSTBiomeTrend <- data.frame(Biome=LSTbiomeTrend$Biome, N.Cities=LSTbiomeTrend$N.Cities, 
                                 LSTTrendCity=paste0(round(LSTbiomeTrend$LST.Trend.CITY, 2), " (",round(LSTbiomeTrend$LST.Trend.CITY, 2), ")"),
                                 LSTTrendBuffer=paste0(round(LSTbiomeTrend$LST.Trend.Buff, 2), " (",round(LSTbiomeTrend$LST.Trend.Buff.SD, 2), ")"),
                                 LSTTrendDiff=paste0(round(LSTbiomeTrend$LST.Trend.Diff, 2), " (",round(LSTbiomeTrend$LST.Trend.Diff.SD, 2), ")"),
                                 pSigWarmCity=paste0(round(LSTbiomeTrend$N.WarmCity.sig/LSTbiomeTrend$N.Cities*100), "%"),
                                 pSigWarmBuffer=paste0(round(LSTbiomeTrend$N.WarmBuff.sig/LSTbiomeTrend$N.Cities*100), "%"),
                                 pSigWarmDiff=paste0(round(LSTbiomeTrend$N.WarmDiff.sig/LSTbiomeTrend$N.Cities*100), "%"))
tableLSTBiomeTrend
write.csv(tableLSTBiomeTrend, file.path(path.figs, "LST_Trends_Biome.csv"), row.names=F)



# XX% of cities and XX% of surrounding areas have significantly warmed between 2000 and 2020, with a global mean increase of XX˚C across all cities. 
length(which(CityBuffStats$factor=="LST" &  CityBuffStats$trend.p.core<0.01 & CityBuffStats$trend.mean.core>0 &  !is.na(CityBuffStats$trend.mean.core)))/length(which(CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core)))

length(which(CityBuffStats$factor=="LST" &  CityBuffStats$trend.p.buffer<0.01 & CityBuffStats$trend.mean.buffer>0 &  !is.na(CityBuffStats$trend.mean.buffer)))/length(which(CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.buffer)))

mean(CityBuffStats$trend.mean.core[which(CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core))])*20; sd(CityBuffStats$trend.mean.core[which(CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core))])*20

# XX% of cities displayed warmer temperatures trends over the 20-year period than the surrounding region.
length(which(CityBuffStats$factor=="LST" &  CityBuffStats$trend.mean.diff.p<0.01 & CityBuffStats$trend.mean.diff>0 &  !is.na(CityBuffStats$trend.mean.diff)))/length(which(CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.diff)))

# As with patterns of global climate change, patterns of urban warming are not uniform across the globe with the greatest changes being seen in YY biome, where the observed warming rate was XX the global average.
avgGlobal <- mean(CityBuffStats$trend.mean.core[which(CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core))])
avgTBF <- mean(CityBuffStats$trend.mean.core[which(CityBuffStats$factor=="LST" & !is.na(CityBuffStats$trend.mean.core) & CityBuffStats$biomeName=="Temperate Broadleaf Forest")])

avgTBF/avgGlobal
## ----------
##########################################



##########################################
# 2. Global patterns of vegetation & temporal trends ----
# ---------
#    Key Result/Message: Most cities have lower tree cover, although at a global scale we are seeing gains
#    2.1. Correlating differences in UHI with spatial & temporal patterns in vegetation (establish the baseline)
#         - Compare magnitude of mean difference in urban area *relative* to its buffer -- (CITY-Buffer)/Buffer
#         - Need to calculate/present mean tree cover in buffer first; present biome means?
#         - **Present all results as standardized to biome mean**
#    2.2 Temporal Trends 
#         - Calculate City + Buffer mean gain/loss in tree cover relative to time period mean; present as map/biome
#         - Highlight specific cities that are doing much better or worse than expected for their biome
##########################################
summary(cityAll.stats)
summary(CityBuffStats)
## ----------
#    2.1. Correlating differences in UHI with spatial & temporal patterns in vegetation (establish the baseline)
#         - Compare magnitude of mean difference in urban area *relative* to its buffer -- (CITY-Buffer)/Buffer
#         - Need to calculate/present mean tree cover in buffer first; present biome means?
#         - **Present all results as standardized to biome mean**
## ----------
summary(CityBuffStats[CityBuffStats$factor!="LST","value.mean.diff"])

png(file.path(path.figs, "Vegetation_Patterns_Difference_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor!="LST",]) +
  facet_grid(factor~.) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=value.mean.diff), size=0.5) +
  scale_color_gradientn(name="Veg. Diff.\n(% cover)", colors=grad.tree,  limits=c(-45, 45)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

BuffCoreComparison <- stack(CityBuffStats[,c("value.mean.core", "value.mean.buffer")])
names(BuffCoreComparison) <- c("value.mean", "location")
BuffCoreComparison$location <- factor(ifelse(grepl("core", BuffCoreComparison$location), "core", "buffer"), levels=c("core", "buffer"))
# BuffCoreComparison$location <-
BuffCoreComparison[,c("ISOURBID", "factor", "NAME", "biomeName")] <- CityBuffStats[,c("ISOURBID", "factor", "NAME", "biomeName")]
BuffCoreComparison$factor <- factor(BuffCoreComparison$factor, levels=c("LST", "tree", "other veg"))

png(file.path(path.figs, "Vegetation_Patterns_Values_boxplot.png"), height=8, width=8, units="in", res=220)
ggplot(data=BuffCoreComparison[BuffCoreComparison$factor!="LST",]) +
  facet_grid(factor~.) +
  geom_boxplot(aes(x=biomeName, y=value.mean, fill=biomeName, alpha=location), position="dodge") +
  # geom_boxplot(aes(x=biomeName, y=value.mean.buffer, fill=biomeName, alpha="Buffer"), position="dodge") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(name="Mean Summer LST (deg. C)", expand=c(0,0)) +
  scale_x_discrete(name="Biome Type") +
  scale_fill_manual(values=biome.pall.all, guide="none") +
  scale_alpha_manual(values=c("core"=1, "buffer"=0.3)) +
  labs(caption="dark fill = urban core; light fill= 10 km buffer") +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text.x=element_text(angle=-15, hjust=0))
dev.off()

png(file.path(path.figs, "Vegetation_Patterns_Difference_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor!="LST",]) +
  facet_grid(factor~.) +
  geom_histogram(aes(x=value.mean.diff, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Difference: Metro Core - 10 km Buffer", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


# Creating a summary Tables
VegBiome <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=length)
names(VegBiome) <- c("Biome", "N.Cities")
VegBiome$Tree.CITY <- aggregate(value.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=mean)[,2]
VegBiome$Tree.CITY.SD <- aggregate(value.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=sd)[,2]
VegBiome$Tree.BUFF <- aggregate(value.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=mean)[,2]
VegBiome$Tree.BUFF.SD <- aggregate(value.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=sd)[,2]
VegBiome$Tree.Diff <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=mean)[,2]
VegBiome$Tree.Diff.SD <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=sd)[,2]

VegBiome$Other.CITY <- aggregate(value.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=mean)[,2]
VegBiome$Other.CITY.SD <- aggregate(value.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=sd)[,2]
VegBiome$Other.BUFF <- aggregate(value.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=mean)[,2]
VegBiome$Other.BUFF.SD <- aggregate(value.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=sd)[,2]
VegBiome$Other.Diff <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=mean)[,2]
VegBiome$Other.Diff.SD <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=sd)[,2]
# Vegbiome$Veg.SE <- Vegbiome$Veg.SD/sqrt(Vegbiome$N.Cities)
# Vegbiome[c("Veg.CITY", "Veg.Diff", "Veg.SD", "Veg.SE")] <- round(Vegbiome[c("Veg.Diff", "Veg.SD", "Veg.SE")], 2)

# Add in percent of cities with stat sig warming 
VegBiome.Sig <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree" &  CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.p<0.01,], FUN=length)
names(VegBiome.Sig) <- c("Biome", "N.Tree.sig.lo")
VegBiome.Sig$N.Other.sig.hi <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg" &  CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.p<0.01,], FUN=length)[,2]

VegBiome.Sig2 <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree" &  CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.p<0.01,], FUN=length)
names(VegBiome.Sig2) <- c("Biome", "N.Tree.sig.hi")
# VegBiome.Sig <- merge(VegBiome.Sig, VegBiome.Sig2, all=T)

VegBiome.Sig3 <- aggregate(value.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg" &  CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.p<0.01,], FUN=length)
names(VegBiome.Sig3) <- c("Biome", "N.Other.sig.lo")

VegBiome.Sig <- merge(merge(VegBiome.Sig, VegBiome.Sig2, all=T), VegBiome.Sig3, all=T)
VegBiome.Sig[is.na(VegBiome.Sig)] <- 0

VegBiome <- merge(VegBiome, VegBiome.Sig, all=T)
VegBiome <- VegBiome[order(VegBiome$Biome),] # Sorting to make in a consistent order
VegBiome
# 
tableTreeBiome <- data.frame(Biome=VegBiome$Biome, N.Cities=VegBiome$N.Cities,
                            CityTree=paste0(round(VegBiome$Tree.CITY, 0), " (",round(VegBiome$Tree.CITY.SD, 0), ")"),
                            BufferTree=paste0(round(VegBiome$Tree.BUFF, 0), " (",round(VegBiome$Tree.BUFF.SD, 0), ")"),
                            TreeDiff=paste0(round(VegBiome$Tree.Diff, 0), " (",round(VegBiome$Tree.Diff.SD, 0), ")"),
                            pCityTreeLo=paste0(round(VegBiome$N.Tree.sig.lo/VegBiome$N.Cities*100), "%"),
                            pCityTreehi=paste0(round(VegBiome$N.Tree.sig.hi/VegBiome$N.Cities*100), "%"))
tableTreeBiome
write.csv(tableTreeBiome, file.path(path.figs, "Vegetation-Tree_Patterns_Biome.csv"), row.names=F)

tableOtherVegBiome <- data.frame(Biome=VegBiome$Biome, N.Cities=VegBiome$N.Cities,
                             CityOther=paste0(round(VegBiome$Other.CITY, 0), " (",round(VegBiome$Other.CITY.SD, 0), ")"),
                             BufferOther=paste0(round(VegBiome$Other.BUFF, 0), " (",round(VegBiome$Other.BUFF.SD, 0), ")"),
                             OtherDiff=paste0(round(VegBiome$Other.Diff, 0), " (",round(VegBiome$Other.Diff.SD, 0), ")"),
                             pCityOtherLo=paste0(round(VegBiome$N.Other.sig.lo/VegBiome$N.Cities*100), "%"),
                             pCityOtherhi=paste0(round(VegBiome$N.Other.sig.hi/VegBiome$N.Cities*100), "%"))
tableOtherVegBiome
write.csv(tableOtherVegBiome, file.path(path.figs, "Vegetation-Other_Patterns_Biome.csv"), row.names=F)


# # Text snippets
# X% of all cities had lower tree cover in the urban core relative to the surrounding region, but both the magnitude of difference and frequency of this pattern were greatest in mesic forested biomes (Table-Trees).  
citiesTreeLo <- which(CityBuffStats$factor=="tree" &  CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.p<0.01)
length(citiesTreeLo)
length(citiesTreeLo)/length(which(CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$value.mean.diff)))

#Across all forested biomes, more than three-quarters of cities had lowered tree cover, with a mean difference of X%.
citiesTreeLoForest <- which(CityBuffStats$factor=="tree" &  CityBuffStats$value.mean.diff<0 & CityBuffStats$value.mean.diff.p<0.01 & grepl("Forest", CityBuffStats$biomeName))
length(citiesTreeLoForest)
length(citiesTreeLoForest)/length(which(CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$value.mean.diff) &  grepl("Forest", CityBuffStats$biomeName)))

mean(CityBuffStats$value.mean.diff[citiesTreeLoForest]); sd(CityBuffStats$value.mean.diff[citiesTreeLoForest])



citiesTreeHi <- which(CityBuffStats$factor=="tree" &  CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.p<0.01)
length(citiesTreeHi)
length(citiesTreeHi)/length(which(CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$value.mean.diff)))


# Although the dominant pattern, reduced tree cover in cities is not universal, particularly in arid or semi-arid regions.  In temperate grassland biomes (mean buffer tree cover = 8%), 50% of cities had higher tree cover than the surrounding area and desert regions (mean buffer tree cover = 5%) had similar proportions of cities with lower (43%) versus higher (39%) tree cover (Table, Figure).  Many of the cities in temperate forest biomes where tree cover is higher in urban areas occur in regions that have heavily converted to agriculture, such as the midwestern U.S. and northeastern China (Figure-Map).
                                                                                                                                                        
citiesTreeHi <- which(CityBuffStats$factor=="tree" &  CityBuffStats$value.mean.diff>0 & CityBuffStats$value.mean.diff.p<0.01)
length(citiesTreeHi)
length(citiesTreeHi)/length(which(CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$value.mean.diff)))

summary(CityBuffStats[citiesTreeHi,])
summary(as.factor(substr(CityBuffStats$ISOURBID[citiesTreeHi], 1, 3)))


# # *** Standardizing Percent Cover in a couple ways ***
# --> This doesn't make a giant difference in our figures, so I think we just skip this for now...
# # 1. looking at difference as a proportion of what's in the buffer
CityBuffStats$value.mean.pDiff[CityBuffStats$factor!="LST"] <- CityBuffStats$value.mean.diff[CityBuffStats$factor!="LST"]/CityBuffStats$value.mean.buffer[CityBuffStats$factor!="LST"]

# # 2. Standardizing Percent Cover to the Biome Mean of the BUFFER ***
for(BIOME in unique(VegBiome$Biome)){
  rowBiomeTree <- which(CityBuffStats$biomeName==BIOME & CityBuffStats$factor=="tree")
  rowBiomeOther <- which(CityBuffStats$biomeName==BIOME & CityBuffStats$factor=="other veg")
  refTree <- VegBiome$Tree.BUFF[VegBiome$Biome==BIOME]
  refOther <- VegBiome$Other.BUFF[VegBiome$Biome==BIOME]

  CityBuffStats[rowBiomeTree, "dValueBiome.core"] <- CityBuffStats$value.mean.core[rowBiomeTree] - refTree
  CityBuffStats[rowBiomeTree, "pValueBiome.core"] <- CityBuffStats$value.mean.core[rowBiomeTree]/refTree
  CityBuffStats[rowBiomeTree, "dValueBiome.buffer"] <- CityBuffStats$value.mean.buffer[rowBiomeTree] - refTree
  CityBuffStats[rowBiomeTree, "pValueBiome.buffer"] <- CityBuffStats$value.mean.buffer[rowBiomeTree]/refTree
  CityBuffStats[rowBiomeTree, "dValueBiome.core"] <- CityBuffStats$value.mean.core[rowBiomeTree] - refTree
  CityBuffStats[rowBiomeTree, "pValueBiome.core"] <- CityBuffStats$value.mean.core[rowBiomeTree]/refTree
  # CityBuffStats[rowBiomeTree, "dValueBiome.diff"] <- CityBuffStats$value.mean.diff[rowBiomeTree] - refTree
  CityBuffStats[rowBiomeTree, "pValueBiome.diff"] <- CityBuffStats$value.mean.diff[rowBiomeTree]/refTree
}
summary(CityBuffStats)

png(file.path(path.figs, "Vegetation-Tree_Patterns_Difference_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="tree",]) +
  facet_grid(factor~.) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=value.mean.diff), size=0.5) +
  scale_color_gradientn(name="Cover Diff.\n(%)", colors=grad.tree, limits=c(-40,40)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "Vegetation-Tree_Patterns_Difference-Proportion_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="tree",]) +
  facet_grid(factor~.) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=pValueBiome.diff), size=0.5) +
  scale_color_gradientn(name="Biome Rel. Diff.\n(prop.)", colors=grad.tree, limits=c(-1.5,1.5)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "Vegetation-Tree_Patterns_Difference-Proportion.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="tree",]) +
  facet_grid(factor~.) +
  geom_histogram(aes(x=value.mean.diff, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Difference: Metro Core - 10 km Buffer", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "Vegetation-Tree_Patterns_Difference-Proportion_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="tree",]) +
  facet_grid(factor~.) +
  geom_histogram(aes(x=pValueBiome.diff, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Proportional Difference: (Core - Buffer)/[Biome Buffer]", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()



## ----------
#    2.2 Temporal Trends 
#         - Calculate City + Buffer mean gain/loss in tree cover relative to time period mean; present as map/biome
#         - Highlight specific cities that are doing much better or worse than expected for their biome
## ----------
summary(CityBuffStats[CityBuffStats$factor=="tree",])

png(file.path(path.figs, "Vegetation-Tree_Trends_Cities_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="tree",]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=trend.mean.core), size=0.5) +
  scale_color_gradientn(name="Core Tree Trend.\n(% / yr)", colors=grad.tree,  limits=c(-1, 1)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "Vegetation-Tree_Trends_Cities_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="tree",]) +
  geom_histogram(aes(x=trend.mean.core, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Trend Difference: Metro Core - 10 km Buffer", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


png(file.path(path.figs, "Vegetation-Tree_Trends_Difference_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=CityBuffStats[CityBuffStats$factor=="tree",]) +
  geom_histogram(aes(x=trend.mean.diff, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Trend Difference: Metro Core - 10 km Buffer", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

BuffCoreCompTrend <- stack(CityBuffStats[,c("trend.mean.core", "trend.mean.buffer")])
names(BuffCoreCompTrend) <- c("trend.mean", "location")
BuffCoreCompTrend$location <- factor(ifelse(grepl("core", BuffCoreCompTrend$location), "core", "buffer"), levels=c("core", "buffer"))
# BuffCoreComparison$location <-
BuffCoreCompTrend[,c("ISOURBID", "factor", "NAME", "biomeName")] <- CityBuffStats[,c("ISOURBID", "factor", "NAME", "biomeName")]
BuffCoreCompTrend$factor <- factor(BuffCoreCompTrend$factor, levels=c("LST", "tree", "other veg"))

png(file.path(path.figs, "Vegetation-Tree_Trends_boxplot.png"), height=8, width=8, units="in", res=220)
ggplot(data=BuffCoreCompTrend[BuffCoreCompTrend$factor=="tree",]) +
  # facet_grid(factor~.) +
  geom_boxplot(aes(x=biomeName, y=trend.mean, fill=biomeName, alpha=location), position="dodge") +
  # geom_boxplot(aes(x=biomeName, y=value.mean.buffer, fill=biomeName, alpha="Buffer"), position="dodge") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(name="Tree Trend (%/yr)", expand=c(0,0)) +
  scale_x_discrete(name="Biome Type") +
  scale_fill_manual(values=biome.pall.all, guide="none") +
  scale_alpha_manual(values=c("core"=1, "buffer"=0.3)) +
  labs(caption="dark fill = urban core; light fill= 10 km buffer") +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text.x=element_text(angle=-15, hjust=0))
dev.off()

# Creating a summary Tables
VegBiomeTrend <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=length)
names(VegBiomeTrend) <- c("Biome", "N.Cities")
VegBiomeTrend$Tree.CITY <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=mean)[,2]
VegBiomeTrend$Tree.CITY.SD <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=sd)[,2]
VegBiomeTrend$Tree.BUFF <- aggregate(trend.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=mean)[,2]
VegBiomeTrend$Tree.BUFF.SD <- aggregate(trend.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=sd)[,2]
VegBiomeTrend$Tree.Diff <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=mean)[,2]
VegBiomeTrend$Tree.Diff.SD <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree",], FUN=sd)[,2]

VegBiomeTrend$Other.CITY <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=mean)[,2]
VegBiomeTrend$Other.CITY.SD <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=sd)[,2]
VegBiomeTrend$Other.BUFF <- aggregate(trend.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=mean)[,2]
VegBiomeTrend$Other.BUFF.SD <- aggregate(trend.mean.buffer ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=sd)[,2]
VegBiomeTrend$Other.Diff <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=mean)[,2]
VegBiomeTrend$Other.Diff.SD <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg",], FUN=sd)[,2]


# Add in percent of cities with stat sig warming
VegBiomeTrend.Sig <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.diff<0 & CityBuffStats$trend.mean.diff.p<0.01,], FUN=length)
names(VegBiomeTrend.Sig) <- c("Biome", "N.Tree.sig.lo")
VegBiomeTrend.Sig$N.Other.sig.hi <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg" &  CityBuffStats$trend.mean.diff>0 & CityBuffStats$trend.mean.diff.p<0.01,], FUN=length)[,2]
VegBiomeTrend.Sig$N.Tree.City.Neg <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.core<0 & CityBuffStats$trend.p.core<0.01,], FUN=length)[,2]

VegBiomeTrend.Sig2 <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.diff>0 & CityBuffStats$trend.mean.diff.p<0.01,], FUN=length)
names(VegBiomeTrend.Sig2) <- c("Biome", "N.Tree.sig.hi")
VegBiomeTrend.Sig2$N.Other.sig.lo <- aggregate(trend.mean.diff ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="other veg" &  CityBuffStats$trend.mean.diff<0 & CityBuffStats$trend.mean.diff.p<0.01,], FUN=length)[,2]
# VegBiomeTrend.Sig <- merge(VegBiomeTrend.Sig, VegBiomeTrend.Sig2, all=T)

VegBiomeTrend.Sig3 <- aggregate(trend.mean.core ~ biomeName, data=CityBuffStats[CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.core>0 & CityBuffStats$trend.p.core<0.01,], FUN=length)
names(VegBiomeTrend.Sig3) <- c("Biome", "N.Tree.City.Pos")

VegBiomeTrend.Sig <- merge(merge(VegBiomeTrend.Sig, VegBiomeTrend.Sig2, all=T), VegBiomeTrend.Sig3, all=T)
VegBiomeTrend.Sig[is.na(VegBiomeTrend.Sig)] <- 0

VegBiomeTrend <- merge(VegBiomeTrend, VegBiomeTrend.Sig, all=T)
VegBiomeTrend <- VegBiomeTrend[order(VegBiomeTrend$Biome),] # Sorting to make in a consistent order
VegBiomeTrend


tableTreeTrendBiome <- data.frame(Biome=VegBiomeTrend$Biome, N.Cities=VegBiomeTrend$N.Cities,
                                  CityTree=paste0(round(VegBiomeTrend$Tree.CITY, 2), " (",round(VegBiomeTrend$Tree.CITY.SD, 2), ")"),
                                  BufferTree=paste0(round(VegBiomeTrend$Tree.BUFF, 2), " (",round(VegBiomeTrend$Tree.BUFF.SD, 2), ")"),
                                  TreeDiff=paste0(round(VegBiomeTrend$Tree.Diff, 2), " (",round(VegBiomeTrend$Tree.Diff.SD, 2), ")"),
                                  pCityTreePos=paste0(round(VegBiomeTrend$N.Tree.City.Pos/VegBiomeTrend$N.Cities*100), "%"),
                                  pCityTreeNeg=paste0(round(VegBiomeTrend$N.Tree.City.Neg/VegBiomeTrend$N.Cities*100), "%"),
                                  pCityTreeLo=paste0(round(VegBiomeTrend$N.Tree.sig.lo/VegBiomeTrend$N.Cities*100), "%"),
                                  pCityTreehi=paste0(round(VegBiomeTrend$N.Tree.sig.hi/VegBiomeTrend$N.Cities*100), "%"))
tableTreeTrendBiome
write.csv(tableTreeTrendBiome, file.path(path.figs, "Vegetation-Tree_Trend_Biome.csv"), row.names=F)


# Now backign up some text
ncities <- length(which(CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$trend.mean.diff)))

# Tree cover is increasing in the majority of cities (XX%) across all biomes with more than half of cities in most biomes showing significant increases (figure, table).
citiesTreePos <- which(CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.core>0 & CityBuffStats$trend.p.core<0.01)
length(citiesTreePos)
length(citiesTreePos)/ncities

citiesTreeNeg <- which(CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.core<0 & CityBuffStats$trend.p.core<0.01)
length(citiesTreeNeg)
length(citiesTreeNeg)/ncities

# Globally, tree cover across all core metropolitan areas is increasing at a rate of XX%/yr.
mean(CityBuffStats$trend.mean.core[CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$trend.mean.core)]); sd(CityBuffStats$trend.mean.core[CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$trend.mean.core)])
mean(CityBuffStats$trend.mean.core[CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$trend.mean.core)])*20

# However, the rate of tree cover increase is typically slower in the urban core compare to the surrounding 10 km buffer, with a mean difference of X %/yr (SD X%/yr) across all cities.
mean(CityBuffStats$trend.mean.diff[CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$trend.mean.core)]); sd(CityBuffStats$trend.mean.diff[CityBuffStats$factor=="tree" &  !is.na(CityBuffStats$trend.mean.core)])

citiesTreeSlow <- which(CityBuffStats$factor=="tree" & CityBuffStats$trend.mean.diff<0 & CityBuffStats$trend.mean.diff.p<0.01)
length(citiesTreeSlow)
length(citiesTreeSlow)/ncities


# In X% of cities, tree cover is increasing more slowly than the buffer and in an additional X%, the urban core is losing tree cover while the surrounding region is gaining or maintaining tree cover.
citiesTreePosSlow <- which(CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.core>0 & CityBuffStats$trend.mean.buffer>=0 & CityBuffStats$trend.mean.diff<0 & CityBuffStats$trend.mean.diff.p<0.01)
length(citiesTreePosSlow)
length(citiesTreePosSlow)/ncities

citiesTreeOpposite <- which(CityBuffStats$factor=="tree" &  CityBuffStats$trend.mean.core<0 & CityBuffStats$trend.mean.buffer>=0 & CityBuffStats$trend.mean.diff<0 & CityBuffStats$trend.mean.diff.p<0.01)
length(citiesTreeOpposite)
length(citiesTreeOpposite)/ncities


# Conversely, X% of cities show a more positive trend in tree cover than the surrounding buffer region between 2000 and 2020.  
citiesTreePosFast <- which(CityBuffStats$factor=="tree" & CityBuffStats$trend.mean.diff>0 & CityBuffStats$trend.mean.diff.p<0.01)
length(citiesTreePosFast)
length(citiesTreePosSlow)/ncities

##########################################


##########################################
# 3. *Attribution* of spatio-temporal variation in temperature to vegetation and particularly trees ----
# ---------
#    Key Result/Message: On average, XX% of the urban heat island effect is attributable to differences in tree cover between urban areas and the surrounding buffer.
#    3.1: Effects of temperature on surface temperature (degrees C per percent cover stats)
#         - Present model slopes (˚C/% cover)
#    3.2. Converting slopes into cooling of surface temperature
#         - Slope x mean Tree Cover = estimated contribution to surface temperature
#         - Change in tree cover relative to temporal trend → what would the UHI be without gains or losses in the urban canopy?
#    3.3. Comparing cooling & tree trends to global warming and other urban warming processes
##########################################
summary(cityAll.stats)

# Background figure showing the performance across all regions
png(file.path(path.figs, "Model_R2adj_Cities_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=cityAll.stats) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=model.R2adj), size=0.5) +
  scale_color_gradientn(name="Model Adj. R2", colors=grad.modfit,  limits=c(0, 1)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


## ----------
#    3.1: Effects of temperature on surface temperature (degrees C per percent cover stats)
#         - Present model slopes (˚C/% cover)
## ----------

# Knowing that some of our model estimates of tree and other veg effect are outliers, lets add a flag so we can remove them down the road
sig.thresh <- 4 # Using 4-sigma 
tree.slope.mean <- mean(cityAll.stats$model.tree.slope); 
tree.slope.sd <- sd(cityAll.stats$model.tree.slope)
other.slope.mean <- mean(cityAll.stats$model.veg.slope); 
other.slope.sd <- sd(cityAll.stats$model.veg.slope)


cityAll.stats$Outlier.TreeSlope <- NA
cityAll.stats$Outlier.TreeSlope <- ifelse(cityAll.stats$model.tree.slope>tree.slope.mean+sig.thresh*tree.slope.sd | cityAll.stats$model.tree.slope<tree.slope.mean-sig.thresh*tree.slope.sd, T, F)
cityAll.stats$Outlier.OtherSlope <- ifelse(cityAll.stats$model.veg.slope>other.slope.mean+sig.thresh*other.slope.sd | cityAll.stats$model.veg.slope<other.slope.mean-sig.thresh*other.slope.sd, T, F)
summary(cityAll.stats)

nCityAll  <- nrow(cityAll.stats)
NoOutliers  <- which(!cityAll.stats$Outlier.TreeSlope & !cityAll.stats$Outlier.OtherSlope)
CityOutliers  <- which(cityAll.stats$Outlier.TreeSlope  | cityAll.stats$Outlier.OtherSlope)
nCityNoOutlier <- length(NoOutliers)

# Trees have a clear, consistent cooling effect on global urban surface temperatures, with a mean effect of -XX˚C per % tree cover across all XXXX cities analyzed and a significant cooling effect in XX% of cities
mean(cityAll.stats$model.tree.slope[NoOutliers]); sd(cityAll.stats$model.tree.slope[NoOutliers])
length(cityAll.stats$model.tree.slope[NoOutliers])

TreeCool <- which(cityAll.stats$model.tree.slope<0 & cityAll.stats$model.tree.p<0.01 & !cityAll.stats$Outlier.TreeSlope & !cityAll.stats$Outlier.OtherSlope)
length(TreeCool)/nCityNoOutlier

# For comparison, non-tree vegetation had a mean effect of -X ˚C per percent cover, with a significant effect in XX % of cities. 
mean(cityAll.stats$model.veg.slope[NoOutliers]); sd(cityAll.stats$model.veg.slope[NoOutliers])

mean(cityAll.stats$model.veg.slope[NoOutliers])/mean(cityAll.stats$model.tree.slope[NoOutliers])

OtherCool <- which(cityAll.stats$model.veg.slope<0 & cityAll.stats$model.veg.p<0.01 & !cityAll.stats$Outlier.TreeSlope & !cityAll.stats$Outlier.OtherSlope)
length(OtherCool)/length(NoOutliers)



# Getting biome-specific breakdowns
VegSlopeBiome <- aggregate(cbind(model.R2adj, model.tree.slope, model.veg.slope) ~ biomeName, data=cityAll.stats[NoOutliers,], FUN=mean)
# names(VegSlopeBiome)[] <- c("Biome", "N.Cities")
VegSlopeBiome[,paste0(c("model.R2adj", "model.tree.slope", "model.veg.slope"), ".SD")] <- aggregate(cbind(model.R2adj, model.tree.slope, model.veg.slope) ~ biomeName, data=cityAll.stats[NoOutliers,], FUN=sd)[,c("model.R2adj", "model.tree.slope", "model.veg.slope")]
VegSlopeBiome$N.Cities <- aggregate(model.R2adj ~ biomeName, data=cityAll.stats[,], FUN=length)[,2]
VegSlopeBiome$N.TreeSigCool <- aggregate(model.tree.slope ~ biomeName, data=cityAll.stats[cityAll.stats$model.tree.slope<0 & cityAll.stats$model.tree.p<0.01 & !cityAll.stats$Outlier.TreeSlope & !cityAll.stats$Outlier.OtherSlope,], FUN=length)[,2]
VegSlopeBiome$N.OtherSigCool <- aggregate(model.veg.slope ~ biomeName, data=cityAll.stats[cityAll.stats$model.veg.slope<0 & cityAll.stats$model.veg.p<0.01 & !cityAll.stats$Outlier.TreeSlope & !cityAll.stats$Outlier.OtherSlope,], FUN=length)[,2]
VegSlopeBiome

tableVegSlopeBiome <- data.frame(Biome=VegSlopeBiome$biomeName, N.Cities=VegSlopeBiome$N.Cities,
                                 modelR2adj=paste0(round(VegSlopeBiome$model.R2adj, 2), " (",round(VegSlopeBiome$model.R2adj.SD, 2), ")"),
                                 TreeSlope=paste0(round(VegSlopeBiome$model.tree.slope, 2), " (",round(VegSlopeBiome$model.tree.slope.SD, 2), ")"),
                                 OtherVegSlope=paste0(round(VegSlopeBiome$model.veg.slope, 2), " (",round(VegSlopeBiome$model.veg.slope.SD, 2), ")"),
                                 pTreeSigCool=paste0(round(VegSlopeBiome$N.TreeSigCool/VegSlopeBiome$N.Cities*100), "%"),
                                 pOtherSigCool=paste0(round(VegSlopeBiome$N.OtherSigCool/VegSlopeBiome$N.Cities*100), "%"))
tableVegSlopeBiome
write.csv(tableVegSlopeBiome, file.path(path.figs, "Vegetation_Slope_Biome.csv"), row.names=F)

slopes.stack <- stack(cityAll.stats[NoOutliers,c("model.tree.slope", "model.veg.slope")])
names(slopes.stack) <- c("model.slope", "vegType")
slopes.stack$vegType <- factor(ifelse(grepl("tree", slopes.stack$vegType), "tree", "other veg"), levels=c("tree", "other veg"))
slopes.stack[,c("ISOURBID", "NAME", "biomeName")] <- cityAll.stats[NoOutliers,c("ISOURBID", "NAME", "biomeName")]
# BuffCoreCompTrend$factor <- factor(BuffCoreCompTrend$factor, levels=c("LST", "tree", "other veg"))


  
png(file.path(path.figs, "Vegetation_Slopes_boxplot.png"), height=8, width=8, units="in", res=220)
ggplot(data=slopes.stack[,]) +
  # facet_grid(factor~.) +
  geom_boxplot(aes(x=biomeName, y=model.slope, fill=biomeName, alpha=vegType), position="dodge") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(name="Model Slopes (deg C / % cover)") +
  scale_x_discrete(name="Biome Type") +
  scale_fill_manual(values=biome.pall.all, guide="none") +
  scale_alpha_manual(values=c("tree"=1, "other veg"=0.4)) +
  labs(caption="4-sigma outliers removed; dark fill = tree slope; light fill= other veg") +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text.x=element_text(angle=-15, hjust=0))
dev.off()


# Estimated cooling effect of trees on a per-percent coverage basis varied among biomes, with the greatest tree cooling potential in arid and semi-arid regions such as grasslands (XX˚C/% cover SD XXX ˚C/% across all grasslands), Mediterranean biomes (-0.18 ˚C/% SD 0.23), and deserts (-0.36˚C/% SD 0.75˚C/%)
cities.grassland <- which(grepl("Grassland", cityAll.stats$biomeName) & !cityAll.stats$Outlier.TreeSlope & !cityAll.stats$Outlier.OtherSlope)
mean(cityAll.stats$model.tree.slope[cities.grassland]); sd(cityAll.stats$model.tree.slope[cities.grassland])


## ----------
#    3.2. Converting slopes into cooling of surface temperature
#         - Slope x mean Tree Cover = estimated contribution to surface temperature
#         - Change in tree cover relative to temporal trend → what would the UHI be without gains or losses in the urban canopy?
## ----------
summary(cityAll.stats[NoOutliers,])
summary(CityBuffStats[NoOutliers,])
cityAll.stats$Outlier <- cityAll.stats$Outlier.TreeSlope | cityAll.stats$Outlier.OtherSlope
cityAll.stats$TreeSlope90 <- cityAll.stats$model.tree.slope>=quantile(cityAll.stats$model.tree.slope, 0.05) & cityAll.stats$model.tree.slope<=quantile(cityAll.stats$model.tree.slope, 0.95)
summary(cityAll.stats)

# # ** IMPORTANT ** Create a new data frame that merges some of the city-level stats with the key differences between cities & buffers
cityTree <- CityBuffStats[CityBuffStats$factor=="tree", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityTree) <- gsub("mean", "tree", names(cityTree))
names(cityTree)[which(names(cityTree)=="trend.p.core")] <- "trend.tree.p.core"

cityOther <- CityBuffStats[CityBuffStats$factor=="other veg", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityOther) <- gsub("mean", "other", names(cityOther))
names(cityOther)[which(names(cityOther)=="trend.p.core")] <- "trend.other.p.core"

cityVeg <- merge(cityTree, cityOther, all=T)
summary(cityVeg)

summary(cityAll.stats[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "model.R2adj", "model.tree.slope", "model.tree.p", "model.veg.slope", "model.veg.p")])
StatsCombined <- merge(cityAll.stats[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "model.R2adj", "model.tree.slope", "model.tree.p", "model.veg.slope", "model.veg.p","Outlier", "TreeSlope90")],
                       cityVeg, all=T)
summary(StatsCombined)

# Oh yeah, we need the LST differences too!
cityLST <- CityBuffStats[CityBuffStats$factor=="LST", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityLST) <- gsub("mean", "LST", names(cityLST))
names(cityLST)[which(names(cityLST)=="trend.p.core")] <- "trend.LST.p.core"

StatsCombined <- merge(StatsCombined, cityLST, all=T)
summary(StatsCombined)


# Calculating the realized effect of trees & other veg in degrees celcsius
StatsCombined$TempContrib.Tree <- StatsCombined$model.tree.slope*StatsCombined$value.tree.core 
StatsCombined$TempContrib.Other <- StatsCombined$model.veg.slope*StatsCombined$value.other.core

# Using the difference in veg cover to determine the estimated contribution to the UHI
StatsCombined$UHIContrib.Tree <- StatsCombined$model.tree.slope*StatsCombined$value.tree.diff
StatsCombined$UHIContrib.Other <- StatsCombined$model.veg.slope*StatsCombined$value.other.diff
StatsCombined$pUHIContrib.Tree <- StatsCombined$UHIContrib.Tree/StatsCombined$value.LST.diff
StatsCombined$pUHIContrib.Other <- StatsCombined$UHIContrib.Other/StatsCombined$value.LST.diff
summary(StatsCombined)


# For the XXXX cities showing urban heat island effects, differences in tree cover account for an average XX% of that, an average of X˚C (SD X˚C). 
cityUHI <- which(StatsCombined$value.LST.diff>0 & !StatsCombined$Outlier)

length(cityUHI)

mean(StatsCombined$pUHIContrib.Tree[cityUHI]); sd(StatsCombined$pUHIContrib.Tree[cityUHI])
mean(StatsCombined$UHIContrib.Tree[cityUHI]); sd(StatsCombined$UHIContrib.Tree[cityUHI])
mean(StatsCombined$UHIContrib.Tree[cityUHI]/StatsCombined$UHIContrib.Other[cityUHI]); sd(StatsCombined$UHIContrib.Tree[cityUHI]/StatsCombined$UHIContrib.Other[cityUHI])

# Despite much higher cover of non-tree vegetation in cities (table), trees contribute an average 6.5 times the cooling on average (non-tree veg cooling: -0.05˚C SD 0.27˚C)
mean(StatsCombined$UHIContrib.Other[cityUHI]); sd(StatsCombined$UHIContrib.Other[cityUHI])
mean(StatsCombined$pUHIContrib.Other[cityUHI]); sd(StatsCombined$pUHIContrib.Other[cityUHI])

# However, because non-tree vegetation cover is higher in metroregion cores relative to the surrounding region (table), non-tree urban green spaces playing a crucial role offsetting the UHI effect with a net impact of -0.05˚C (SD 0.27˚C), or X% of the total observed UHI
mean(StatsCombined$UHIContrib.Other[cityUHI]); sd(StatsCombined$UHIContrib.Other[cityUHI])

# Now Getting the Biome Breakdown
summary(StatsCombined)

VegEffectsBiome <- aggregate(cbind(value.LST.diff, model.R2adj, value.tree.core, value.other.core, model.tree.slope, model.veg.slope, TempContrib.Tree, TempContrib.Other, UHIContrib.Tree, UHIContrib.Other, pUHIContrib.Tree, pUHIContrib.Other) ~ biomeName, data=StatsCombined[!StatsCombined$Outlier,], FUN=mean)
# names(VegSlopeBiome)[] <- c("Biome", "N.Cities")
VegEffectsBiome[,paste0(c("value.LST.diff", "model.R2adj", "value.tree.core", "value.other.core", "model.tree.slope", "model.veg.slope",  "TempContrib.Tree", "TempContrib.Other", "UHIContrib.Tree", "UHIContrib.Other", "pUHIContrib.Tree", "pUHIContrib.Other"), ".SD")] <- aggregate(cbind(value.LST.diff, model.R2adj, value.tree.core, value.other.core, model.tree.slope, model.veg.slope, TempContrib.Tree, TempContrib.Other, UHIContrib.Tree, UHIContrib.Other, pUHIContrib.Tree, pUHIContrib.Other) ~ biomeName, data=StatsCombined[!StatsCombined$Outlier,], FUN=sd)[,c("value.LST.diff", "model.R2adj", "value.tree.core", "value.other.core", "model.tree.slope", "model.veg.slope", "TempContrib.Tree", "TempContrib.Other", "UHIContrib.Tree", "UHIContrib.Other", "pUHIContrib.Tree", "pUHIContrib.Other")]
VegEffectsBiome$N.Cities <- aggregate(value.LST.diff ~ biomeName, data=StatsCombined[NoOutliers,], FUN=length)[,2]
VegEffectsBiome$N.TreeSigCool <- aggregate(model.tree.slope ~ biomeName, data=StatsCombined[!StatsCombined$Outlier & StatsCombined$model.tree.slope<0 & StatsCombined$model.tree.p<0.01,], FUN=length)[,2]
VegEffectsBiome$N.OtherSigCool <- aggregate(model.veg.slope ~ biomeName, data=StatsCombined[!StatsCombined$Outlier & StatsCombined$model.veg.slope<0 & StatsCombined$model.veg.p<0.01,], FUN=length)[,2]
VegEffectsBiome

tableVegEffectBiome <- data.frame(Biome=VegEffectsBiome$biomeName, N.Cities=VegEffectsBiome$N.Cities,
                                  LSTdiff=paste0(round(VegEffectsBiome$value.LST.diff, 2), " (",round(VegEffectsBiome$value.LST.diff, 2), ")"),
                                  modelR2adj=paste0(round(VegEffectsBiome$model.R2adj, 2), " (",round(VegEffectsBiome$model.R2adj.SD, 2), ")"),
                                  TreeSlope=paste0(round(VegEffectsBiome$model.tree.slope, 2), " (",round(VegEffectsBiome$model.tree.slope.SD, 2), ")"),
                                  OtherVegSlope=paste0(round(VegEffectsBiome$model.veg.slope, 2), " (",round(VegEffectsBiome$model.veg.slope.SD, 2), ")"),
                                  TreeEffect.degC=paste0(round(VegEffectsBiome$TempContrib.Tree, 2), " (",round(VegEffectsBiome$TempContrib.Tree.SD, 2), ")"),
                                  OtherEffect.degC=paste0(round(VegEffectsBiome$TempContrib.Other, 2), " (",round(VegEffectsBiome$TempContrib.Other.SD, 2), ")"),
                                  TreeContribUHI=paste0(round(VegEffectsBiome$UHIContrib.Tree, 2), " (",round(VegEffectsBiome$UHIContrib.Tree.SD, 2), ")"),
                                  OtherContribUHI=paste0(round(VegEffectsBiome$UHIContrib.Other, 2), " (",round(VegEffectsBiome$UHIContrib.Other.SD, 2), ")"),
                                  pUHI.Tree=paste0(round(VegEffectsBiome$pUHIContrib.Tree, 2), " (",round(VegEffectsBiome$pUHIContrib.Tree.SD, 2), ")"),
                                  pUHI.Other=paste0(round(VegEffectsBiome$pUHIContrib.Other, 2), " (",round(VegEffectsBiome$pUHIContrib.Other.SD, 2), ")"))
tableVegEffectBiome
write.csv(tableVegEffectBiome, file.path(path.figs, "Vegetation_Effects-All_Biome.csv"), row.names=F)


# Across all biomes, trees cool cities an average of X˚C (SD X˚C), although there is large variation among biomes based on both regional variation in tree cooling potential (model slopes) and tree cover in the metroregion core (figure, table)
mean(StatsCombined$TempContrib.Tree[!StatsCombined$Outlier]); sd(StatsCombined$TempContrib.Tree[!StatsCombined$Outlier])



effects.stack <- stack(StatsCombined[,c("TempContrib.Tree", "TempContrib.Other")])
names(effects.stack) <- c("TempContrib", "vegType")
effects.stack$vegType <- factor(ifelse(grepl("Tree", effects.stack$vegType), "tree", "other veg"), levels=c("tree", "other veg"))
effects.stack[,c("ISOURBID", "NAME", "biomeName", "Outlier")] <- StatsCombined[,c("ISOURBID", "NAME", "biomeName", "Outlier")]
summary(effects.stack)


png(file.path(path.figs, "Vegetation_TempEffects_boxplot.png"), height=8, width=8, units="in", res=220)
ggplot(data=effects.stack[!effects.stack$Outlier,]) +
  # facet_grid(factor~.) +
  geom_boxplot(aes(x=biomeName, y=TempContrib, fill=biomeName, alpha=vegType), position="dodge") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(name="Veg Effect on Temp (deg C)") +
  scale_x_discrete(name="Biome Type") +
  scale_fill_manual(values=biome.pall.all, guide="none") +
  scale_alpha_manual(values=c("tree"=1, "other veg"=0.4)) +
  labs(caption="4-sigma outliers removed; dark fill = tree slope; light fill= other veg") +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text.x=element_text(angle=-15, hjust=0))
dev.off()


summary(StatsCombined)
StatsCombined$TempContrib.NoVeg <- StatsCombined$value.LST.diff - StatsCombined$TempContrib.Tree - StatsCombined$TempContrib.Other
StatsCombined$UHIContrib.NoVeg <- StatsCombined$value.LST.diff - StatsCombined$UHIContrib.Tree - StatsCombined$UHIContrib.Other

summary(StatsCombined)
summary(StatsCombined[!StatsCombined$Outlier,])

TempEstimates <- stack(StatsCombined[,c("value.LST.diff", "TempContrib.Tree", "TempContrib.Other", "TempContrib.NoVeg")])
names(TempEstimates) <- c("TempContrib", "ind")
TempEstimates$UHIContrib <- stack(StatsCombined[,c("value.LST.diff", "UHIContrib.Tree", "UHIContrib.Other", "UHIContrib.NoVeg")])[,1]
TempEstimates$ind <- car::recode(TempEstimates$ind, "'value.LST.diff'='UHI (Observed)'; 'TempContrib.Tree'='Tree Effect'; 'TempContrib.Other'='Other Veg Effect'; 'TempContrib.NoVeg'='No Veg Temp (Estimated)'")
TempEstimates[,c("ISOURBID", "NAME", "biomeName", "Outlier")] <- StatsCombined[,c("ISOURBID", "NAME", "biomeName", "Outlier")]
summary(TempEstimates)




png(file.path(path.figs, "LST_Contributions_densityplot.png"), height=8, width=8, units="in", res=220)
ggplot(data=TempEstimates) +
  # facet_grid(ind~.) +
  geom_density(aes(x=TempContrib, fill=ind), alpha=0.5) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Effect on LST (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="Proportion of Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("UHI (Observed)"="black", "Tree Effect"="#1b9e77", "Other Veg Effect"="#7570b3", "No Veg Temp (Estimated)"="#d95f02")) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "LST_Contributions-Biomes_densityplot.png"), height=8, width=11, units="in", res=220)
ggplot(data=TempEstimates) +
  facet_wrap(~biomeName, scales="free") +
  geom_density(aes(x=TempContrib, fill=ind), alpha=0.5) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Effect on LST (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="Proportion of Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("UHI (Observed)"="black", "Tree Effect"="#1b9e77", "Other Veg Effect"="#7570b3", "No Veg Temp (Estimated)"="#d95f02")) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


png(file.path(path.figs, "UHI_Contributions_densityplot.png"), height=8, width=8, units="in", res=220)
ggplot(data=TempEstimates[!TempEstimates$Outlier,]) +
  # facet_grid(ind~.) +
  coord_cartesian(xlim=quantile(TempEstimates$UHIContrib[!TempEstimates$Outlier], c(0.005, 0.995), na.rm = T)) +
  geom_density(aes(x=UHIContrib, fill=ind), alpha=0.5) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="UHI Contribution (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="Proportion of Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("UHI (Observed)"="black", "Tree Effect"="#1b9e77", "Other Veg Effect"="#7570b3", "No Veg Effect (Estimated)"="#d95f02")) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


# Looking at how much higher tree cover would need to be to offset the UHI by trees alone
# Because of 

StatsCombined$TempDeficit.Trees <- StatsCombined$value.LST.diff/StatsCombined$model.tree.slope
StatsCombined$TempDeficit.Trees.Trim <- StatsCombined$TempDeficit.Trees
StatsCombined$TempDeficit.Trees.Trim[!StatsCombined$TreeSlope90] <- NA
summary(StatsCombined)
length(which(!is.na(StatsCombined$TempDeficit.Trees.Trim)))
mean(StatsCombined$TempDeficit.Trees.Trim, na.rm=T); sd(StatsCombined$TempDeficit.Trees.Trim, na.rm=T)

# Setting a Tree-based UHI Offset Goal
StatsCombined$TreeTempOffsetGoal <- StatsCombined$value.tree.core - StatsCombined$TempDeficit.Trees
StatsCombined$TreeTempOffsetGoal.Trimmed <- StatsCombined$TreeTempOffsetGoal
StatsCombined$TreeTempOffsetGoal.Trimmed[!StatsCombined$TreeSlope90] <- NA

# StatsCombined$Ratio.TreeTempOffsetGoal <- StatsCombined$TreeTempOffsetGoal/StatsCombined$value.tree.core
# StatsCombined$Ratio.TreeTempOffsetGoal[StatsCombined$Ratio.TreeTempOffsetGoal==Inf] <- NA
# summary(StatsCombined)

# For trees to be the sole nature-based solution for offsetting UHI effects, globally tree cover would need to be an average XX % higher (SD XX%), a near doubling of current average tree cover, when analyzing the middle 90% of cities.
mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T); sd(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)
mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)/mean(StatsCombined$value.tree.core, na.rm=T)
median(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)/median(StatsCombined$value.tree.core, na.rm=T)

length(which(StatsCombined$value.tree.core>=mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)))
length(which(StatsCombined$value.tree.core>=mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)))/nrow(StatsCombined)


mean(StatsCombined$value.tree.core[!is.na(StatsCombined$TreeTempOffsetGoal.Trimmed)], na.rm=T); sd(StatsCombined$value.tree.core[!is.na(StatsCombined$TreeTempOffsetGoal.Trimmed)], na.rm=T)
mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T); sd(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)



TreeDeficitBiome <- aggregate(cbind(TreeTempOffsetGoal.Trimmed, TempDeficit.Trees.Trim) ~ biomeName, data=StatsCombined[,], FUN=mean, na.rm=T)
# # names(VegSlopeBiome)[] <- c("Biome", "N.Cities")
TreeDeficitBiome[,paste0(c("TreeTempOffsetGoal.Trimmed", "TempDeficit.Trees.Trim"), ".SD")] <- aggregate(cbind(TreeTempOffsetGoal.Trimmed, TempDeficit.Trees.Trim) ~ biomeName, data=StatsCombined[,], FUN=sd, na.rm=T)[,c("TreeTempOffsetGoal.Trimmed", "TempDeficit.Trees.Trim")]
TreeDeficitBiome$N.OffsetGoal <- aggregate(TreeTempOffsetGoal.Trimmed ~ biomeName, data=StatsCombined[!is.na(StatsCombined$TreeTempOffsetGoal.Trimmed),], FUN=length)[,c("TreeTempOffsetGoal.Trimmed")]
TreeDeficitBiome$N.Deficit <- aggregate(TempDeficit.Trees.Trim ~ biomeName, data=StatsCombined[!is.na(StatsCombined$TreeTempOffsetGoal.Trimmed),], FUN=length)[,c("TempDeficit.Trees.Trim")]
TreeDeficitBiome

tableTreeDeficitBiome <- data.frame(Biome=TreeDeficitBiome$biomeName, 
                                    N.Cities.Offset=TreeDeficitBiome$N.OffsetGoal,
                                    N.Cities.Deficit=TreeDeficitBiome$N.Deficit,
                                    TreeOffsetGoal=paste0(round(TreeDeficitBiome$TreeTempOffsetGoal.Trimmed, 0), " (",round(TreeDeficitBiome$TreeTempOffsetGoal.Trimmed.SD, 0), ")"),
                                    TreeGoalDeficit=paste0(round(TreeDeficitBiome$TempDeficit.Trees.Trim, 0), " (",round(TreeDeficitBiome$TempDeficit.Trees.Trim.SD, 0), ")"))
tableTreeDeficitBiome
write.csv(tableTreeDeficitBiome, file.path(path.figs, "UHI-Offset_GoalsDeficts_Biome.csv"), row.names=F)




png(file.path(path.figs, "UHI-Offset_TreeGoalTotal_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=StatsCombined[,]) +
  geom_histogram(aes(x=TreeTempOffsetGoal.Trimmed, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Trees Needed to Offset UHI (%)", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(caption="**Warming Cities Only*; Display shown is 2% trimmed") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()



png(file.path(path.figs, "UHI-Offset_TreeDeficit_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=StatsCombined[,]) +
  geom_histogram(aes(x=TempDeficit.Trees.Trim, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Trees Deficit to Offset UHI (%)", expand=c(0,0)) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(caption="**Warming Cities Only*; Display shown is 2% trimmed") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "UHI-Offset_TreeDeficit_map.png"), height=8, width=8, units="in", res=220)
ggplot(data=StatsCombined[,]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=TempDeficit.Trees.Trim), size=0.5) +
  scale_color_gradientn(name="Tree UHI\nOffset Deficit\n(%)", colors=grad.tree,  limits=c(-max(abs(StatsCombined$TempDeficit.Trees.Trim), na.rm=T), max(abs(StatsCombined$TempDeficit.Trees.Trim), na.rm=T))) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
dev.off()


ggplot(data=StatsCombined[which(StatsCombined$value.tree.core>=mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)),]) +
  coord_equal(expand=0, ylim=c(-60,75)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color="Above 32% Tree Cover"), size=0.5) +
  scale_color_manual(values="green3") +
  # scale_color_gradientn(name="Tree UHI\nOffset Deficit\n(%)", colors=grad.tree,  limits=c(-max(abs(StatsCombined$TempDeficit.Trees.Trim), na.rm=T), max(abs(StatsCombined$TempDeficit.Trees.Trim), na.rm=T))) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())

summary(StatsCombined[which(StatsCombined$value.tree.core>=mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)),])

StatsCombined[which(StatsCombined$value.tree.core>=mean(StatsCombined$TreeTempOffsetGoal.Trimmed, na.rm=T)),c("ISOURBID", "NAME", "biomeName", "value.tree.core")]
StatsCombined[StatsCombined$model.R2adj<0.5, c("ISOURBID", "NAME", "biomeName", "value.tree.core", "model.R2adj", "model.tree.slope", "TreeSlope90")]

summary(StatsCombined[StatsCombined$model.R2adj<0.5,])
## ----------
#    3.3. Comparing cooling & tree trends to global warming and other urban warming processes
## ----------
# Figure out how much the LST would change if there wasn't the widespread gains (or losses) we saw in tree cover 
StatsCombined$trend.LST.core.tree <- StatsCombined$trend.tree.core*StatsCombined$model.tree.slope # Tree Trend Effect on temperature (deg C/yr)
StatsCombined$trend.LST.core.StatTree <- StatsCombined$trend.LST.core - StatsCombined$trend.LST.core.tree # What the LST trend would be without changes in tree cover (Deg C/yr)
StatsCombined$pTrendAdd.Tree <- StatsCombined$trend.LST.core.StatTree/StatsCombined$trend.LST.core -1 # proporitional LST trend if there was no change in cover
StatsCombined$trend.LST.core.other <- StatsCombined$trend.other.core*StatsCombined$model.veg.slope
StatsCombined$trend.LST.core.StatOther <- StatsCombined$trend.LST.core - StatsCombined$trend.LST.core.other
StatsCombined$pTrendAdd.Other <- StatsCombined$trend.LST.core.StatOther/StatsCombined$trend.LST.core -1 # How much worse would warming be without tree gains 
summary(StatsCombined)

summary(StatsCombined$pTrendAdd.Tree[!StatsCombined$Outlier]*100)
quantile(StatsCombined$pTrendAdd.Tree[!StatsCombined$Outlier]*100, c(0.05, 0.95), na.rm=T)

# ggplot(data=TempEstimates) +
#   facet_wrap(~biomeName, scales="free") +
#   geom_density(aes(x=TempContrib, fill=ind), alpha=0.5) +
#   geom_vline(xintercept=0, linetype="dashed") +
#   scale_x_continuous(name="Effect on LST (deg. C)", expand=c(0,0)) +
#   scale_y_continuous(name="Proportion of Cities", expand=c(0,0)) +
#   scale_fill_manual(values=c("UHI (Observed)"="black", "Tree Effect"="#1b9e77", "Other Veg Effect"="#7570b3", "No Veg Temp (Estimated)"="#d95f02")) +
#   theme_bw() +
#   theme(legend.position="top",
#         legend.title=element_blank(),
#         legend.text=element_text(color="black"),
#         legend.background=element_blank(),
#         panel.background = element_rect(fill="NA"),
#         panel.grid = element_blank())


# On average, changes in tree cover from 2000-2020 contribute a global mean cooling effect of XXX˚C/yr (SD XX˚C/yr) in urban areas, which is equivalent to an average XX˚C over the 20-year analysis period. 
mean(StatsCombined$trend.LST.core.tree[!StatsCombined$Outlier], na.rm=T); sd(StatsCombined$trend.LST.core.tree[!StatsCombined$Outlier], na.rm=T)
mean(StatsCombined$trend.LST.core.tree[!StatsCombined$Outlier], na.rm=T)*20; sd(StatsCombined$trend.LST.core.tree[!StatsCombined$Outlier], na.rm=T)*20


# While significant and the opposite pattern than seen from changes in other vegetation cover (XX˚C/yr SD XX˚C/yr), cooling from increased tree cover only accounts for a median 1% of the observed urban land surface temperature trend.  
mean(StatsCombined$trend.LST.core.other[!StatsCombined$Outlier], na.rm=T); sd(StatsCombined$trend.LST.core.other[!StatsCombined$Outlier], na.rm=T)

median(StatsCombined$pTrendAdd.Tree[!StatsCombined$Outlier], na.rm=T)
mean(StatsCombined$pTrendAdd.Tree[!StatsCombined$Outlier], na.rm=T)

# In order to have offset the recent warming observed in cities, tree cover would have needed to increase at a median rate of 0.4%/yr – more than 6 times the median rate of tree cover increase detected in our data set.
# LST Trend (deg C / year) / tree effect (deg C / %) = deg C/yr * %/deg C??
StatsCombined$TreeTrendToOffsetLST <- StatsCombined$trend.LST.core/-StatsCombined$model.tree.slope

StatsCombined$TreeTrendRatio <- StatsCombined$TreeTrendToOffsetLST/-StatsCombined$trend.tree.core
summary(StatsCombined)
summary(StatsCombined[!StatsCombined$Outlier,])

mean(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], na.rm=T); sd(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], na.rm=T)
median(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], na.rm=T)

median(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], na.rm=T)/median(StatsCombined$trend.tree.core[!StatsCombined$Outlier], na.rm=T)

summary(StatsCombined$TreeTrendToOffsetLST)
summary(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier])

quantile(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], c(0.005, 0.995), na.rm=T)
summary(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], c(0.005, 0.995), na.rm=T)

# png(file.path(path.figs, "Vegetation-Tree_Trends_Difference_histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=StatsCombined[!StatsCombined$Outlier,]) +
  # coord_cartesian(xlim=quantile(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], c(0.005, 0.995), na.rm=T)) +
  geom_histogram(aes(x=TreeTrendToOffsetLST, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Tree Trend Needed to Offset LST Trend (%/yr)", expand=c(0,0), limits=quantile(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], c(0.01, 0.99), na.rm=T)) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(caption="Display shown is 2% trimmed") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())
# dev.off()

ggplot(data=StatsCombined[!StatsCombined$Outlier & StatsCombined$trend.LST.core>0,]) +
  geom_histogram(aes(x=TreeTrendToOffsetLST, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Tree Trend Needed to Offset LST Trend (%/yr)", expand=c(0,0), limits=quantile(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], c(0.01, 0.99), na.rm=T)) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(caption="**Warming Cities Only*; Display shown is 2% trimmed") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())


summary(StatsCombined$TreeTrendToOffsetLST - StatsCombined$trend.tree.core)

# looking at tree trend deficits
mean(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier] - StatsCombined$trend.tree.core[!StatsCombined$Outlier], na.rm=T); sd(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier] - StatsCombined$trend.tree.core[!StatsCombined$Outlier], na.rm=T)
median(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier] - StatsCombined$trend.tree.core[!StatsCombined$Outlier], na.rm=T)

ggplot(data=StatsCombined[!StatsCombined$Outlier,]) +
  geom_histogram(aes(x=TreeTrendToOffsetLST-trend.tree.core, fill=biomeName)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(name="Tree Deficit (%/yr)", expand=c(0,0), limits=quantile(StatsCombined$TreeTrendToOffsetLST[!StatsCombined$Outlier], c(0.01, 0.99), na.rm=T)) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(caption="**Warming Cities Only*; Display shown is 2% trimmed") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())

##########################################



##########################################
# 4. Value of Trees: Direct comparison of trees compared to other vegetation
# ---------
#    Key Result/Message: Trees really are far more effective than other vegetation types in cooling cities
#   [may not need more analysis??]
##########################################


##########################################
# 5. Critical role of trees as green infrastructure to mitigate the effects of climate change [probably no analyses needed] ----
# ---------
#.   Key Message: Our results show trees playing a key role in cooling cities in every biome on earth
##########################################
