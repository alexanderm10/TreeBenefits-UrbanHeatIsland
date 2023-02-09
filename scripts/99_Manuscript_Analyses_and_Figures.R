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
google.user <- dir("~/Library/CloudStorage/")

path.cities <- file.path("~/Library/CloudStorage/", google.user, "Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis/data_processed_final")

path.figs <- file.path(path.cities, "figures", "Manuscript Options")
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
##########################################

# ##########################################
# Read in base datasets ----
# ##########################################
# Regional Sumary Stuff ----
cityAll.stats <- read.csv(file.path(path.cities, "city_stats_all.csv"))
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
CityBuffStats$factor <- as.factor(CityBuffStats$factor)
summary(CityBuffStats)

# MErging some of the useful city metadata with the BuffStats file
CityBuffStats <- merge(CityBuffStats, cityAll.stats[,c("ISOURBID", "NAME", "LONGITUDE", "LATITUDE", "biomeName", "ES00POP")], all.x=T)

CityBuffStats[CityBuffStats$value.mean.core==0,] # This is for tree cover, so VERY very low.
summary(CityBuffStats[CityBuffStats$value.mean.core<1,]) # This is for tree cover, so VERY very low.

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
  annotate(geom="text", x=1, y=5, label="dark fill = urban core; light fill= 10 km buffer", hjust=0) +
  theme_bw() +
  theme(legend.position="none",
        legend.title=element_text(color="black", face="bold", size=rel(1.5)),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text.x=element_text(angle=-15, hjust=0))
dev.off()

# Creating a summary Tabls
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


##########################################
# 3. *Attribution* of spatio-temporal variation in temperature to vegetation and particularly trees ----
# ---------
#    Key Result/Message: On average, XX% of the urban heat island effect is attributable to differences in tree cover between urban areas and the surrounding buffer.
#    3.1: Effects of temperature on surface temperature (degrees C per percent cover stats)
#         - Present model slopes (˚C/% cover)
#    3.2. Converting slopes into cooling of surface temperature
#         - Slope x mean Tree Cover = estimated contribution to surface temperature
#         - Change in tree cover relative to temporal trend → what would the UHI be without gains or losses in the urban canopy?
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
