# Script to synthesize the results from all of the individual city models ----
library(ggplot2); library(RColorBrewer); library(cowplot)
# path.figs <- "../figures/v6_vegonly"


###########################################
# Establish file paths etc ----
###########################################
path.cities <- "/Volumes/GoogleDrive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis/data_processed_final"
file.cityAll.stats <- file.path(path.cities, "city_stats_all.csv")

path.figs <- file.path(path.cities, "figures")
dir.create(path.figs, recursive=T, showWarnings=F)


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
# Read in Data; do some cleanup ----
# ##########################################
cityAll.stats <- read.csv(file.cityAll.stats)
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

cityAll.stats$biomeClim[grepl("tropical", cityAll.stats$biome) | grepl("flooded", cityAll.stats$biome) | grepl("mangroves", cityAll.stats$biome)] <- "Tropical/Subtropical"
cityAll.stats$biomeClim[grepl("temperate", cityAll.stats$biome)] <- "Temperate"
cityAll.stats$biomeClim[grepl("xeric", cityAll.stats$biome) | grepl("mediterranean", cityAll.stats$biome)] <- "Dry"
cityAll.stats$biomeClim[grepl("taiga", cityAll.stats$biome) | grepl("tundra", cityAll.stats$biome) | grepl("montane", cityAll.stats$biome)] <- "Polar/Montane"
summary(as.factor(cityAll.stats$biomeClim))

cityAll.stats$biomeVeg[grepl("forest", cityAll.stats$biome) | grepl("mangrove", cityAll.stats$biome)] <- "Forest"
cityAll.stats$biomeVeg[grepl("grassland", cityAll.stats$biome)] <- "Grassland/Savanna"
cityAll.stats$biomeVeg[grepl("shrub", cityAll.stats$biome) | grepl("tundra", cityAll.stats$biome) | grepl("mediterranean", cityAll.stats$biome)] <- "Shrubland"
summary(as.factor(cityAll.stats$biomeVeg))
# unique(cityAll.stats$ISO3)

biome.order <- aggregate(LST.mean ~ biomeName, data=cityAll.stats, FUN=mean)
biome.order <- biome.order[order(biome.order$LST.mean),]

cityAll.stats$biomeName <- factor(cityAll.stats$biomeName, levels=biome.order$biomeName)

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


# ##########################################
# Exploratory of raw output ----
# ##########################################

# ##########################
# Start with looking at some patterns of overall model fit (R2adj) ----
# ##########################
summary(cityAll.stats$model.R2adj)
hist(cityAll.stats$model.R2adj)

# don't save it, but do a quick map of model performance
# # The models with weird tree slopes are not the ones with low R2
# ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),])+
#   geom_point(aes(x=model.R2adj, y=model.tree.slope, color=biomeName)) +
#   scale_color_manual(name="biome", values=biome.pall.all) +
#   theme_bw()

mod.r2.biome <- lm(model.R2adj ~ biomeName-1, data=cityAll.stats)
anova(mod.r2.biome)
summary(mod.r2.biome)

r2.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=model.R2adj), size=0.5) +
  # scale_color_manual(name="Tree Effect\n(deg. C / % cover)", values=colors.cut) +
  # scale_color_gradient2(name="Tree Effect\n(deg. C / % cover)", low = "dodgerblue2", high = "red3", mid = "white", midpoint =0) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

png(file.path(path.figs, "ModelFit_R2adj_Map.png"), height=6, width=12, units="in", res=220)
r2.map
dev.off()

r2.histo.biome <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),])+
  geom_histogram(aes(x=model.R2adj, fill=biomeName)) +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  theme_bw() +
  theme(legend.position="right",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(size=rel(0.8), color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank())


png(file.path(path.figs, "ModelFit_R2adj_histogram.png"), height=8, width=8, units="in", res=220)
plot_grid(r2.map, r2.histo.biome, ncol=1, rel_heights = c(0.6, 0.4))
dev.off()

# ##########################

hist(cityAll.stats$model.elev.slope)
summary(cityAll.stats$model.elev.slope)
nrow(cityAll.stats)
# ##########################
# Now looking at the model slopes for trees ----
# ##########################
summary(cityAll.stats$model.tree.slope)
cityAll.stats$tree.slope.cut <- cut(cityAll.stats$model.tree.slope, breaks=c(-Inf, -1, -0.5, -0.25, -0.1, -0.05, -0.025, 0, 0.025, 0.5, Inf))
summary(cityAll.stats$tree.slope.cut)
colors.cut <- c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#eff3ff", "#fee0d2", "#fc9272", "#de2d26")


cooling.map <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.slope.cut), size=0.5) +
  scale_color_manual(name="Tree Effect\n(deg. C / % cover)", values=colors.cut) +
  # scale_color_gradient2(name="Tree Effect\n(deg. C / % cover)", low = "dodgerblue2", high = "red3", mid = "white", midpoint =0) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        legend.background=element_blank(),
        panel.background = element_rect(fill="NA"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.margin=margin(0,0.5,0,1, "lines"))

TreeCoolingLat <- ggplot(data=cityAll.stats,) +
  coord_flip(xlim=c(-65,80)) +
  # coord_cartesian(, ylim=c(-65,80))
  # geom_point(aes(x=LATITUDE, y=exp(model.tree.slope), color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  stat_smooth(aes(x=LATITUDE, y=model.tree.slope), color="black") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(y="Tree Effect\n(deg C / % cover)", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        plot.margin=margin(8,1, 1.5, 0.5, "lines"))



png(file.path(path.figs, "TreeCooling_PercentEffect_LatitudeCombo.png"), height=6, width=12, units="in", res=220)
plot_grid(cooling.map, TreeCoolingLat, nrow=1, rel_widths = c(0.8, 0.2))
dev.off()

ggplot(data=cityAll.stats,) +
  coord_flip(xlim=c(-65,80)) +
  # coord_cartesian(, ylim=c(-65,80))
  geom_histogram(aes(x=LATITUDE)) +
  geom_hline(yintercept=0, linetype="dashed") +
  # geom_vline(xintercept=0, linetype="dashed", size=0.5, color="red") +
  # stat_smooth(aes(x=LATITUDE, y=model.tree.slope), color="black") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_fill_manual(values=biome.pall.all[]) +
  labs(y="Number Cities", x="Latitude") +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        plot.margin=margin(8,1, 1.5, 0.5, "lines"))


# TreeSlope.breaks <- c(-1, -0.5, -0.25, -0.1, -0.05, -0.025, 0, 0.025, 0.5)
# TreeCoolingBiomeHist <- 
ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  # geom_histogram(aes(x=model.tree.slope, fill=biomeName)) +
  geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  geom_vline(xintercept=7.5, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  # scale_x_continuous(breaks=TreeSlope.breaks)+
  labs(x="Tree Effect (deg. C / % cover)") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

cityAll.stats$model.tree.slope2 <- cityAll.stats$model.tree.slope
cityAll.stats$model.tree.slope2[cityAll.stats$model.tree.slope2 < -1] <- -1
cityAll.stats$model.tree.slope2[cityAll.stats$model.tree.slope2 >0.25] <- 0.25

TreeEffectBiomeHisto <- ggplot(data=cityAll.stats[!is.na(cityAll.stats$biome),]) +
  geom_histogram(aes(x=model.tree.slope2, fill=biomeName), breaks=seq(-1.1,0.3, by=0.05)) +
  geom_vline(xintercept=0,linetype="dashed") +
  # geom_bar(aes(x=tree.slope.cut, fill=biomeName), stat="count") +
  # geom_vline(xintercept=7.5, linetype="dashed") +
  scale_fill_manual(name="biome", values=biome.pall.all) +
  scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="Tree Effect (deg. C / % cover)") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

png(file.path(path.figs, "TreeEffect_Histogram_Biome.png"), height=6, width=10, units="in", res=220)
TreeEffectBiomeHisto
dev.off()

ggplot(data=cityAll.stats[!is.na(cityAll.stats$model.R2adj),]) +
  facet_wrap(~biomeName, scales="free", ncol=3) +
  geom_density(aes(x=model.elev.slope, fill="Elev Effect"), adjust=2, alpha=0.5) +
  geom_density(aes(x=model.veg.slope, fill="Other Veg Effect"), adjust=2, alpha=0.5) +
  geom_density(aes(x=model.tree.slope, fill="Tree Effect"), adjust=2, alpha=0.5) +
  geom_vline(xintercept=0,linetype="dashed") +
  scale_fill_manual(values=c("Tree Effect" = "darkgreen", "Other Veg Effect"="darkgoldenrod", "Elev Effect" = "gray20")) +
  # scale_x_continuous(breaks=c(-1.025, seq(-0.75, 0, by=0.25), 0.225), labels=c("<= -1", seq(-0.75, 0, by=0.25), ">= 0.25"))+
  labs(x="LST Effect: deg. C per (% cover or meter)") +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold"),
        legend.text=element_text(color="black"),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))


# Quick summary of places with significant tree cover trends -- most places (so far) have been INCREASING in cover, but also still getting warmer on average; how much would these cities have warmed without their trees??  
summary(cityAll.stats[!is.na(cityAll.stats$trend.tree.slope) & cityAll.stats$trend.tree.p<0.01,])
summary(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.p<0.01,])
summary(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.p>0.01,])
nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.p<0.01,]);
nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.p>0.01,])

nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope<0,]);
nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope>0,])

nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope<0 & cityAll.stats$model.tree.p<0.01,]);
nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope<0 & cityAll.stats$model.tree.p<0.01,])/nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope),])


# Cities with significant warming effect of trees
summary(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope>0.0,])

nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope>0 & cityAll.stats$model.tree.p<0.01,])
nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope>0 & cityAll.stats$model.tree.p<0.01,])/nrow(cityAll.stats[!is.na(cityAll.stats$model.tree.slope),])

# There are a couple in the US --> lets find those 
cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope>0 & cityAll.stats$model.tree.p<0.01 & cityAll.stats$ISO3=="USA",]
cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope>0.5 & cityAll.stats$model.tree.p<0.01 ,]
cityAll.stats[cityAll.stats$ISO3=="USA" & cityAll.stats$NAME=="Chicago",]

cityAll.stats[!is.na(cityAll.stats$model.tree.slope) & cityAll.stats$model.tree.slope< -2.5 & cityAll.stats$model.tree.p<0.01,]



ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=tree.sd, y=model.tree.slope))
ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=veg.mean, y=model.tree.slope))
ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=100-(veg.mean + tree.mean), y=model.tree.slope))
ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=100-(veg.mean + tree.mean), y=model.R2adj))


ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=trend.tree.slope, y=model.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=trend.veg.slope, y=model.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=(trend.veg.slope-trend.tree.slope)/trend.veg.slope, y=exp(model.tree.slope)))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=trend.LST.slope, y=model.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=tree.veg.trend, y=model.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=veg.mean/tree.mean, y=model.tree.slope))

ggplot(data=cityAll.stats[,]) +
  geom_point(aes(x=model.tree.slope, y=model.veg.slope))


# Looking at the tree slope by biome & by MST
TreeSlope.lm.biome <- lm(model.tree.slope ~ biome-1, data=cityAll.stats[!is.na(cityAll.stats$model.tree.slope),])
anova(TreeSlope.lm.biome)
summary(TreeSlope.lm.biome)

TreeSlope.lm.mst <- lm(model.tree.slope ~ biome-1, data=cityAll.stats[!is.na(cityAll.stats$model.tree.slope),])
anova(TreeSlope.lm.mst)
summary(TreeSlope.lm.mst)


ggplot(data=cityAll.stats[!is.na(cityAll.stats$model.R2adj),]) +
  geom_histogram(aes(x=model.tree.slope, fill=biomeName, y=log(stat(count)))) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_alpha_manual(values=c(0.4,1)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

# Tree Effect as a funciton of LST
ggplot(data=cityAll.stats[!is.na(cityAll.stats$model.R2adj) & cityAll.stats$model.tree.p<0.01,]) +
  geom_point(aes(x = LST.mean, y=model.tree.slope, color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

# Tree Effect as a funciton of mean tree cover -- where do we get diminishing returns?
ggplot(data=cityAll.stats[!is.na(cityAll.stats$model.R2adj) & cityAll.stats$model.tree.p<0.01,]) +
  coord_cartesian(ylim=c(-2.5, 0.5)) +
  geom_point(aes(x = tree.mean, y=model.tree.slope, color=biomeName)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

ggplot(data=cityAll.stats[!is.na(cityAll.stats$model.R2adj) & cityAll.stats$model.tree.p<0.01,]) +
  coord_cartesian(ylim=c(0, 1.5)) +
  geom_point(aes(x = tree.mean, y=exp(model.tree.slope), color=biomeName), alpha=0.5) +
  geom_hline(yintercept=exp(0), linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))

# Ross had the suggestion of plotting the best fit lines for each biome, but to do that, we'll need to compute the trends by hand 
biome.npts <- aggregate(model.tree.slope ~ biomeName, data=cityAll.stats, FUN = length)

ggplot(data=cityAll.stats[!is.na(cityAll.stats$model.R2adj) & cityAll.stats$biomeName %in% biome.npts$biomeName[biome.npts$model.tree.slope>20],]) +
  coord_cartesian(xlim=c(0, 20)) +
  # geom_point(aes(x = tree.mean, y=model.tree.slope, color=biomeName), size=0.1) +
  stat_smooth(aes(x = tree.mean, y=model.tree.slope, color=biomeName, fill=biomeName), alpha=0.3) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=biome.pall.all[]) +
  scale_fill_manual(values=biome.pall.all[]) +
  scale_y_continuous(expand=c(0,0)) +
  # guides(fill="none") +
  theme_bw()+
  theme(legend.title=element_blank(),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"))



cityAll.stats[cityAll.stats$model.tree.slope< -2.5 & !is.na(cityAll.stats$model.tree.slope),]

# ##########################




 









####################################################
####################################################
# Everything that follows is OLD and has not been updated/incorporated yet
####################################################
####################################################











# Trying to find Buenos Aires as an example to post in Slack
# cityAll.stats[grep("AGO", cityAll.stats$ISOURBID),]
cityAll.stats[grep("ARG", cityAll.stats$ISOURBID),c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP", "biome", "n.pixels", "model.R2adj", "model.tree.slope", "model.veg.slope")]
cityAll.stats[grep("AUS", cityAll.stats$ISOURBID),c("ISOURBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP", "biome", "n.pixels", "model.R2adj", "model.tree.slope", "model.veg.slope")]
usa <- cityAll.stats[grep("USA", cityAll.stats$ISOURBID),]
usa[order(usa$ES00POP, decreasing=T),c("ISOURBID", "NAME", "ES00POP", "biome", "n.pixels")]
# Useful Cities to know
# Buenos Aires = "ARG66611"
# Sydney = AUS66430


# Forest: Dark Greens (tropical); Dark Reds (Temperate); Dark Blue (Boreal)
# Grasslands: Yellows
# Dry: Oranges




biomes.all.df <- data.frame(biome=names(biome.pall.all), color=biome.pall.all)
ggplot(data=biomes.all.df) +
  geom_bar(aes(x=biome, fill=biome)) +
  scale_fill_manual(values=biome.pall.all) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() 



biome.pall = data.frame(biome=c("Desert", "Grassland/Savanna", "Mediterranean", "Tropical Forest", "Temperate Forest", "Boreal"),
                        color=c("#D55E00", "#E69F00", "#CC79A7", "#009E73", "#56B4E9", "#0072B2"),
                        color2=c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"))

cat.3 <- data.frame(cover=c("tree", "other", "non-veg"),
                    color=c("#1b9e77", "#7570b3", "#d95f02"),
                    color2=c("#66c2a5", "#8da0cb", "#fc8d62"),
                    color3=c("#018571", "#4dac26", "#e66101"),
                    color4=c("#80cdc1", "#4dac26", "#fbd863"),
                    color5=c("#1b9e77", "#4dac26", "#d95f02"),
                    color6=c("#1b9e77", "#b2df8a", "#a6611a"))
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange

# --------------------------------------------------------------
# Look at output
# --------------------------------------------------------------
# dat.uhi <- read.csv("../data_processed/analysis_cities_summary_sdei_v6.csv")
# dat.filter <- dat.uhi$prop.missing<0.33 & dat.uhi$prop.temp.n.lo<0.33
# 
# # Fix some biome mis-specifications
# dat.uhi$WWF_ECO <- as.character(dat.uhi$WWF_ECO)
# dat.uhi$WWF_BIOME <- as.character(dat.uhi$WWF_BIOME)
# dat.uhi[is.na(dat.uhi$WWF_BIOME),]
# dat.uhi[!is.na(dat.uhi$WWF_BIOME) & dat.uhi$WWF_BIOME=="98",]


# Lumping the biomes a bit more to make easier to see figures
dat.uhi$Biome2 <- car::recode(dat.uhi$WWF_BIOME, 
                              "'mangroves'='Tropical Forest'; 
                              'boreal forest/taiga'='Boreal'; 
                              'desert/xeric shrublands'='Desert'; 
                              'flooded grassland/savanna'='Grassland/Savanna'; 
                              'mediterranean'='Mediterranean'; 
                              'montane grassland/savanna'='Grassland/Savanna'; 
                              'temperate broadleaf/mixed forest'='Temperate Forest';
                              'temperate coniferous forest'='Temperate Forest';
                              'temperate grassland/savanna'='Grassland/Savanna'; 
                              'tropical coniferous forest'='Tropical Forest'; 
                              'tropical dry broadleaf forest'='Tropical Forest'; 
                              'tropical grassland/savannas'='Grassland/Savanna';
                              'tropical moist broadleaf forest'='Tropical Forest'")
dat.uhi$Biome2 <- factor(dat.uhi$Biome2, levels=c("Desert", "Grassland/Savanna", "Mediterranean", "Tropical Forest", "Temperate Forest", "Boreal"))
summary(dat.uhi)
dim(dat.uhi)
dim(dat.uhi[dat.filter,])
dim(dat.uhi[!dat.filter,])
summary(dat.uhi[!dat.filter,])

hist(dat.uhi$prop.missing)
hist(dat.uhi$prop.temp.n.lo)

hist(dat.uhi$tree.slope[dat.filter], main="Tree Slope")
hist(dat.uhi$veg.slope[dat.filter], main="Veg Slope")


# ------------------------------------------------
# Looking at Urban Warming & vegetation trends
# ------------------------------------------------
# Calculated urban heat island effect
hist(dat.uhi$d.temp.summer.buff[dat.filter])
mean(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T); sd(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)
median(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)
range(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)
quantile(dat.uhi$d.temp.summer.buff[dat.filter], c(0.025, 0.975), na.rm=T)

# UHI summary by biome
warm.agg.mean <- aggregate(dat.uhi[dat.filter, c("temp.summer.city", "d.temp.summer.buff", "cover.tree.city", "cover.veg.city", "tree.slope", "veg.slope", "Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city")],
                      by=list(dat.uhi[dat.filter, "Biome2"]),
                      FUN=mean)
names(warm.agg.mean)[1] <- "Biome2"
warm.agg.mean$temp.summer.city <- warm.agg.mean$temp.summer.city-273.16
warm.agg.mean

warm.agg.sd <- aggregate(dat.uhi[dat.filter, c("temp.summer.city", "d.temp.summer.buff", "cover.tree.city", "cover.veg.city", "tree.slope", "veg.slope", "Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city")],
                           by=list(dat.uhi[dat.filter, "Biome2"]),
                           FUN=sd)
names(warm.agg.sd)[1] <- "Biome2"
warm.agg.sd

world <- map_data("world")
uhi.map <- ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=d.temp.summer.buff), size=3) +
  scale_color_gradient2(name="Urban\nWarming\n(deg. C)", low = "dodgerblue2", high = "red3", mid = "white", midpoint =0) +
  theme_bw() +
  theme(legend.position=c(0.1,0.4),
        legend.title=element_text(color="white", face="bold", size=rel(1.5)),
        legend.text=element_text(color="white", size=rel(1.25)),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

uhi.histo <- ggplot(data=dat.uhi[dat.filter,]) +
  geom_histogram(aes(x=d.temp.summer.buff, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Urban Warming (deg. C)") +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(name="Biome", values=paste(biome.pall$color)) +
  theme(legend.position=c(0.2, 0.65),
        legend.title=element_text(size=rel(1.5), face="bold"),
        legend.text=element_text(size=rel(1.25)),
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank())

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffect_Multiplot.png"), height=8, width=8, units="in", res=220)
cowplot::plot_grid(uhi.map, uhi.histo, ncol=1)
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffect_City_Map.png"), height=3.25, width=8, units="in", res=220)
uhi.map
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffect_City_Histogram_Biome.png"), height=4, width=8, units="in", res=220)
uhi.histo
dev.off()

summary(dat.uhi[dat.filter & dat.uhi$d.temp.summer.buff>0,])


ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(aes(x=d.cover.tree.buff, y=d.temp.summer.buff, color=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(aes(x=d.cover.tree.buff+d.cover.veg.buff, y=d.temp.summer.buff, color=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_hline(yintercept=0, linetype="dashed")


# Urban tree cover
hist(dat.uhi$cover.tree.city[dat.filter])
mean(dat.uhi$cover.tree.city[dat.filter], na.rm=T); sd(dat.uhi$cover.tree.city[dat.filter], na.rm=T)
median(dat.uhi$cover.tree.city[dat.filter], na.rm=T)
range(dat.uhi$cover.tree.city[dat.filter], na.rm=T)
quantile(dat.uhi$cover.tree.city[dat.filter], c(0.025, 0.975), na.rm=T)

mean(dat.uhi$cover.veg.city[dat.filter], na.rm=T); sd(dat.uhi$cover.veg.city[dat.filter], na.rm=T)
mean(dat.uhi$d.cover.veg.buff[dat.filter], na.rm=T); sd(dat.uhi$d.cover.veg.buff[dat.filter], na.rm=T)

# Overall veg differences
mean(dat.uhi$d.cover.tree.buff[dat.filter], na.rm=T); sd(dat.uhi$d.cover.tree.buff[dat.filter], na.rm=T)
mean(dat.uhi$d.cover.veg.buff[dat.filter], na.rm=T); sd(dat.uhi$d.cover.veg.buff[dat.filter], na.rm=T)

# Differences in arid biomes: Desert, Grassland/Savanna, Mediterranean
mean(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T); sd(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T)
mean(dat.uhi$d.cover.veg.buff[dat.filter & dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T); sd(dat.uhi$d.cover.veg.buff[dat.filter & dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T)

# Differences in forest-dominated biomes
mean(dat.uhi$d.cover.tree.buff[dat.filter & !dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T); sd(dat.uhi$d.cover.tree.buff[dat.filter & !dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T)
mean(dat.uhi$d.cover.veg.buff[dat.filter & !dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T); sd(dat.uhi$d.cover.veg.buff[dat.filter & !dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")], na.rm=T)


# city with highest cover
dat.uhi[!is.na(dat.uhi$cover.tree.city) & dat.uhi$cover.tree.city==max(dat.uhi$cover.tree.city[dat.filter], na.rm=T),]
dat.uhi[!is.na(dat.uhi$cover.tree.city) & dat.uhi$cover.tree.buff==max(dat.uhi$cover.tree.buff[dat.filter], na.rm=T),]

# dat.uhi[dat.uhi$cover.]
dat.uhi[!is.na(dat.uhi$cover.tree.city) & dat.uhi$d.cover.tree.buff==max(dat.uhi$d.cover.tree.buff[dat.filter], na.rm=T),]

# Difference between buffer & urban tree cover
t.test(dat.uhi$cover.tree.city, dat.uhi$cover.tree.buff, paired=T)
hist(dat.uhi$d.cover.tree.buff)
mean(dat.uhi$d.cover.tree.buff, na.rm=T); sd(dat.uhi$d.cover.tree.buff, na.rm=T)
median(dat.uhi$d.cover.tree.buff, na.rm=T)
quantile(dat.uhi$d.cover.tree.buff, c(0.025, 0.975), na.rm=T)

world <- map_data("world")
map.city.tree <-  ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cover.tree.city), size=3) +
  scale_colour_gradientn(name="Tree\nCover (%)", colors=grad.tree) +
  theme(legend.position=c(0.1,0.35),
        legend.title=element_text(color="gray90", face="bold", size=rel(1.5)),
        legend.text=element_text(color="gray90", size=rel(1.25)),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

map.city.other <-  ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cover.veg.city), size=3) +
  scale_colour_gradientn(name="Other Veg.\nCover (%)", colors=grad.other) +
  theme(legend.position=c(0.1,0.35),
        legend.title=element_text(color="gray90", face="bold", size=rel(1.5)),
        legend.text=element_text(color="gray90", size=rel(1.25)),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

map.city.noveg <-  ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cover.noveg.city), size=3) +
  scale_colour_gradientn(name="No Veg.\nCover (%)", colors=grad.bare) +
  theme(legend.position=c(0.1,0.35),
        legend.title=element_text(color="gray90", face="bold", size=rel(1.5)),
        legend.text=element_text(color="gray90", size=rel(1.25)),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_Map_CoverType_City_Multiplot.png"), height=10, width=8, units="in", res=220)
cowplot::plot_grid(map.city.tree, map.city.other, map.city.noveg, ncol=1)
dev.off()




map.city.cover <- ggplot(data=dat.uhi[dat.filter,]) +
  geom_text(x=-170, y=-10, label="City Cover", color="gray90", size=7, hjust=0, fontface="bold") +
  geom_text(x=-170, y=-25, label="% Trees", color="green", size=6, hjust=0) +
  geom_text(x=-170, y=-37.5, label="% Other Veg.", color="blue", size=6, hjust=0) +
  geom_text(x=-170, y=-50, label="% No Veg.", color="red", size=6, hjust=0) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), size=3, color=rgb(dat.uhi$cover.noveg.city[dat.filter]/100,
                                                             dat.uhi$cover.tree.city[dat.filter]/100,
                                                             dat.uhi$cover.veg.city[dat.filter]/100)) +
  theme(legend.position="top",
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

map.buffer.cover <- ggplot(data=dat.uhi[dat.filter,]) +
  geom_text(x=-170, y=-10, label="Buffer Cover", color="gray90", size=7, hjust=0, fontface="bold") +
  geom_text(x=-170, y=-25, label="% Trees", color="green", size=6, hjust=0) +
  geom_text(x=-170, y=-37.5, label="% Other Veg.", color="blue", size=6, hjust=0) +
  geom_text(x=-170, y=-50, label="% No Veg.", color="red", size=6, hjust=0) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), size=3, color=rgb(dat.uhi$cover.noveg.buff[dat.filter]/100,
                                                             dat.uhi$cover.tree.buff[dat.filter]/100,
                                                             dat.uhi$cover.veg.buff[dat.filter]/100)) +
  theme(legend.position="top",
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_Map_Cover_Multiplot.png"), height=10, width=8, units="in", res=220)
cowplot::plot_grid(map.buffer.cover, map.city.cover, map.city.tree, ncol=1)
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_CoverTree_City_Map.png"), height=3.25, width=8, units="in", res=220)
map.city.tree
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_CoverType_City_Map.png"), height=3.25, width=8, units="in", res=220)
map.city.cover
dev.off()


png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_CoverType_Buffer_Map.png"), height=3.25, width=8, units="in", res=220)
map.buffer.cover
dev.off()

# png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_CoverType_City_Map_RYB.png"), height=3.25, width=8, units="in", res=220)
png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_CoverType_City_Map_OBG.png"), height=3.25, width=8, units="in", res=220)
ggplot(data=dat.uhi[dat.filter,]) +
  geom_text(x=-170, y=-10, label="City Cover", color="gray90", size=7, hjust=0, fontface="bold") +
  geom_text(x=-170, y=-25, label="% Trees", color="#009E73", size=6, hjust=0) +
  geom_text(x=-170, y=-37.5, label="% Other Veg.", color="#0072B2", size=6, hjust=0) +
  geom_text(x=-170, y=-50, label="% No Veg.", color="#D55E00", size=6, hjust=0) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), size=3, color="#D55E00", alpha=dat.uhi$cover.noveg.city[dat.filter]/100) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), size=3, color="#0072B2", alpha=dat.uhi$cover.veg.city[dat.filter]/100) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE), size=3, color="#009E73", alpha=dat.uhi$cover.tree.city[dat.filter]/100) +
  guides(color=F, alpha=F, size=F) +
  theme(legend.position="top",
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
dev.off()






cover.comp <- stack(city.buffer[,c("cover.tree", "cover.veg", "cover.noveg")])
names(cover.comp) <- c("cover", "type")
cover.comp[,c("Name", "Biome2", "location", "cover.dom", "cool.tree", "cool.veg")] <- city.buffer[,c("Name", "Biome2", "location", "cover.dom", "cool.tree", "cool.veg")]
cover.comp$trend <- stack(city.buffer[,c("trend.tree", "trend.veg", "trend.noveg")])[,1]
summary(cover.comp)

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_DominantCover_CityBuff.png"), height=6, width=6, units="in", res=220)
ggplot(data=city.buffer[!is.na(city.buffer$cover.dom),]) +
  geom_bar(aes(x=cover.dom, fill=location), stat="count", position="dodge") +
  scale_x_discrete(name="Dominant Cover Type") +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b")) +
  theme_bw() +
  theme(legend.position=c(0.8, 0.8))
dev.off()


# comparing difference in cover 
cover.comp2 <- stack(dat.uhi[dat.filter,c("d.cover.tree.buff", "d.cover.veg.buff", "d.cover.noveg.buff")])
cover.comp2$ind <- car::recode(cover.comp2$ind, "'d.cover.tree.buff'='Trees'; 'd.cover.veg.buff'='Other Veg.'; 'd.cover.noveg.buff'='No Veg.'")
cover.comp2[,c("Name", "Biome2")] <- dat.uhi[dat.filter, c("NAME", "Biome2")]
cover.comp2$ind <- factor(cover.comp2$ind, levels=c("Trees", "Other Veg.", "No Veg."))
summary(cover.comp2)

cover.labels = data.frame(x=-45, y=75, 
                          panel.label=c("a)", "b)", "c)"),
                          ind=c("Trees", "Other Veg.", "No Veg."))

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_CoverType_Difference_Histograms_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=cover.comp2) +
  facet_grid(ind~.) +
  geom_text(data=cover.labels, aes(x=x-5, y=y, label=panel.label), size=6, fontface="bold", hjust=0) +
  geom_text(data=cover.labels, aes(x=x, y=y, label=ind), size=6, hjust=0) +
  geom_histogram(aes(x=values, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Difference in Cover (%)", expand=c(0,0)) +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(name="Biome", values=paste(biome.pall$color)) +
  theme(legend.position="top",
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size=rel(1), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank())
dev.off()

summary(dat.uhi[dat.filter & dat.uhi$d.temp.summer.buff<0,])
n.cool <- length(which(dat.filter & dat.uhi$d.temp.summer.buff<0))
length(which(dat.filter & dat.uhi$d.temp.summer.buff<0 & dat.uhi$Biome2 %in% c("Desert", "Grassland/Savanna", "Mediterranean")))/n.cool

mean(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$d.temp.summer.buff>0]); sd(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$d.temp.summer.buff>0])
mean(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$d.temp.summer.buff<=0]); sd(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$d.temp.summer.buff<=0])

mean(dat.uhi$d.cover.noveg.buff[dat.filter & dat.uhi$d.temp.summer.buff>0]); sd(dat.uhi$d.cover.noveg.buff[dat.filter & dat.uhi$d.temp.summer.buff>0])
mean(dat.uhi$d.cover.noveg.buff[dat.filter & dat.uhi$d.temp.summer.buff<=0]); sd(dat.uhi$d.cover.noveg.buff[dat.filter & dat.uhi$d.temp.summer.buff<=0])

# Cover Difference T-Tests: no warming
t.test(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$d.temp.summer.buff<=0])
t.test(dat.uhi$d.cover.noveg.buff[dat.filter & dat.uhi$d.temp.summer.buff<=0])
t.test(dat.uhi$d.cover.veg.buff[dat.filter & dat.uhi$d.temp.summer.buff<=0])

# Cover Difference T-Tests: warming
t.test(dat.uhi$d.cover.tree.buff[dat.filter & dat.uhi$d.temp.summer.buff>0])
t.test(dat.uhi$d.cover.veg.buff[dat.filter & dat.uhi$d.temp.summer.buff>0])
t.test(dat.uhi$d.cover.noveg.buff[dat.filter & dat.uhi$d.temp.summer.buff>0])



png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCover_CityDiff_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=-d.cover.tree.buff), size=3) +
  scale_colour_distiller(name="Difference Tree Cover", palette="BrBG", limits=c(-1,1)*max(abs(dat.uhi$d.cover.tree.buff), na.rm=T)) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top",
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank())
dev.off()


# No difference in tree-cover trends in the urban vs. buffer
# number of cities with statistical trend in tree cover
length(which(dat.filter & dat.uhi$p.trend.cover.tree.city<0.05))/length(which(dat.filter)) # 70% of cities have a significant trend in tree cover, but it's mixed as to direction
length(which(dat.filter & dat.uhi$p.trend.cover.veg.city<0.05))/length(which(dat.filter)) # 
length(which(dat.filter & dat.uhi$p.trend.cover.noveg.city<0.05))/length(which(dat.filter)) # vast majority of cities have no trend in unvegetated cover

hist(dat.uhi[dat.filter & dat.uhi$p.trend.cover.tree.city<0.05, "trend.cover.tree.city"]) # 
hist(dat.uhi[dat.filter & dat.uhi$p.trend.cover.veg.city<0.05, "trend.cover.veg.city"]) # 
hist(dat.uhi[dat.filter & dat.uhi$p.trend.cover.noveg.city<0.05, "trend.cover.noveg.city"]) # 

t.test(dat.uhi$trend.cover.tree.city[dat.filter], dat.uhi$trend.cover.tree.buff[dat.filter], paired=T)
t.test(dat.uhi$trend.cover.tree.city) # global trend towards slighly increased cover (0.1%/yr), but no real ecological significance
hist(dat.uhi$trend.cover.tree.city)
mean(dat.uhi$trend.cover.tree.city, na.rm=T); sd(dat.uhi$trend.cover.tree.city, na.rm=T)
median(dat.uhi$trend.cover.tree.city, na.rm=T)
quantile(dat.uhi$trend.cover.tree.city, c(0.025, 0.975), na.rm=T)
# ------------------------------------------------



# ------------------------------------------------
# Digging into model & predicted tree effects
# ------------------------------------------------
dat.filter <- dat.uhi$prop.missing<0.33 & dat.uhi$prop.temp.n.lo<0.33 & !is.na(dat.uhi$gam.r2) & !is.na(dat.uhi$Biome2)

summary(dat.uhi[dat.filter,])
summary(dat.uhi$R2.lin[dat.filter]-dat.uhi$R2.log[dat.filter])

# Model Summary
hist(dat.uhi$gam.r2[dat.filter])
range(dat.uhi$gam.r2[dat.filter], na.rm=T)
mean(dat.uhi$gam.r2[dat.filter], na.rm=T); sd(dat.uhi$gam.r2[dat.filter], na.rm=T)
summary(dat.uhi$tree.slope.lin)
summary(dat.uhi$elevation.slope[dat.filter]) # adiabatic laspe = -9.8˚C/km = -9.8e-3

# Looking for significant 
summary(dat.uhi$WWF_BIOME)
tree.filter <- dat.uhi$cover.tree.90>10
length(which(dat.filter)); length(which(tree.filter))


cool.tree <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$tree.slope<0
warm.tree <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$tree.slope>0
cool.veg <- dat.filter & dat.uhi$veg.pval<0.05 & dat.uhi$veg.slope<0
warm.veg <- dat.filter & dat.uhi$veg.pval<0.05 & dat.uhi$veg.slope>0
# length(which(cool.tree))/length(which(tree.filter))
summary(dat.uhi[dat.filter,])
summary(dat.uhi[cool.tree,])
summary(dat.uhi[warm.tree,]) # only 1 city
summary(dat.uhi[cool.veg,])
summary(dat.uhi[warm.veg,]) 

summary(dat.uhi[dat.filter,"gam.r2"])

# looking at mean cooling effects
mean(dat.uhi$Tdiff.trees2noveg.city[dat.filter]); sd(dat.uhi$Tdiff.trees2noveg.city[dat.filter])
mean(dat.uhi$Tdiff.veg2noveg.city[dat.filter]); sd(dat.uhi$Tdiff.veg2noveg.city[dat.filter])



mean(dat.uhi$tree.slope[dat.filter]); sd(dat.uhi$tree.slope[dat.filter])
summary(dat.uhi[dat.filter & dat.uhi$tree.pval<0.05,])

dat.uhi[dat.filter & dat.uhi$veg.slope<dat.uhi$tree.slope,]

# Comparing effects of different veg types
names(dat.uhi)
slope.comp <- stack(dat.uhi[dat.filter,c("tree.slope", "veg.slope")])
names(slope.comp)[names(slope.comp)=="values"] <- c("slope")
slope.comp$ind <- car::recode(slope.comp$ind, "'tree.slope'='Trees'; 'veg.slope'='Other Veg.'")
slope.comp$val.p <- stack(dat.uhi[dat.filter,c("tree.pval", "veg.pval")])$values
slope.comp$cover <- stack(dat.uhi[dat.filter,c("cover.tree.city", "cover.veg.city")])$values
slope.comp$sig <- as.factor(ifelse(slope.comp$val.p<0.05, "p<0.05", "N.S."))
slope.comp[,c("Name", "Biome2", "temp.summer.city")] <- dat.uhi[dat.filter, c("NAME", "Biome2", "temp.summer.city")]
slope.comp$ind <- factor(slope.comp$ind, levels=c("Trees", "Other Veg.", "No Veg."))
summary(slope.comp)

slope.labels = data.frame(x=-0.9, y=100, 
                          panel.label=c("a)", "b)"),
                          ind=c("Trees", "Other Veg."))

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Slopes_Histograms_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=slope.comp[,]) +
  facet_grid(ind~.) +
  geom_text(data=slope.labels, aes(x=x-0.1, y=y, label=panel.label), size=6, fontface="bold", hjust=0) +
  geom_text(data=slope.labels, aes(x=x, y=y, label=ind), size=6, hjust=0) +
  geom_histogram(aes(x=slope, fill=Biome2, alpha=sig)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Effect Slope (deg C / % Cover)", expand=c(0,0)) +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(name="Biome", values=paste(biome.pall$color)) +
  scale_alpha_manual(name="significance", values=c(0.4, 1)) +
  theme(legend.position="top",
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size=rel(1), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank())
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Slopes_v_Cover_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=slope.comp[,]) +
  facet_wrap(ind~., scales="free", ncol=1) +
  geom_point(aes(x=cover, y=slope, color=Biome2, alpha=sig)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(name="Effect Slope (deg C / % Cover)") +
  scale_x_continuous(name="Mean Cover (%)") +
  scale_color_manual(name="Biome", values=paste(biome.pall$color)) +
  scale_alpha_manual(name="significance", values=c(0.4, 1)) +
  theme(legend.position="right",
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1)),
        legend.key = element_rect(fill=NA),
        strip.background = element_blank(),
        strip.text = element_text(size=rel(1.25), face="bold", color="black", hjust=0),
        axis.text = element_text(size=rel(1), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank())
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Slopes_v_Temp_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=slope.comp[,]) +
  facet_wrap(ind~., scales="free", ncol=1) +
  geom_point(aes(x=temp.summer.city-273.16, y=slope, color=Biome2, alpha=sig)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_y_continuous(name="Effect Slope (deg C / % Cover)") +
  scale_x_continuous(name="Mean Summer Temp (deg. C)") +
  scale_color_manual(name="Biome", values=paste(biome.pall$color)) +
  scale_alpha_manual(name="significance", values=c(0.4, 1)) +
  theme(legend.position="right",
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1)),
        legend.key = element_rect(fill=NA),
        strip.background = element_blank(),
        strip.text = element_text(size=rel(1.25), face="bold", color="black", hjust=0),
        axis.text = element_text(size=rel(1), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank())
dev.off()


mean(dat.uhi$tree.slope[dat.filter]); sd(dat.uhi$tree.slope[dat.filter])
mean(dat.uhi$veg.slope[dat.filter]); sd(dat.uhi$veg.slope[dat.filter])
# mean(dat.uhi$noveg.slope[dat.filter]); sd(dat.uhi$noveg.slope[dat.filter])

# length(which(dat.uhi$tree.pval<0.05))/nrow(dat.uhi[,]) # Significant tree effect in 86% of ALL cities, even with bad data
length(which((cool.tree) | (warm.tree)))/length(which(dat.filter)) # Significant tree effect in 99% of cities
length(which(cool.tree))/length(which(dat.filter)) # Significant Tree Effect effect in 99% of cities
length(which(warm.tree))/length(which(dat.filter)) # Significant tree warming in effect in 1 city in Yemen
length(which(cool.veg))/length(which(dat.filter)) # Significant Tree Effect effect in 64% of cities
length(which(warm.veg))/length(which(dat.filter)) # Significant tree warming in effect in 33% of cities

mean(dat.uhi[dat.filter,"Tdiff.trees2noveg.city"]); sd(dat.uhi[dat.filter,"Tdiff.trees2noveg.city"]); 
mean(dat.uhi[dat.filter,"Tdiff.trees2veg.city"]); sd(dat.uhi[dat.filter,"Tdiff.trees2veg.city"]); 

dat.uhi[warm.tree,] # only 1 city shows a sig. pos slope, but it's actual impact on temperature is negligible

names(dat.uhi)
effect.comp <- stack(dat.uhi[dat.filter,c("Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city")])
effect.comp$ind <- car::recode(effect.comp$ind, "'Tdiff.trees2noveg.city'='Trees'; 'Tdiff.veg2noveg.city'='Other Veg.'")
effect.comp[,c("Name", "Biome2")] <- dat.uhi[dat.filter, c("NAME", "Biome2")]
effect.comp$ind <- factor(effect.comp$ind, levels=c("Trees", "Other Veg.", "No Veg."))
summary(effect.comp)

effect.labels = data.frame(x=-4.5, y=70,
                          panel.label=c("a)", "b)"),
                          ind=c("Trees", "Other Veg."))

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Effect_Histograms_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=effect.comp[,]) +
  facet_grid(ind~.) +
  geom_text(data=effect.labels, aes(x=x-0.75, y=y, label=panel.label), size=6, fontface="bold", hjust=0) +
  geom_text(data=effect.labels, aes(x=x, y=y, label=ind), size=6, hjust=0) +
  geom_histogram(aes(x=-values, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Effect on Temperature (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(name="Biome", values=paste(biome.pall$color)) +
  theme(legend.position="top",
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size=rel(1), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank())
dev.off()


# Creating a categorical, binned warming response
dat.uhi$TreeEffect <- round(dat.uhi$Tdiff.trees2noveg.city*4)/4 # To the nearest 0.25 degree
dat.uhi$TreeEffect2 <- round(dat.uhi$Tdiff.trees2noveg.city) # To the nearest degree
summary(dat.uhi)
length(unique(dat.uhi$TreeEffect))


# bins <- 100
# cols <- c("darkblue","darkred")
# colGradient <- colorRampPalette(cols)
# cut.cols <- colGradient(bins)
# cuts <- cut(df$val,bins)
# names(cuts) <- sapply(cuts,function(t) cut.cols[which(as.character(t) == levels(cuts))])
# 
effect.lims <- seq(-1*max(abs(dat.uhi$TreeEffect), na.rm=T), 1*max(abs(dat.uhi$TreeEffect), na.rm=T), by=0.5)
effect.palette <- colorRampPalette(brewer.pal(11, "BrBG"))(length(effect.lims))

effect.lims2 <- seq(-1*max(abs(dat.uhi$TreeEffect2), na.rm=T), 1*max(abs(dat.uhi$TreeEffect2), na.rm=T), by=1)
effect.palette2 <- colorRampPalette(brewer.pal(11, "BrBG"))(length(effect.lims2))
# length(unique())

world <- map_data("world")
map.tree.cool <- ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  # geom_point(data=dat.uhi[!dat.filter,], aes(x=LONGITUDE, y=LATITUDE), size=1, color="gray70") +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval>=0.05,], aes(x=LONGITUDE, y=LATITUDE), color="gray30", size=3) +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=-Tdiff.trees2noveg.city), size=3) +
  scale_color_gradient2(name="Tree\nEffect\n(deg. C)", low = "dodgerblue2", high = "red3", mid = "white", midpoint =0, limits=range(dat.uhi[dat.filter, c("Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city")]*-1)) +
  theme_bw() +
  theme(legend.position=c(0.1,0.35),
        legend.title=element_text(color="white", face="bold", size=rel(1.25)),
        legend.text=element_text(color="white", size=rel(1.)),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())

map.veg.cool <- ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  # geom_point(data=dat.uhi[!dat.filter,], aes(x=LONGITUDE, y=LATITUDE), size=1, color="gray70") +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$veg.pval>=0.05,], aes(x=LONGITUDE, y=LATITUDE), color="gray30", size=3) +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$veg.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=-Tdiff.veg2noveg.city), size=3) +
  scale_color_gradient2(name="Other Veg.\nEffect\n(deg. C)", low = "dodgerblue2", high = "red3", mid = "white", midpoint=0, limits=range(dat.uhi[dat.filter, c("Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city")]*-1)) +
  theme_bw() +
  theme(legend.position=c(0.1,0.35),
        legend.title=element_text(color="white", face="bold", size=rel(1.25)),
        legend.text=element_text(color="white", size=rel(1.)),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())


png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_Effects_Multiplot_Map_GAM.png"), height=7.5, width=8, units="in", res=220)
cowplot::plot_grid(map.tree.cool, map.veg.cool, ncol=1)
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_Effects_Trees_Map_GAM.png"), height=3.25, width=8, units="in", res=220)
map.tree.cool
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_Effects_Veg_Map_GAM.png"), height=3.25, width=8, units="in", res=220)
map.veg.cool
dev.off()



# Comparing cooling effects of the urban heat island effect
summary(dat.uhi)
dat.uhi$Temp_NoVeg <- dat.uhi$d.temp.summer.buff + dat.uhi$Tdiff.trees2noveg.city + dat.uhi$Tdiff.veg2noveg.city
veg.efffects <- stack(dat.uhi[dat.filter,c("d.temp.summer.buff", "Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city", "Temp_NoVeg")])
veg.efffects[,c("Name", "Biome2", "LATITUDE", "LONGITUDE")] <- dat.uhi[dat.filter, c("NAME", "Biome2", "LATITUDE", "LONGITUDE")]
veg.efffects$ind <- car::recode(veg.efffects$ind, "'d.temp.summer.buff'='Urban Warming (Observed)'; 'Tdiff.trees2noveg.city'='Tree Effect'; 'Tdiff.veg2noveg.city'='Other Veg. Effect'; 'Temp_NoVeg'='No Veg. (Modeled)'")
veg.efffects$values <- ifelse(veg.efffects$ind %in% c("Urban Warming (Observed)", "No Veg. (Modeled)"), veg.efffects$values, -veg.efffects$values)
veg.efffects$ind <- factor(veg.efffects$ind, levels=c("Urban Warming (Observed)", "Tree Effect", "Other Veg. Effect", "No Veg. (Modeled)"))
summary(veg.efffects)


png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Veg_Map.png"), height=8, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=veg.efffects[veg.efffects$ind %in% c("Tree Effect", "Other Veg. Effect"),]) +
  facet_grid(ind~.) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=values), size=3) +
  scale_color_gradient2(name="Vegetation Effect\n(deg. C)", low = "dodgerblue2", high = "red3", mid = "white", midpoint=0, limits=range(dat.uhi[dat.filter, c("Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city")]*-1)) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_text(color="black", face="bold", size=rel(1.25)),
        legend.text=element_text(color="black", size=rel(1.)),
        legend.background=element_blank(),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
dev.off()


png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Histograms_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=veg.efffects) +
  facet_grid(ind~.) +
  geom_histogram(aes(x=values, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Temperature (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="# of Cities", expand=c(0,0)) +
  scale_fill_manual(name="Biome", values=paste(biome.pall$color)) +
  theme(legend.position="top",
        legend.title=element_text(size=rel(1.5), face="bold"),
        legend.text=element_text(size=rel(1.25)),
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank())
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Density.png"), height=6, width=6, units="in", res=220)
ggplot(data=veg.efffects) +
  # facet_grid(ind~.) +
  geom_density(aes(x=values, fill=ind), alpha=0.8) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Temperature (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="Proportion of Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("black", paste(cat.3$color6))) +
  theme_bw() +
  theme(legend.position=c(0.65, 0.8),
        legend.text = element_text(size=rel(1.5)),
        # legend.title=element_text(size=rel(1.5), face="bold"),
        legend.title=element_blank(),
        panel.grid=element_blank()) 
dev.off()
# --------------------------------------------------------------




