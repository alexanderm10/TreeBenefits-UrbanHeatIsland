# Analyzing how much trees push temperatures away from the 35˚C Wet Bulb Temperature Threshold
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
path.dat <- "../data_processed/"
path.figs <- "/Volumes/GoogleDrive/My Drive/TreeBenefits_UrbanHeatIsland/figures/veg_only"

# Read in city summary dataset
dat.uhi <- read.csv("../data_processed/analysis_cities_summary_sdei_v6.csv")
summary(dat.uhi)

# Read in the GLDAS-extracted data
dat.gldas <- read.csv("../data_processed/cities_full_sdei_v6/analysis_all_years/Cities_GLDAS_extract_2011-2015.csv")
summary(dat.gldas)

# Read in some functions to make adjusting things easier
source("wetbulb_functions.R")

n.lo = 2 # Minimum number of time points used to describe that summer's temperature
buff.use = 10

pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
# yr.process <- 2011:2015
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago")
  # i=which(dat.uhi$NAME=="MEDELLIN")
  # i=which(dat.uhi$NAME=="Tokyo")
  # i=which(dat.uhi$NAME=="Nashville-Davidson")
  # i=which(dat.uhi$NAME=="Atlanta")
  # i=which(dat.uhi$NAME=="Cuernavaca")
  
  setTxtProgressBar(pb, i)
  path.city <- file.path(path.dat, "cities_full_sdei_v6", "analysis_all_years", paste0(dat.uhi$NAME[i], "_data_full_analyzed.csv"))
  
  if(!file.exists(path.city)) next
  
  dat.city <- read.csv(path.city)
  dat.city <- dat.city[dat.city$location==0 & !is.na(dat.city$gam.pred),]
  summary(dat.city)
  dim(dat.city)
  
  dat.city <- merge(dat.city, dat.gldas[dat.gldas$NAME==dat.uhi$NAME[i],c("year", "GLDAS.Qair", "GLDAS.Psurf", "GLDAS.Tair", "GLDAS.Tsurf")], all.x=T, all.y=F)
  summary(dat.city)
  
  # Re-adjust pressure and calculate relative humidity
  dat.city$Psurf.adj <- adjust.press(h=dat.city$elevation/mean(dat.city$elevation), p0=dat.city$GLDAS.Psurf)
  summary(dat.city)
  # Calculate RH taking into account elevation adjustments --> question is do we take into account temp adjustments too?  I think we need to... 
  # -- if we do, higher humidity where trees are and it's cooler
  # -- if we don't very similar humidty with higher by the lake
  dat.city$RH.adj <- qair2rh(dat.city$GLDAS.Qair, dat.city$temp.summer-273.16, dat.city$GLDAS.Psurf*0.01)*100
  dat.city$RH.pred <- qair2rh(dat.city$GLDAS.Qair, dat.city$gam.pred-273.16, dat.city$GLDAS.Psurf*0.01)*100
  dat.city$RH.notree <- qair2rh(dat.city$GLDAS.Qair, dat.city$pred.trees2noveg-273.16, dat.city$GLDAS.Psurf*0.01)*100
  summary(dat.city)
  
  # dat.city$Temp.Summer.C <- dat.city$temp.summer-273.16
  dat.city$Twb.summer.adj <- calc.wetbulb(TEMP=dat.city$temp.summer-273.16, RH=dat.city$RH.adj)
  dat.city$Twb.summer.pred <- calc.wetbulb(TEMP=dat.city$gam.pred-273.16, RH=dat.city$RH.adj)
  dat.city$Twb.summer.notree.adj <- calc.wetbulb(TEMP=dat.city$pred.trees2noveg-273.16, RH=dat.city$RH.adj)
  summary(dat.city)
  
  # Doing an additional check with constant RH
  # dat.city$RH.gldas <- qair2rh(dat.city$GLDAS.Qair, dat.city$GLDAS.Tair-273.16, dat.city$GLDAS.Psurf*0.01)*100
  # dat.city$WetBulb.Diff.adj <- dat.city$Twb.summer.adj - (dat.city$Temp.Summer.C)
  # dat.city$Twb.summer.gldas <- calc.wetbulb(TEMP=dat.city$Temp.Summer.C, RH=dat.city$RH.gldas)
  # dat.city$WetBulb.Diff.gldas <- dat.city$Twb.summer.gldas - (dat.city$Temp.Summer.C)
  # summary(dat.city)
  
  # Some QAQC graphs
  # ggplot(data=dat.city[,]) +
  #   coord_equal() +
  #   facet_wrap(~year) +
  #   geom_raster(aes(x=x, y=y, fill=Temp.Summer.C))
  # 
  # ggplot(data=dat.city[]) +
  #   coord_equal() +
  #   facet_wrap(~year) +
  #   geom_raster(aes(x=x, y=y, fill=Twb.summer.adj))
  # 
  # ggplot(data=dat.city[,]) +
  #   coord_equal() +
  #   facet_wrap(~year) +
  #   geom_raster(aes(x=x, y=y, fill=Twb.summer.notree.adj))
  

  # Calculate the percentage of cells in the city that exceed the 35˚ threshold with and without trees
  # Note: need to calculate it per year and then average years
  df.tmp <- data.frame(year=unique(dat.city$year))
  
  for(YR in unique(df.tmp$year)){
    yr.ind <- which(df.tmp$year==YR)
    
    df.tmp[yr.ind, "Twb.obs"] <- mean(dat.city$Twb.summer.adj[dat.city$year==YR], na.rm=T)
    df.tmp[yr.ind, "Twb.pred"] <- mean(dat.city$Twb.summer.pred[dat.city$year==YR], na.rm=T)
    df.tmp[yr.ind, "Twb.tree2noveg"] <- mean(dat.city$Twb.summer.notree.adj[dat.city$year==YR], na.rm=T)
    
    df.tmp[yr.ind, "p.TwbAbv35.obs"] <- length(dat.city$Twb.summer.adj[dat.city$year==YR & dat.city$Twb.summer.adj>35 & !is.na(dat.city$Twb.summer.adj)])/length(dat.city$Twb.summer.adj[dat.city$year==YR & !is.na(dat.city$Twb.summer.adj)])
    df.tmp[yr.ind, "p.TwbAbv35.pred"] <- length(dat.city$Twb.summer.pred[dat.city$year==YR & dat.city$Twb.summer.pred>35 & !is.na(dat.city$Twb.summer.pred)])/length(dat.city$Twb.summer.pred[dat.city$year==YR & !is.na(dat.city$Twb.summer.pred)])
    df.tmp[yr.ind, "p.TwbAbv35.tree2noveg"] <- length(dat.city$Twb.summer.notree.adj[dat.city$year==YR & dat.city$Twb.summer.notree.adj>35 & !is.na(dat.city$Twb.summer.notree.adj)])/length(dat.city$Twb.summer.notree.adj[yr.ind & !is.na(dat.city$Twb.summer.notree.adj)])
  }
  
  dat.uhi[i, "Twb.obs"] <- mean(df.tmp$Twb.obs, na.rm=T)
  dat.uhi[i, "Twb.pred"] <- mean(df.tmp$Twb.pred, na.rm=T)
  dat.uhi[i, "Twb.tree2noveg"] <- mean(df.tmp$Twb.tree2noveg, na.rm=T)
  
  dat.uhi[i, "p.TwbAbv35.obs"] <- mean(df.tmp$p.TwbAbv35.obs, na.rm=T)
  dat.uhi[i, "p.TwbAbv35.pred"] <- mean(df.tmp$p.TwbAbv35.pred, na.rm=T)
  dat.uhi[i, "p.TwbAbv35.tree2noveg"] <- mean(df.tmp$p.TwbAbv35.tree2noveg, na.rm=T)
  
  rm(dat.city)
}
dat.uhi$temp.summer.city <- dat.uhi$temp.summer.city-273.16
dat.uhi$diff.Twb.tree2noveg <- dat.uhi$Twb.tree2noveg - dat.uhi$Twb.pred
summary(dat.uhi)


# -------------------------
# Pulling in the data filtering from the other analyses
# -------------------------
dat.filter <- dat.uhi$prop.missing<0.33 & dat.uhi$prop.temp.n.lo<0.33

# Fix some biome mis-specifications
dat.uhi$WWF_ECO <- as.character(dat.uhi$WWF_ECO)
dat.uhi$WWF_BIOME <- as.character(dat.uhi$WWF_BIOME)
dat.uhi[is.na(dat.uhi$WWF_BIOME),]
dat.uhi[!is.na(dat.uhi$WWF_BIOME) & dat.uhi$WWF_BIOME=="98",]

# Ecoregions based on looking at them on a map on a websites
dat.uhi[dat.uhi$NAME=="NewYork", c("WWF_ECO", "WWF_BIOME")] <- c("Northeastern coastal forests", "temperate broadleaf/mixed forest")
dat.uhi[dat.uhi$NAME=="SanJose", c("WWF_ECO", "WWF_BIOME")] <- c("California interior chaparral and woodlands", "mediterranean")
dat.uhi[dat.uhi$NAME=="VirginiaBeach", c("WWF_ECO", "WWF_BIOME")] <- c("Middle Atlantic coastal forests", "temperate coniferous forest")
dat.uhi[dat.uhi$NAME=="as-Sib", c("WWF_ECO", "WWF_BIOME")] <- c("Gulf of Oman desert and semi-desert", "desert/xeric shrublands")
dat.uhi[dat.uhi$NAME=="Genova", c("WWF_ECO", "WWF_BIOME")] <- c("Italian sclerophyllous and semi-deciduous forests", "mediterranean")
dat.uhi[dat.uhi$NAME=="Itaquari", c("WWF_ECO", "WWF_BIOME")] <- c("Bahia coastal forests", "tropical moist broadleaf forest")
dat.uhi[dat.uhi$NAME=="Maracaibo", c("WWF_ECO", "WWF_BIOME")] <- c("Guajira-Barranquilla xeric scrub", "desert/xeric shrublands")
dat.uhi[dat.uhi$NAME=="Shenzhen", c("WWF_ECO", "WWF_BIOME")] <- c("South China-Vietnam subtropical evergreen forests", "tropical moist broadleaf forest")
dat.uhi[dat.uhi$NAME=="Toronto", c("WWF_ECO", "WWF_BIOME")] <- c("Southern Great Lakes forests", "temperate broadleaf/mixed forest")
dat.uhi[dat.uhi$NAME=="Rasht", c("WWF_ECO", "WWF_BIOME")] <- c("Caspian Hyrcanian mixed forests", "temperate coniferous forest")
dat.uhi$WWF_ECO <- as.factor(dat.uhi$WWF_ECO)
dat.uhi$WWF_BIOME <- as.factor(dat.uhi$WWF_BIOME)
dat.uhi[dat.uhi$WWF_BIOME=="mangroves",]
summary(dat.uhi$WWF_BIOME)


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

# -------------------------





summary(dat.uhi[dat.filter & !is.na(dat.uhi$p.TwbAbv35.obs) & dat.uhi$p.TwbAbv35.tree2noveg>0,c("NAME", "WWF_ECO", "WWF_BIOME", "temp.summer.city", "p.TwbAbv35.obs", "p.TwbAbv35.pred", "p.TwbAbv35.tree2noveg")])

dat.uhi[dat.filter & !is.na(dat.uhi$p.TwbAbv35.obs) & dat.uhi$p.TwbAbv35.tree2noveg>1e-4,c("NAME", "WWF_ECO", "WWF_BIOME", "temp.summer.city", "p.TwbAbv35.obs", "p.TwbAbv35.pred", "p.TwbAbv35.tree2noveg")]


dat.warm <- stack(dat.uhi[dat.filter,c("Tdiff.trees2noveg.city", "diff.Twb.tree2noveg")])
dat.warm[,c("NAME", "WWF_ECO", "WWF_BIOME", "Biome2", "prop.missing", "prop.temp.n.lo")] <- dat.uhi[dat.filter,c("NAME", "WWF_ECO", "WWF_BIOME", "Biome2", "prop.missing", "prop.temp.n.lo")]
dat.warm$type <- car::recode(dat.warm$ind, "'Tdiff.trees2noveg.city'='Dry Bulb'; 'diff.Twb.tree2noveg'='Wet Bulb'")
dat.warm$MeanTemp.trees <- stack(dat.uhi[dat.filter,c("temp.summer.city", "Twb.pred")])[,1]
summary(dat.warm)

png(file.path(path.figs, "Temperature_DryBulbWetBulb_Histogram_Biome.png"), height=6, width=8, units="in", res=220)
ggplot(data=dat.warm) +
  facet_grid(type~.) +
  geom_histogram(aes(x=MeanTemp.trees, fill=Biome2)) +
  geom_vline(xintercept=35, linetype="dashed") +
  scale_x_continuous(name="Mean Summer Temperature (deg. C)") +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(name="Biome", values=paste(biome.pall$color)) +
  theme(legend.position="top",
        legend.title=element_text(size=rel(1.5), face="bold"),
        legend.text=element_text(size=rel(1.25)),
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        strip.text = element_text(size=rel(1.25), face="bold"))
dev.off()
 

png(file.path(path.figs, "WarmingEffect_DryBulbWetBulb_Histogram_Biome.png"), height=6, width=8, units="in", res=220)
ggplot(data=dat.warm) +
  facet_grid(type~.) +
  geom_histogram(aes(x=values, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Urban Warming (deg. C)") +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(name="Biome", values=paste(biome.pall$color)) +
  theme(legend.position="top",
        legend.title=element_text(size=rel(1.5), face="bold"),
        legend.text=element_text(size=rel(1.25)),
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        strip.text = element_text(size=rel(1.25), face="bold"))
dev.off()
