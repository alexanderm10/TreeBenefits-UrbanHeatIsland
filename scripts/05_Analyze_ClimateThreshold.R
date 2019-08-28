# Analyzing how much trees push temperatures away from the 35˚C Wet Bulb Temperature Threshold
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
path.dat <- "../data_processed/"

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
summary(dat.uhi)
dat.uhi$Twb35pred

summary(dat.uhi[!is.na(dat.uhi$p.TwbAbv35.obs) & dat.uhi$p.TwbAbv35.tree2noveg>0,c("NAME", "WWF_ECO", "WWF_BIOME", "temp.summer.city", "p.TwbAbv35.obs", "p.TwbAbv35.tree2noveg")])

dat.uhi[!is.na(dat.uhi$p.TwbAbv35.obs) & dat.uhi$p.TwbAbv35.tree2noveg>1e-3,c("NAME", "WWF_ECO", "WWF_BIOME", "temp.summer.city", "p.TwbAbv35.obs", "p.TwbAbv35.tree2noveg")]
