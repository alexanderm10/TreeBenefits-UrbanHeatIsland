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
yr.process <- 2011:2015
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago")
  # i=which(dat.uhi$NAME=="MEDELLIN")
  # i=which(dat.uhi$NAME=="Tokyo")
  # i=which(dat.uhi$NAME=="Nashville-Davidson")
  # i=which(dat.uhi$NAME=="Atlanta")
  # i=which(dat.uhi$NAME=="Cuernavaca")
  
  dat.city <- read.csv(file.path(path.dat, "cities_full_sdei_v6", "analysis_all_years", paste0(dat.uhi$NAME[i], "_data_full_analyzed.csv")))
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
  summary(dat.city)
  
  dat.city$Temp.Summer.C <- dat.city$temp.summer-273.16
  dat.city$Twb.summer.adj <- calc.wetbulb(TEMP=dat.city$Temp.Summer.C, RH=dat.city$RH.adj)
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
}
