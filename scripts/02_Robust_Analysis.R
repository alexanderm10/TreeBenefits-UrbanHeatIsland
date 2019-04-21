# Analyzing effects of trees on the Urban Heat Island effect
library(sp); library(rgdal); library(raster); library(rgeos); library(maps)
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
# path.dat <- "../data_processed/"
path.dat <- "/Volumes/GoogleDrive/My Drive/TreeBenefits_UrbanHeatIsland/data_processed/"

# --------------------------------------------------------------
# Run models
# --------------------------------------------------------------
dat.uhi <- read.csv(file.path(path.dat, "cities_summary_sdei_v6.csv"))
# dat.uhi <- dat.uhi[!is.na(dat.uhi$elev.mean), ]
dat.uhi$temp.diff <- dat.uhi$temp.max - dat.uhi$temp.min
dat.uhi$tree.diff <- dat.uhi$tree.max - dat.uhi$tree.min
summary(dat.uhi)
dim(dat.uhi)
# dat.uhi[grepl("[?]", dat.uhi$NAME),"NAME"]

uhi.sp <- SpatialPointsDataFrame(coords=dat.uhi[,c("LONGITUDE", "LATITUDE")], data=dat.uhi, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
summary(uhi.sp)

# Getting some baseline ecoregion info: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
ecoregions <- readOGR("../data_raw/wwf_biomes_official/wwf_terr_ecos.shp")
# ecoregions <- readOGR("../data_raw/global200ecoregions/g200_terr.shp")
ecoregions$biome.name <- car::recode(ecoregions$BIOME, "'1'='tropical moist broadleaf forest'; 
                                     '2'='tropical dry broadleaf forest'; 
                                     '3'='tropical coniferous forest';
                                     '4'='temperate broadleaf/mixed forest'; 
                                     '5'='temperate coniferous forest'; 
                                     '6'='boreal forest/taiga'; 
                                     '7'='tropical grassland/savannas'; 
                                     '8'='temperate grassland/savanna'; 
                                     '9'='flodded grassland/savanna'; 
                                     '10'='montane grassland/savanna'; 
                                     '11'='tundra'; 
                                     '12'='mediterranean'; 
                                     '13'='desert/xeric shrublands'; 
                                     '14'='mangroves'")
unique(data.frame(ecoregions[ecoregions$BIOME==4, "G200_REGIO"]))
summary(ecoregions)
# plot(ecoregions)

# Filter outliers
filter.outliers <- function(DAT, n.sigma=6, max.iters=10){
  tmp.x <- mean(DAT, na.rm=T); tmp.sd <- sd(DAT, na.rm=T)
  ITER=1
  while(length(which(DAT < tmp.x-n.sigma*tmp.sd | DAT > tmp.x+n.sigma*tmp.sd))>0 & ITER<max.iters){
    DAT[DAT < tmp.x-n.sigma*tmp.sd]  <- NA
    DAT[DAT > tmp.x+n.sigma*tmp.sd]  <- NA
    
    tmp.x <- mean(DAT, na.rm=T); tmp.sd <- sd(DAT, na.rm=T)
    ITER=ITER+1
  } # end while loop
  
  return(DAT)
} # End function

# New plan for quantifying effects of trees on UHI: 
# apply mean temperature of treeless area to whole area and calculate the difference
# Set some thresholds for analyses
n.lo = 2
buff.use = 10

pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
# yr.process <- 2011:2015
yr.process=2013
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago") # MEDELLIN
  # i=which(dat.uhi$NAME=="MEDELLIN")
  # i=which(dat.uhi$NAME=="Tokyo")
  # i=which(dat.uhi$NAME=="Nashville-Davidson")
  
  
  setTxtProgressBar(pb, i)
  biome <- extract(ecoregions, uhi.sp[i,], fun=modal)
  dat.uhi[i,"WWF_ECO"] <- biome$ECO_NAME
  dat.uhi[i,"WWF_BIOME"] <- biome$biome.name
  
  # ---------------------------
  # Load and check the data
  # ---------------------------
  dat.city <- data.frame()
  for(YEAR in yr.process){
    # If we don't have any data for this year, skip it
    if(!file.exists(file.path(path.dat, "cities_full_sdei_v6", YEAR, paste0(dat.uhi$NAME[i], "_data_full.csv")))) next
    
    dat.yr <- read.csv(file.path(path.dat, "cities_full_sdei_v6", YEAR, paste0(dat.uhi$NAME[i], "_data_full.csv")))
    dat.yr$year <- YEAR
    dat.yr$temp.dev.summer2 <- dat.yr$temp.summer - mean(dat.yr$temp.summer[dat.yr$temp.n>n.lo], na.rm=T)
    # summary(dat.yr)
    
    dat.city <- rbind(dat.city, dat.yr)
    rm(dat.yr)
  }
  summary(dat.city)
  
  # If we don't have any data for this city, skip over it
  if(nrow(dat.city)==0) next
  
  # Remove impossible values and outliers
  # dat.city$temp.summer <- filter.outliers(DAT=dat.city$temp.summer, n.sigma=6)
  # dat.city$temp.dev.summer <- filter.outliers(DAT=dat.city$temp.dev.summer, n.sigma=6)
  # dat.city$cover.tree <- filter.outliers(DAT=dat.city$cover.tree, n.sigma=6)
  # dat.city$cover.veg <- filter.outliers(DAT=dat.city$cover.veg, n.sigma=6)
  # dat.city$cover.noveg <- filter.outliers(DAT=dat.city$cover.noveg, n.sigma=6)
  
  # Add a couple QAQC flags
  dat.uhi[i,"prop.missing"] <- length(which(is.na(dat.city$temp.dev.summer)))/nrow(dat.city)
  dat.uhi[i,"prop.temp.n.lo"] <- length(which(dat.city$temp.n<=n.lo & !is.na(dat.city$temp.n)))/length(which(!is.na(dat.city$temp.n)))
  
  # Remove missing values and cells with questionable temperature data for our own sanity
  dat.city <- dat.city[!is.na(dat.city$temp.dev.summer2) & dat.city$temp.n>n.lo,]
  summary(dat.city)
  if(nrow(dat.city)<=10) next
  
  # dim(dat.city)
  
  # ggplot(data=dat.city[city.filter,]) +
  #   facet_wrap(~year) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=temp.summer))
  # ggplot(data=dat.city[city.filter,]) +
  #   facet_wrap(~year) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=cover.tree))
  # ---------------------------
  
  # ---------------------------
  # Fit models to predict temperature based on elevation and land cover
  # ---------------------------
  # Because we'll attempt 
  dat.city[dat.city$cover.tree==0, "cover.tree"] <- 0.1
  dat.city[dat.city$cover.veg==0, "cover.veg"] <- 0.1
  dat.city[dat.city$cover.noveg==0, "cover.noveg"] <- 0.1
  
  # -------
  # Simple generalized additive models that work for a single year of data 
  # -------
  mod.gam.lin <- gam(temp.dev.summer2 ~ cover.tree + cover.veg + cover.noveg + elevation + s(x,y) + as.factor(year), data=dat.city)
  mod.gam.log <- gam(temp.dev.summer2 ~ log(cover.tree) + log(cover.veg) + log(cover.noveg) + elevation + s(x,y) + as.factor(year), data=dat.city)
  # AIC(mod.gam.lin, mod.gam.log)

  sum.lin <- summary(mod.gam.lin)
  sum.log <- summary(mod.gam.log)
  # -------
  
  # -------
  # More complicated generalized additive *mixed* model with year as a random rather than fixed effect
  # Note: this would be the more statistically appropriate model, but may not work
  # -------
  # mod.gam.lin <- gamm(temp.dev.summer2 ~ cover.tree + cover.veg + cover.noveg + elevation + s(x,y), random=list(year=~1), data=dat.city)
  # mod.gam.log <- gamm(temp.dev.summer2 ~ log(cover.tree) + log(cover.veg) + log(cover.noveg) + elevation + s(x,y), random=list(year=~1), data=dat.city)
  # 
  # AIC(mod.gam.lin, mod.gam.log)
  # 
  # sum.lin <- summary(mod.gam.lin$gam)
  # sum.log <- summary(mod.gam.log$gam)
  # -------
  
  dat.uhi[i, "tree.slope.lin"] <- sum.lin$p.coeff["cover.tree"]
  dat.uhi[i, "tree.slope.log"] <- sum.log$p.coeff["log(cover.tree)"]
  dat.uhi[i, "veg.slope.lin"] <- sum.lin$p.coeff["cover.veg"]
  dat.uhi[i, "veg.slope.log"] <- sum.log$p.coeff["log(cover.veg)"]
  dat.uhi[i, "noveg.slope.lin"] <- sum.lin$p.coeff["cover.noveg"]
  dat.uhi[i, "noveg.slope.log"] <- sum.log$p.coeff["log(cover.noveg)"]
  
  if(sum.lin$r.sq >= sum.log$r.sq) {
    mod.gam <- mod.gam.lin
    
    dat.uhi[i, "model.type"] <- "linear"
    dat.uhi[i, "gam.r2"] <- sum.lin$r.sq
    dat.uhi[i, "gam.dev.exp"] <- sum.lin$dev.expl
    dat.uhi[i, "elevation.slope"] <- sum.lin$p.coeff["elevation"]
    dat.uhi[i, "tree.pval"] <- sum.lin$p.pv["cover.tree"]
    
  } else {
    mod.gam <- mod.gam.log
    
    dat.uhi[i, "model.type"] <- "log-effect"
    dat.uhi[i, "gam.r2"] <- sum.log$r.sq
    dat.uhi[i, "gam.dev.exp"] <- sum.log$dev.expl
    dat.uhi[i, "elevation.slope"] <- sum.log$p.coeff["elevation"]
    dat.uhi[i, "tree.pval"] <- sum.log$p.pv["log(cover.tree)"]
    
  }
  
  gam.summary <- summary(mod.gam)
  dat.city$gam.pred <- predict(mod.gam)
  dat.city$gam.resid <- resid(mod.gam)
  # plot(mod.gam)
  
  png(file.path("../data_processed/cities_full_sdei_v6", paste0(dat.uhi$NAME[i], "_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(mod.gam)
  hist(dat.city$gam.resid)
  plot(gam.resid ~ gam.pred, data=dat.city); abline(h=0, col="red")
  plot(temp.dev.summer ~ gam.pred, data=dat.city); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  # ---------------------------
  
  # ---------------------------
  # "World without trees" scenarios
  # ---------------------------
  # ---------
  # Scenario 1: Trees to No Veg
  # ---------
  dat.new <- dat.city
  dat.new$cover.noveg <- dat.new$cover.noveg + (dat.new$cover.tree - min(dat.city$cover.tree, na.rm=T))
  dat.new$cover.tree <- min(dat.city$cover.tree, na.rm=T)
  
  dat.city$pred.trees2noveg <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.trees2noveg <- dat.city$pred.trees2noveg - dat.city$gam.pred 
  # ---------
  
  # ---------
  # Scenario 2: Trees to Other Veg
  # ---------
  dat.new <- dat.city
  dat.new$cover.veg <- dat.new$cover.veg + (dat.new$cover.tree - min(dat.city$cover.tree, na.rm=T))
  dat.new$cover.tree <- min(dat.city$cover.tree, na.rm=T)
  
  dat.city$pred.trees2veg <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.trees2veg <- dat.city$pred.trees2veg - dat.city$gam.pred 
  # ---------
  
  # ---------
  # Scenario 3: Other Veg to No Veg
  # ---------
  dat.new <- dat.city
  dat.new$cover.noveg <- dat.new$cover.noveg + (dat.new$cover.veg - min(dat.city$cover.veg, na.rm=T))
  dat.new$cover.veg <- min(dat.city$cover.veg, na.rm=T)
  
  dat.city$pred.veg2noveg <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.veg2noveg <- dat.city$pred.veg2noveg - dat.city$gam.pred 
  # ---------
  # summary(dat.city)
  
  # ---------------------------
  
  
  # -------------------------
  # Doing some summary statistics of the city data
  # -------------------------
  vars.agg <- c("temp.summer", "cover.tree", "cover.veg", "cover.noveg", "gam.pred", "diff.trees2noveg", "diff.trees2veg", "diff.veg2noveg")
  city.summary <- aggregate(dat.city[,vars.agg], 
                            by=dat.city[,c("location", "year")],
                            FUN=mean, na.rm=T)
  summary(city.summary)
  
  vars.diff <- c("temp.summer", "cover.tree", "cover.veg", "cover.noveg")
  for(YR in unique(city.summary$year)){
    yr.ind <- which(city.summary$year==YEAR)
    dat.ref <- city.summary[city.summary$year==YEAR & city.summary$location==0,]
    
    for(VAR in vars.diff){
      city.summary[yr.ind, paste0("d.", VAR, ".buff")] <- city.summary[yr.ind,VAR] - dat.ref[,VAR]
    }
  }
  summary(city.summary)
  
  # ggplot(data=city.summary) +
  #   geom_bar(aes(x=as.factor(location), y=d.temp.summer, fill=as.factor(location)), stat="identity", position="dodge")
  
  # ----------
  # Exploratory graphing of select cities 
  # ----------
  # city.cover <- stack(city.summary[,c("cover.tree", "cover.veg", "cover.noveg")])
  # names(city.cover) <- c("cover.mean", "cover.type")
  # city.cover$cover.type <- car::recode(city.cover$cover.type, "'cover.tree'='veg-tree'; 'cover.veg'='veg-other'; 'cover.noveg'='no veg'")
  # city.cover[,c("location", "year")] <- city.summary[,c("location", "year")]
  # city.cover$cover.change <- stack(city.summary[,paste0("d.",c("cover.tree.buff", "cover.veg.buff", "cover.noveg.buff"))])$values
  # summary(city.cover)
  # 
  # ggplot(data=city.cover) +
  #   geom_bar(aes(x=cover.type, y=cover.mean, fill=as.factor(location)), stat="identity", position="dodge")
  # ggplot(data=city.cover) +
  #   geom_bar(aes(x=cover.type, y=cover.change, fill=as.factor(location)), stat="identity", position="dodge")
  # ----------
  
  # ----------
  # Saving summary stats 
  # ----------
  dat.uhi[i, "temp.summer.city"] <- mean(city.summary[city.summary$location==0,"temp.summer"], na.rm=T)
  dat.uhi[i, "cover.noveg.city"] <- mean(city.summary[city.summary$location==0,"cover.noveg"], na.rm=T)
  dat.uhi[i, "cover.veg.city"] <- mean(city.summary[city.summary$location==0,"cover.veg"], na.rm=T)
  dat.uhi[i, "cover.tree.city"] <- mean(city.summary[city.summary$location==0,"cover.tree"], na.rm=T)
  dat.uhi[i, "cover.noveg.buff"] <- mean(city.summary[city.summary$location==buff.use,"cover.noveg"], na.rm=T)
  dat.uhi[i, "cover.veg.buff"] <- mean(city.summary[city.summary$location==buff.use,"cover.veg"], na.rm=T)
  dat.uhi[i, "cover.tree.buff"] <- mean(city.summary[city.summary$location==buff.use,"cover.tree"], na.rm=T)
  
  # Regional tree cover stats
  dat.uhi[i, "cover.tree.min"] <- min(city.summary[city.summary$location==0,"cover.tree"], na.rm=T)
  dat.uhi[i, "cover.tree.10"] <- quantile(city.summary[city.summary$location==0,"cover.tree"], 0.1, na.rm=T)
  dat.uhi[i, "cover.tree.med"] <- median(city.summary[city.summary$location==0,"cover.tree"], na.rm=T)
  dat.uhi[i, "cover.tree.90"] <- quantile(city.summary[city.summary$location==0,"cover.tree"], 0.9, na.rm=T)
  dat.uhi[i, "cover.tree.max"] <- max(city.summary[city.summary$location==0,"cover.tree"], na.rm=T)
  
  dat.uhi[i, "d.temp.summer.buff"] <- -mean(city.summary[city.summary$location==buff.use,"d.temp.summer.buff"], na.rm=T)
  dat.uhi[i, "d.cover.tree.buff"] <- -mean(city.summary[city.summary$location==buff.use,"d.cover.tree.buff"], na.rm=T)
  dat.uhi[i, "d.cover.veg.buff"] <- -mean(city.summary[city.summary$location==buff.use,"d.cover.veg.buff"], na.rm=T)
  dat.uhi[i, "d.cover.noveg.buff"] <- -mean(city.summary[city.summary$location==buff.use,"d.cover.noveg.buff"], na.rm=T)
  dat.uhi[i, "Tdiff.trees2noveg.city"] <- mean(city.summary[city.summary$location==0,"diff.trees2noveg"], na.rm=T)
  dat.uhi[i, "Tdiff.trees2veg.city"] <- mean(city.summary[city.summary$location==0,"diff.trees2veg"], na.rm=T)
  dat.uhi[i, "Tdiff.veg2noveg.city"] <- mean(city.summary[city.summary$location==0,"diff.veg2noveg"], na.rm=T)
  
  yr.min <- min(city.summary$year); yr.max <- max(city.summary$year)
  dat.uhi[i, "d.cover.tree.time.city"] <- city.summary[city.summary$location==0 & city.summary$year==yr.max,"cover.tree"] - city.summary[city.summary$location==0 & city.summary$year==yr.min,"cover.tree"]
  dat.uhi[i, "d.cover.veg.time.city"] <- city.summary[city.summary$location==0 & city.summary$year==yr.max,"cover.veg"] - city.summary[city.summary$location==0 & city.summary$year==yr.min,"cover.veg"]
  dat.uhi[i, "d.cover.noveg.time.city"] <- city.summary[city.summary$location==0 & city.summary$year==yr.max,"cover.noveg"] - city.summary[city.summary$location==0 & city.summary$year==yr.min,"cover.noveg"]
  
  dat.uhi[i, "d.cover.tree.time.buff"] <- city.summary[city.summary$location==buff.use & city.summary$year==yr.max,"cover.tree"] - city.summary[city.summary$location==buff.use & city.summary$year==yr.min,"cover.tree"]
  dat.uhi[i, "d.cover.veg.time.buff"] <- city.summary[city.summary$location==buff.use & city.summary$year==yr.max,"cover.veg"] - city.summary[city.summary$location==buff.use & city.summary$year==yr.min,"cover.veg"]
  dat.uhi[i, "d.cover.noveg.time.buff"] <- city.summary[city.summary$location==buff.use & city.summary$year==yr.max,"cover.noveg"] - city.summary[city.summary$location==buff.use & city.summary$year==yr.min,"cover.noveg"]
  
  
  # ----------
  
  # -------------------------
  
  rm(dat.city)
}
dat.uhi$WWF_BIOME <- as.factor(dat.uhi$WWF_BIOME)
dat.uhi$model.type <- as.factor(dat.uhi$model.type)
summary(dat.uhi)

write.csv(dat.uhi, "../data_processed/analysis_cities_summary_sdei_v6.csv", row.names=F)
# --------------------------------------------------------------
# dat.uhi[is.na(dat.uhi$gam.r2),]

