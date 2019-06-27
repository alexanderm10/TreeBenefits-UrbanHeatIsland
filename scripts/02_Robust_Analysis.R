# Analyzing effects of trees on the Urban Heat Island effect
library(sp); library(rgdal); library(raster); library(rgeos); library(maps)
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
path.dat <- "../data_processed/"
# path.dat <- "/Volumes/GoogleDrive/My Drive/TreeBenefits_UrbanHeatIsland/data_processed/"

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
                                     '9'='flooded grassland/savanna'; 
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
yr.process <- 2011:2015
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago")
  # i=which(dat.uhi$NAME=="MEDELLIN")
  # i=which(dat.uhi$NAME=="Tokyo")
  # i=which(dat.uhi$NAME=="Nashville-Davidson")
  # i=which(dat.uhi$NAME=="Atlanta")
  # i=which(dat.uhi$NAME=="Cuernavaca")
  
  
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
    # dat.yr$temp.dev.summer2 <- dat.yr$temp.summer - mean(dat.yr$temp.summer[dat.yr$temp.n>n.lo], na.rm=T)
    # summary(dat.yr)
    
    dat.city <- rbind(dat.city, dat.yr)
    rm(dat.yr)
  }
  summary(dat.city)
  
  # If we don't have any data for this city, skip over it
  if(nrow(dat.city)==0) next
  
  # ggplot(data=dat.city) +
  #   geom_histogram(aes(x=temp.summer, fill=year))
  # ggplot(data=dat.city) +
  #   facet_wrap(~year) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=temp.summer)) +
  #   theme(legend.position="top")

  # Remove impossible values and outliers
  dat.city$temp.summer <- filter.outliers(DAT=dat.city$temp.summer, n.sigma=6)
  dat.city$temp.dev.summer <- filter.outliers(DAT=dat.city$temp.dev.summer, n.sigma=6)
  # dat.city$temp.dev.summer2 <- filter.outliers(DAT=dat.city$temp.dev.summer2, n.sigma=6)
  # dat.city$cover.tree <- filter.outliers(DAT=dat.city$cover.tree, n.sigma=6)
  # dat.city$cover.veg <- filter.outliers(DAT=dat.city$cover.veg, n.sigma=6)
  # dat.city$cover.noveg <- filter.outliers(DAT=dat.city$cover.noveg, n.sigma=6)
  
  # Now that we've excluded outliers (AGAIN), loop through and do the deviations again
  for(YEAR in unique(dat.city$year)){
    yr.ind <- which(dat.city$year==YEAR)
    ind.good <- which(dat.city$year==YEAR & dat.city$temp.n > n.lo)
    
    # Note we're just going to re-center temp.dev.summer, but temp.dev.summer2 is based off of just good cells
    dat.city[yr.ind, "temp.dev.summer"] <- dat.city[yr.ind, "temp.dev.summer"] - mean(dat.city[yr.ind, "temp.dev.summer"], na.rm=T)
    dat.city[yr.ind, "temp.dev.summer2"] <- dat.city[yr.ind, "temp.summer"] - mean(dat.city[ind.good, "temp.summer"], na.rm=T)
  }
  
  # Add a couple QAQC flags
  dat.uhi[i,"prop.missing"] <- length(which(is.na(dat.city$temp.dev.summer)))/nrow(dat.city)
  dat.uhi[i,"prop.temp.n.lo"] <- length(which(dat.city$temp.n<=n.lo & !is.na(dat.city$temp.n)))/length(which(!is.na(dat.city$temp.n)))
  
  # Remove missing values and cells with questionable temperature data for our own sanity; also require multiple years
  dat.exclude <- is.na(dat.city$temp.dev.summer2) & dat.city$temp.n<=n.lo
  # dat.city <- dat.city[,]
  summary(dat.city[!dat.exclude,]); dim(dat.city[!dat.exclude,])
  
  # Note, because we can't predict in years we don't have *any* data, we need to exclude those years entirely
  dat.city <- dat.city[dat.city$year %in% unique(dat.city$year[!dat.exclude]),]
  dat.uhi[i,"n.years"] <- length(unique(dat.city$year))
  
  # Need to redo indices now because we've screwed it up by getting rid of blank years
  dat.exclude <- is.na(dat.city$temp.dev.summer2) & dat.city$temp.n<=n.lo
  
  if(nrow(dat.city[!dat.exclude,])<=50*length(unique(dat.city$year)) | length(unique(dat.city$year[!dat.exclude]))<2) next
  
  # dim(dat.city)
  
  # ggplot(data=dat.city[,]) +
  #   facet_wrap(~year) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=temp.summer))
  # ggplot(data=dat.city[,]) +
  #   facet_wrap(~year) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=cover.tree))
  # ---------------------------
  
  # ---------------------------
  # Fit models to predict temperature based on elevation and land cover
  # ---------------------------
  # Because we'll attempt a log model
  dat.city[dat.city$cover.tree==0, "cover.tree"] <- 0.1
  dat.city[dat.city$cover.veg==0, "cover.veg"] <- 0.1
  dat.city[dat.city$cover.noveg==0, "cover.noveg"] <- 0.1
  
  # -------
  # Simple generalized additive models that work for a single year of data 
  # -------
  mod.gam.lin <- gam(temp.summer ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=dat.city[!dat.exclude,])
  mod.gam.log <- gam(temp.summer ~ log(cover.tree) + log(cover.veg) + elevation + s(x,y) + as.factor(year)-1, data=dat.city[!dat.exclude,])
  # mod.gam.lin <- gam(temp.dev.summer2 ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year), data=dat.city)
  # mod.gam.log <- gam(temp.dev.summer2 ~ log(cover.tree) + log(cover.veg) + elevation + s(x,y) + as.factor(year), data=dat.city)
  

  sum.lin <- summary(mod.gam.lin)
  sum.log <- summary(mod.gam.log)
  
  dat.city$pred.lin <- predict(mod.gam.lin, newdata=dat.city)
  dat.city$pred.log <- predict(mod.gam.log, newdata=dat.city)
  dat.city$res.lin <- dat.city$pred.lin - dat.city$temp.summer
  dat.city$res.log <- dat.city$pred.log - dat.city$temp.summer
  dat.uhi[i, "R2.lin"] <- sum.lin$r.sq
  dat.uhi[i, "R2.log"] <- sum.log$r.sq
  
  # range(dat.city$res.lin, na.rm=T); range(dat.city$res.log, na.rm=T)
  # mean(dat.city$res.lin, na.rm=T); sd(dat.city$res.lin, na.rm=T)
  # mean(dat.city$res.log, na.rm=T); sd(dat.city$res.log, na.rm=T)
  # plot(pred.log ~ pred.lin, data=dat.city)
  # plot(pred.lin ~ temp.summer, data=dat.city)
  # plot(pred.log ~ temp.summer, data=dat.city)
  # AIC(mod.gam.lin, mod.gam.log)
  # sum.lin$r.sq; sum.log$r.sq
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
  
  # if(sum.lin$r.sq >= sum.log$r.sq) {
  if(max(abs(dat.city$res.lin), na.rm=T) <= max(abs(dat.city$res.log), na.rm=T)) {
      mod.gam <- mod.gam.lin
    
    dat.uhi[i, "model.type"] <- "linear"
    dat.uhi[i, "gam.r2"] <- sum.lin$r.sq
    dat.uhi[i, "gam.dev.exp"] <- sum.lin$dev.expl
    dat.uhi[i, "elevation.slope"] <- sum.lin$p.coeff["elevation"]
    dat.uhi[i, "tree.pval"] <- sum.lin$p.pv["cover.tree"]
    dat.uhi[i, "veg.pval"] <- sum.lin$p.pv["cover.veg"]
    dat.uhi[i, "noveg.pval"] <- sum.lin$p.pv["cover.noveg"]
    
  } else {
    mod.gam <- mod.gam.log
    
    dat.uhi[i, "model.type"] <- "log-effect"
    dat.uhi[i, "gam.r2"] <- sum.log$r.sq
    dat.uhi[i, "gam.dev.exp"] <- sum.log$dev.expl
    dat.uhi[i, "elevation.slope"] <- sum.log$p.coeff["elevation"]
    dat.uhi[i, "tree.pval"] <- sum.log$p.pv["log(cover.tree)"]
    dat.uhi[i, "veg.pval"] <- sum.log$p.pv["log(cover.noveg)"]
    dat.uhi[i, "noveg.pval"] <- sum.log$p.pv["log(cover.veg)"]
    
  }

  gam.summary <- summary(mod.gam)
  dat.city$gam.pred <- predict(mod.gam, newdata=dat.city)
  dat.city$gam.resid[!dat.exclude] <- resid(mod.gam)
  # plot(mod.gam)
  
  png(file.path(path.dat, "cities_full_sdei_v6", "analysis_all_years", paste0(dat.uhi$NAME[i], "_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
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
  # Just get rid of trees to do their raw effects
  dat.new <- dat.city
  dat.new$cover.tree <- min(dat.city$cover.tree, na.rm=T)
  dat.city$pred.trees <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.trees <- dat.city$pred.trees - dat.city$gam.pred 

  # Covert trees to no veg
  dat.new <- dat.city
  dat.new$cover.noveg <- dat.new$cover.noveg + (dat.new$cover.tree - min(dat.city$cover.tree, na.rm=T))
  dat.new$cover.tree <- min(dat.city$cover.tree, na.rm=T)
  dat.city$pred.trees2noveg <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.trees2noveg <- dat.city$pred.trees2noveg - dat.city$gam.pred 
  # ---------
  
  # ---------
  # Scenario 2: Trees to Other Veg
  # ---------
  # Covert trees to no veg
  dat.new <- dat.city
  dat.new$cover.veg <- dat.new$cover.veg + (dat.new$cover.tree - min(dat.city$cover.tree, na.rm=T))
  dat.new$cover.tree <- min(dat.city$cover.tree, na.rm=T)
  dat.city$pred.trees2veg <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.trees2veg <- dat.city$pred.trees2veg - dat.city$gam.pred 
  # ---------
  
  # ---------
  # Scenario 3: Other Veg to No Veg
  # ---------
  # Just get rid of veg to do their raw effects
  dat.new <- dat.city
  dat.new$cover.veg <- min(dat.city$cover.veg, na.rm=T)
  dat.city$pred.veg <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.veg <- dat.city$pred.veg - dat.city$gam.pred 
  
  
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
  vars.agg <- c("temp.summer", "cover.tree", "cover.veg", "cover.noveg", "gam.pred", "diff.trees", "diff.veg",  "diff.trees2noveg", "diff.trees2veg", "diff.veg2noveg")
  # Calculating temperature deviation based off of the regional mean because the current method can be highly biased to uneven representation
  city.mean <- aggregate(dat.city[,vars.agg],
                         by=dat.city[,c("Name", "location", "x", "y")], 
                         FUN=mean, na.rm=T)
  summary(city.mean)
  
  city.summary <- aggregate(city.mean[,vars.agg], by=city.mean[,c("Name", "location")], FUN=mean, na.rm=T)
  summary(city.summary)
  
  vars.diff <- c("temp.summer", "cover.tree", "cover.veg", "cover.noveg")
  dat.ref <- city.summary[city.summary$location==0,]
    
  for(VAR in vars.diff){
    city.summary[, paste0("d.", VAR, ".buff")] <- city.summary[,VAR] - dat.ref[,VAR]
  }
  # city.summary
  
  
  # ggplot(data=city.summary) +
  #   geom_bar(aes(x=as.factor(location), y=d.temp.summer.buff, fill=as.factor(location)), stat="identity", position="dodge")

  # ggplot(data=city.summary) +
  #   geom_line(aes(x=year, y=cover.tree, color=as.factor(location)))
  
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
#   ggplot(data=city.cover) +
#     geom_bar(aes(x=cover.type, y=cover.mean, fill=as.factor(location)), stat="identity", position="dodge")
#   ggplot(data=city.cover) +
#     geom_bar(aes(x=cover.type, y=cover.change, fill=as.factor(location)), stat="identity", position="dodge")
  # ----------
  
  # ----------
  # Saving summary stats 
  # ----------
  dat.uhi[i, "temp.summer.city"] <- city.summary[city.summary$location==0,"temp.summer"]
  dat.uhi[i, "cover.noveg.city"] <- city.summary[city.summary$location==0,"cover.noveg"]
  dat.uhi[i, "cover.veg.city"] <- city.summary[city.summary$location==0,"cover.veg"]
  dat.uhi[i, "cover.tree.city"] <- city.summary[city.summary$location==0,"cover.tree"]
  dat.uhi[i, "cover.noveg.buff"] <- city.summary[city.summary$location==buff.use,"cover.noveg"]
  dat.uhi[i, "cover.veg.buff"] <- city.summary[city.summary$location==buff.use,"cover.veg"]
  dat.uhi[i, "cover.tree.buff"] <- city.summary[city.summary$location==buff.use,"cover.tree"]
  
  # Regional tree cover stats
  dat.uhi[i, "cover.tree.min"] <- min(city.mean[city.mean$location==0,"cover.tree"], na.rm=T)
  dat.uhi[i, "cover.tree.10"] <- quantile(city.mean[city.mean$location==0,"cover.tree"], 0.1, na.rm=T)
  dat.uhi[i, "cover.tree.med"] <- median(city.mean[city.mean$location==0,"cover.tree"], na.rm=T)
  dat.uhi[i, "cover.tree.90"] <- quantile(city.mean[city.mean$location==0,"cover.tree"], 0.9, na.rm=T)
  dat.uhi[i, "cover.tree.max"] <- max(city.mean[city.mean$location==0,"cover.tree"], na.rm=T)
  
  dat.uhi[i, "d.temp.summer.buff"] <- -city.summary[city.summary$location==buff.use,"d.temp.summer.buff"]
  dat.uhi[i, "d.cover.tree.buff"] <- -city.summary[city.summary$location==buff.use,"d.cover.tree.buff"]
  dat.uhi[i, "d.cover.veg.buff"] <- -city.summary[city.summary$location==buff.use,"d.cover.veg.buff"]
  dat.uhi[i, "d.cover.noveg.buff"] <- -city.summary[city.summary$location==buff.use,"d.cover.noveg.buff"]
  dat.uhi[i, "Tdiff.trees.city"] <- city.summary[city.summary$location==0,"diff.trees"]
  dat.uhi[i, "Tdiff.veg.city"] <- city.summary[city.summary$location==0,"diff.veg"]
  dat.uhi[i, "Tdiff.trees2noveg.city"] <- city.summary[city.summary$location==0,"diff.trees2noveg"]
  dat.uhi[i, "Tdiff.trees2veg.city"] <- city.summary[city.summary$location==0,"diff.trees2veg"]
  dat.uhi[i, "Tdiff.veg2noveg.city"] <- city.summary[city.summary$location==0,"diff.veg2noveg"]
  
  # Quick calculation of veg trends
  # yr.min <- min(city.summary$year); yr.max <- max(city.summary$year)
  trend.tree <- lm(cover.tree ~ year*as.factor(location)-year, data=dat.city[dat.city$location %in% c(0, buff.use),])
  trend.veg <- lm(cover.veg ~ year*as.factor(location)-year, data=dat.city[dat.city$location %in% c(0, buff.use),])
  trend.noveg <- lm(cover.noveg ~ year*as.factor(location)-year, data=dat.city[dat.city$location %in% c(0, buff.use),])
  sum.trend.tree <- summary(trend.tree)
  sum.trend.veg <- summary(trend.veg)
  sum.trend.noveg <- summary(trend.noveg)
  
  # Note: in some cases, may not be able to estimate city
  ind.city <- which(dimnames(sum.trend.tree$coefficients)[[1]]==paste0("year:as.factor(location)",0))
  ind.buff <- which(dimnames(sum.trend.tree$coefficients)[[1]]==paste0("year:as.factor(location)",buff.use))
  
  if(length(ind.city)>0){
    dat.uhi[i, "trend.cover.tree.city"] <- sum.trend.tree$coefficients[ind.city,1]
    dat.uhi[i, "trend.cover.veg.city"] <- sum.trend.veg$coefficients[ind.city,1]
    dat.uhi[i, "trend.cover.noveg.city"] <- sum.trend.noveg$coefficients[ind.city,1]
    dat.uhi[i, "p.trend.cover.tree.city"] <- sum.trend.tree$coefficients[ind.city,4]
    dat.uhi[i, "p.trend.cover.veg.city"] <- sum.trend.veg$coefficients[ind.city,4]
    dat.uhi[i, "p.trend.cover.noveg.city"] <- sum.trend.noveg$coefficients[ind.city,4]
  } 
  if(length(ind.buff)>0){
    dat.uhi[i, "trend.cover.tree.buff"] <- sum.trend.tree$coefficients[ind.buff,1]
    dat.uhi[i, "trend.cover.veg.buff"] <- sum.trend.veg$coefficients[ind.buff,1]
    dat.uhi[i, "trend.cover.noveg.buff"] <- sum.trend.noveg$coefficients[ind.buff,1]
    dat.uhi[i, "p.trend.cover.tree.buff"] <- sum.trend.tree$coefficients[ind.buff,4]
    dat.uhi[i, "p.trend.cover.veg.buff"] <- sum.trend.veg$coefficients[ind.buff,4]
    dat.uhi[i, "p.trend.cover.noveg.buff"] <- sum.trend.noveg$coefficients[ind.buff,4]
    
  } 
  
  # ----------
  
  # -------------------------
  write.csv(dat.city, file.path(path.dat, "cities_full_sdei_v6", "analysis_all_years", paste0(dat.uhi$NAME[i], "_data_full_analyzed.csv")))
  rm(dat.city)
}
dat.uhi$WWF_BIOME <- as.factor(dat.uhi$WWF_BIOME)
dat.uhi$model.type <- as.factor(dat.uhi$model.type)
summary(dat.uhi)

write.csv(dat.uhi, "../data_processed/analysis_cities_summary_sdei_v6.csv", row.names=F)
# --------------------------------------------------------------
# dat.uhi[is.na(dat.uhi$gam.r2),]

