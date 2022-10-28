library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)


# file paths for where to put the processed data
# path.cities <- "../data_processed/data_cities_all"
path.cities <- "/Volumes/GoogleDrive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis/data_processed/data_cities_all"
if(!dir.exists(path.cities)) dir.create(path.cities, recursive=T, showWarnings = F)
file.cityAll.stats <- file.path(path.cities, "../city_stats_all.csv")

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- "/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output"

# Some color palettes for later
grad.temp <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
grad.elev <- c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4")
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange



# Lets add the ecoregion for each city; accessed 27 Oct 2022 9:30 a.m.
# SDEI shapefile: https://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013/data-download# # NOTE: REQUIRES LOGIN
# ecoregion file: https://www.worldwildlife.org/publications/global-200
sdei.urb <- vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
sdei.urb <- sdei.urb[sdei.urb$ES00POP>100e3 & sdei.urb$SQKM_FINAL>100,]
summary(sdei.urb)


ecoregions <- vect("../data_raw/global200ecoregions/g200_terr.shp")
ecoregions$biome.name <- car::recode(ecoregions$G200_BIOME, "'1'='tropical moist broadleaf forest'; 
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
summary(ecoregions)


# If we don't have our summary file yet, create it and create all the column names we're going to want
if(!file.exists(file.cityAll.stats)){
  # cityAll.stats <- read.csv("../sdei-global-uhi-2013.csv")
  cols.keep <-  c("ISOURBID", "ISO3", "URBID", "NAME", "LATITUDE", "LONGITUDE", "ES00POP")
  cityAll.stats <- data.frame(sdei.urb[, cols.keep])[,1:length(cols.keep)]
  # head(cityAll.stats)
  
  # Some summary stats about the inputs: 
  # - number of pixels, mean LST, cover, elev --> for ranges, give range across entire dataset to indicate range of values used in full model
  cityAll.stats[,c("biome", "n.pixels", "LST.mean", "LST.sd", "LST.min", "LST.max", "tree.mean", "tree.sd", "tree.min", "tree.max", "veg.mean", "veg.sd", "veg.min", "veg.max", "elev.mean", "elev.sd", "elev.min", "elev.max")] <- NA
  
  # Save the key info from the full model
  cityAll.stats[,c("model.R2adj", "model.tree.slope", "model.veg.slope", "model.elev.slope", "model.tree.p", "model.veg.p", "model.elev.p")] <- NA
  
  # For each variable calculate the overall trends --> this will need to be separate from the pixel-by-pixel analysis, although we'll do and save that as well
  cityAll.stats[,c("trend.LST.slope", "trend.LST.slope.sd", "trend.LST.p")] <- NA
  cityAll.stats[,c("trend.tree.slope", "trend.tree.slope.sd", "trend.tree.p")] <- NA
  cityAll.stats[,c("trend.veg.slope", "trend.veg.slope.sd", "trend.veg.p")] <- NA
  
  # Also look at the correlation between warming and change in tree & veg cover
  cityAll.stats[,c("corr.LST.tree.slope", "corr.LST.tree.p", "corr.LST.tree.Rsq")] <- NA
  cityAll.stats[,c("corr.LST.veg.slope", "corr.LST.veg.p", "corr.LST.veg.Rsq")] <- NA
  cityAll.stats[,c("corr.tree.veg.slope", "corr.tree.veg.p", "corr.tree.veg.Rsq")] <- NA
  
  summary(cityAll.stats)
  dim(cityAll.stats)
  
  write.csv(cityAll.stats, file.cityAll.stats, row.names=F)  
}

cityAll.stats <- read.csv(file.cityAll.stats)
summary(cityAll.stats); dim(cityAll.stats)

# Get a list of the files that are done
files.elev <- dir(path.EEout, "elevation")
files.lst <- dir(path.EEout, "LST_Day_Tmean")
files.tree <- dir(path.EEout, "PercentTree")
files.veg <- dir(path.EEout, "PercentOtherVeg")
length(files.elev); length(files.lst); length(files.tree); length(files.veg)

# Figure out which cities have all the layers needed ot be analyzed
cities.elev <- unlist(lapply(files.elev, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.lst <- unlist(lapply(files.lst, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.tree <- unlist(lapply(files.tree, FUN=function(x){strsplit(x, "_")[[1]][1]}))
cities.veg <- unlist(lapply(files.veg, FUN=function(x){strsplit(x, "_")[[1]][1]}))


citiesDone <- cities.lst[cities.lst %in% cities.elev & cities.lst %in% cities.tree & cities.lst %in% cities.veg]
length(citiesDone)

# Now compare the done list to what needs to be analyzed
citiesAnalyze <- citiesDone[citiesDone %in% cityAll.stats$ISOURBID[is.na(cityAll.stats$model.R2adj)]]
length(citiesAnalyze)

for(CITY in citiesAnalyze){
  # # Good Test Cities: Sydney (AUS66430)
  # CITY="AUS66430"
  row.city <- which(cityAll.stats$ISOURBID==CITY)
  print(CITY)
  dir.create(file.path(path.cities, CITY), recursive = T, showWarnings = F)
  
  sp.city <- buffer(sdei.urb[sdei.urb$ISOURBID==CITY, ], width=10e3)
  biome <- terra::intersect(ecoregions, sp.city)
  # data.frame(biome)
  if(nrow(biome)>0){ # Some aren't quite aligning-- we'll figure those out later
    if(nrow(biome)==1){
      cityAll.stats$biome[row.city] <- biome$biome.name
    } else { 
      biome$area <- expanse(biome)
      biome.sum <- aggregate(area ~ biome.name, data=biome, FUN=sum)
      
      if(nrow(biome.sum)>1){
        cityAll.stats$biome[row.city] <- biome.sum$biome.name[biome.sum$area==max(biome.sum$area)]
      } else {
        cityAll.stats$biome[row.city] <- biome.sum$biome.name
      }
      
      rm(biome.sum)
    } # End if/else
  } # End skipping biomes that don't exist
  elevCity <- raster(file.path(path.EEout, paste0(CITY, "_elevation.tif")))
  lstCity <- brick(file.path(path.EEout, paste0(CITY, "_LST_Day_Tmean.tif")))-273.15
  treeCity <- brick(file.path(path.EEout, paste0(CITY, "_Vegetation_PercentTree.tif")))
  vegCity <- brick(file.path(path.EEout, paste0(CITY, "_Vegetation_PercentOtherVeg.tif")))
  
  # lst.mean <- mean(lstCity)
  # tree.mean <- mean(treeCity)
  # veg.mean <- mean(vegCity)
  # par(mfrow=c(2,2))
  # plot(elevCity); plot(lst.mean); plot(tree.mean); plot(veg.mean)
  
  coordsCity <- data.frame(coordinates(lstCity))
  coordsCity$location <- paste0("x", coordsCity$x, "y", coordsCity$y)
  
  # names(lstCity)
  # names(treeCity)
  # In case we're missing some years of LST (likely in the tropics); only pull certain layers
  layers.use <- names(treeCity)[names(treeCity) %in% names(lstCity)]
  
  # Put everything into a single data frame 
  valsCity <- stack(data.frame(getValues(lstCity[[layers.use]])))
  names(valsCity) <- c("LST_Day", "year")
  valsCity$cover.tree <- stack(data.frame(getValues(treeCity[[layers.use]])))[,1]
  valsCity$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
  valsCity$elevation <- getValues(elevCity)
  valsCity$year <- as.numeric(substr(valsCity$year, 2, 5))
  valsCity[,c("x", "y", "location")] <- coordsCity
  valsCity <- valsCity[!is.na(valsCity$LST_Day),]
  dim(valsCity); summary(valsCity)
  
  # Saving some summary stats of our inputs
  # cityAll.stats[row.city,]
  cityAll.stats$n.pixels[row.city] <- length(unique(valsCity$location))
  cityAll.stats$LST.mean[row.city] <- mean(valsCity$LST_Day, na.rm=T)
  cityAll.stats$LST.sd[row.city] <- sd(valsCity$LST_Day, na.rm=T)
  cityAll.stats$LST.min[row.city] <- min(valsCity$LST_Day, na.rm=T)
  cityAll.stats$LST.max[row.city] <- max(valsCity$LST_Day, na.rm=T)
  cityAll.stats$tree.mean[row.city] <- mean(valsCity$cover.tree, na.rm=T)
  cityAll.stats$tree.sd[row.city] <- sd(valsCity$cover.tree, na.rm=T)
  cityAll.stats$tree.min[row.city] <- min(valsCity$cover.tree, na.rm=T)
  cityAll.stats$tree.max[row.city] <- max(valsCity$cover.tree, na.rm=T)
  cityAll.stats$veg.mean[row.city] <- mean(valsCity$cover.veg, na.rm=T)
  cityAll.stats$veg.sd[row.city] <- sd(valsCity$cover.veg, na.rm=T)
  cityAll.stats$veg.min[row.city] <- min(valsCity$cover.veg, na.rm=T)
  cityAll.stats$veg.max[row.city] <- max(valsCity$cover.veg, na.rm=T)
  cityAll.stats$elev.mean[row.city] <- mean(valsCity$elevation, na.rm=T)
  cityAll.stats$elev.sd[row.city] <- sd(valsCity$elevation, na.rm=T)
  cityAll.stats$elev.min[row.city] <- min(valsCity$elevation, na.rm=T)
  cityAll.stats$elev.max[row.city] <- max(valsCity$elevation, na.rm=T)
  
  
  # Running the actual model! Woot Woot
  modCity <- gam(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
  sum.modCity <- summary(modCity)
  valsCity$gam.pred <- predict(modCity)
  valsCity$gam.resid <- resid(modCity)
  save(modCity, file=file.path(path.cities, CITY, paste0(CITY, "_Model_gam.RData")))
  # par(mfrow=c(1,1)); plot(modCity)
  
  png(file.path(path.cities, CITY, paste0(CITY, "_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(modCity)
  hist(valsCity$gam.resid)
  plot(gam.resid ~ gam.pred, data=valsCity); abline(h=0, col="red")
  plot(LST_Day ~ gam.pred, data=valsCity); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  
  # Save the key stats from the big model
  cityAll.stats$model.R2adj[row.city] <- sum.modCity$r.sq
  cityAll.stats[row.city,c("model.tree.slope", "model.veg.slope", "model.elev.slope")] <- sum.modCity$p.coeff[c("cover.tree", "cover.veg", "elevation")]
  cityAll.stats[row.city,c("model.tree.p", "model.veg.p", "model.elev.p")] <- sum.modCity$p.pv[c("cover.tree", "cover.veg", "elevation")]
  # cityAll.stats[row.city,]
  
  # Calculating pixel-based summary stats to do some trend correlations
  # For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds
  summaryCity <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation) ~ x+y+location, data=valsCity, FUN=mean)
  names(summaryCity)[names(summaryCity) %in% c("LST_Day", "cover.tree", "cover.veg")] <- c("LST.mean", "tree.mean", "veg.mean")
  summary(summaryCity)
  
  summaryCity[,c("LST.trend", "LST.p", "LST.R2")] <- NA
  summaryCity[,c("tree.trend", "tree.p", "tree.R2")] <- NA
  summaryCity[,c("veg.trend", "veg.p", "veg.R2")] <- NA
  
  pb.lms <- txtProgressBar(min=0, max=nrow(summaryCity), style=3)
  for(i in 1:nrow(summaryCity)){
  	rowsCity <- which(valsCity$location==summaryCity$location[i])
  	setTxtProgressBar(pb.lms, i)
  	
  	# Skip any analysis if there's less than 10 years of data or our trend doesn't go to the last 5 years of our record
  	if(length(rowsCity)<10 | max(valsCity$year[rowsCity])<=2015) next
  	
  	trend.LST <- lm(LST_Day ~ year, data=valsCity[rowsCity,])
  	sum.LST <- summary(trend.LST)
  	summaryCity[i,c("LST.trend", "LST.p")] <- sum.LST$coefficients["year",c(1,4)]
  	summaryCity[i,"LST.R2"] <- sum.LST $r.squared
  	
  	trend.tree <- lm(cover.tree ~ year, data=valsCity[rowsCity,])
  	sum.tree <- summary(trend.tree)
  	summaryCity[i,c("tree.trend", "tree.p")] <- sum.tree$coefficients["year",c(1,4)]
  	summaryCity[i,"tree.R2"] <- sum.tree$r.squared
  
  	trend.veg <- lm(cover.veg ~ year, data=valsCity[rowsCity,])
  	sum.veg <- summary(trend.veg)
  	summaryCity[i,c("veg.trend", "veg.p")] <- sum.veg$coefficients["year",c(1,4)]
  	summaryCity[i,"veg.R2"] <- sum.veg$r.squared
  	
  }
  summary(summaryCity)
  write.csv(summaryCity, file.path(path.cities, CITY, paste0(CITY, "_CityStats_Pixels.csv")), row.names=F)
  
  rm(trend.LST, trend.tree, trend.veg, sum.LST, sum.tree, sum.veg)
  
  # Making some plots with the summary data
  # proj.elev <- projection(elevCity)
  # sp.city2 <- terra::project(x=sp.city, proj.elev)
  # test <- sf::st_as_sf(sp.city)
  # sp.city3 <- st_transform(test, proj.elev)
  plot.lst <- ggplot(data=summaryCity[!is.na(summaryCity$LST.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=LST.mean)) +
    # geom_sf(data=sp.city3, fill=NA) +
    scale_fill_gradientn(name="Summer\nTemp\n(deg. C)", colors=grad.temp) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.elev <- ggplot(data=summaryCity[!is.na(summaryCity$elevation),]) +
    coord_equal() +
    # geom_tile(aes(x=x2, y=y2, fill=temp.summer)) +
    geom_tile(aes(x=x, y=y, fill=elevation)) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Elevation\n(m)", colors=grad.elev) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.tree <- ggplot(data=summaryCity[!is.na(summaryCity$tree.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=tree.mean)) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Tree\nCover\n(%)", colors=grad.tree, limits=c(0,100)) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.veg <- ggplot(data=summaryCity[!is.na(summaryCity$veg.mean),]) +
    coord_equal() +
    geom_tile(aes(x=x, y=y, fill=veg.mean)) +
    # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Other Veg\nCover (%)", colors=grad.other, limits=c(0,100)) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  
  png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Maps_Means.png")), height=8, width=8, units="in", res=120)
  print(
    cowplot::plot_grid(plot.lst, plot.elev, plot.tree, plot.veg)
  )
  dev.off()  
  
  if(length(which(!is.na(summaryCity$LST.trend)))>50){
    # cityAll.stats[row.city,]
    # Calculate the stats for the trends in LST and veg cover
    cityAll.stats$trend.LST.slope[row.city] <- mean(summaryCity$LST.trend, na.rm=T)
    cityAll.stats$trend.LST.slope.sd[row.city] <- sd(summaryCity$LST.trend, na.rm=T)
    LST.out <- t.test(summaryCity$LST.trend)
    cityAll.stats$trend.LST.p[row.city] <- LST.out$p.value
    
    cityAll.stats$trend.tree.slope[row.city] <- mean(summaryCity$tree.trend, na.rm=T)
    cityAll.stats$trend.tree.slope.sd[row.city] <- sd(summaryCity$tree.trend, na.rm=T)
    tree.out <- t.test(summaryCity$tree.trend)
    cityAll.stats$trend.tree.p[row.city] <- tree.out$p.value
    
    cityAll.stats$trend.veg.slope[row.city] <- mean(summaryCity$veg.trend, na.rm=T)
    cityAll.stats$trend.veg.slope.sd[row.city] <- sd(summaryCity$veg.trend, na.rm=T)
    veg.out <- t.test(summaryCity$veg.trend)
    cityAll.stats$trend.veg.p[row.city] <- veg.out$p.value
    
    # Creating and saving some maps of those trends
    plot.lst.trend <- ggplot(data=summaryCity[!is.na(summaryCity$LST.trend),]) +
      coord_equal() +
      geom_tile(aes(x=x, y=y, fill=LST.trend)) +
      # geom_sf(data=sp.city3, fill=NA) +
      scale_fill_gradientn(name="Summer\nTemp\n(deg. C/yr)", colors=grad.temp) +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_blank(),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
    
    plot.tree.trend <- ggplot(data=summaryCity[!is.na(summaryCity$tree.trend),]) +
      coord_equal() +
      geom_tile(aes(x=x, y=y, fill=tree.trend)) +
      # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
      scale_fill_gradientn(name="Tree\nCover\n(%/yr)", colors=grad.tree) +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_blank(),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
    
    plot.veg.trend <- ggplot(data=summaryCity[!is.na(summaryCity$veg.trend),]) +
      coord_equal() +
      geom_tile(aes(x=x, y=y, fill=veg.trend)) +
      # geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
      scale_fill_gradientn(name="Other Veg\nCover (%/yr)", colors=grad.other) +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_blank(),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
    
    
    png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Maps_Trends.png")), height=8, width=8, units="in", res=120)
    print(
      cowplot::plot_grid(plot.lst.trend, NULL, plot.tree.trend, plot.veg.trend)
    )
    dev.off()  
    
    
    # cityAll.stats[row.city,]
    # Now calculating the correlations among variables
    tree.lst <- lm(LST.trend ~ tree.trend, data=summaryCity)
    sum.corrTreeLST <- summary(tree.lst)
    cityAll.stats$corr.LST.tree.slope[row.city] <- sum.corrTreeLST$coefficients["tree.trend",1]
    cityAll.stats$corr.LST.tree.p[row.city] <- sum.corrTreeLST$coefficients["tree.trend",4]
    cityAll.stats$corr.LST.tree.Rsq[row.city]  <- sum.corrTreeLST$r.squared
    
    veg.lst <- lm(LST.trend ~ veg.trend, data=summaryCity)
    sum.corrVegLST <- summary(veg.lst)
    cityAll.stats$corr.LST.veg.slope[row.city] <- sum.corrVegLST$coefficients["veg.trend",1]
    cityAll.stats$corr.LST.veg.p[row.city] <- sum.corrVegLST$coefficients["veg.trend",4]
    cityAll.stats$corr.LST.veg.Rsq[row.city]  <- sum.corrVegLST$r.squared
  
    veg.tree <- lm(tree.trend ~ veg.trend, data=summaryCity)
    sum.corrVegTree <- summary(veg.tree)
    cityAll.stats$corr.tree.veg.slope[row.city] <- sum.corrVegTree$coefficients["veg.trend",1]
    cityAll.stats$corr.tree.veg.p[row.city] <- sum.corrVegTree$coefficients["veg.trend",4]
    cityAll.stats$corr.tree.veg.Rsq[row.city]  <- sum.corrVegTree$r.squared
    
    plot.corr.LST.Tree <- ggplot(data=summaryCity, aes(x=tree.trend, y=LST.trend)) +
      geom_point() +
      stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
      labs(x="Tree Trend (%/yr)", y="LST Trend (deg. C/yr)") +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_text(face="bold"),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
    
    plot.corr.LST.Veg <- ggplot(data=summaryCity, aes(x=veg.trend, y=LST.trend)) +
      geom_point() +
      stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
      labs(x="Other Veg Trend (%/yr)", y="LST Trend (deg. C/yr)") +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_text(face="bold"),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
    plot.corr.Tree.Veg <- ggplot(data=summaryCity, aes(x=veg.trend, y=tree.trend)) +
      geom_point() +
      stat_smooth(method=lm, color="red", fill="red", alpha=0.2) +
      labs(x="Other Veg Trend (%/yr)", y="Tree Trend (%/yr)") +
      theme(panel.background=element_rect(fill=NA, color="black"),
            panel.grid=element_blank(),
            axis.ticks.length = unit(-0.5, "lines"),
            axis.title=element_text(face="bold"),
            axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
            axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
    png(file.path(path.cities, CITY, paste0(CITY, "_CityStats_Correlations_Trends.png")), height=8, width=8, units="in", res=120)
    print(
      cowplot::plot_grid(plot.corr.LST.Tree, plot.corr.LST.Veg, NULL, plot.corr.Tree.Veg)
    )
    dev.off()  
  }
  write.csv(cityAll.stats, file.cityAll.stats, row.names=F)  # Write our city stats file each time in case it bonks

  # Remove a bunch of stuff for our own sanity
  rm(elevCity, treeCity, vegCity, lstCity, modCity, valsCity, summaryCity, coordsCity, biome, sp.city, plot.corr.LST.Tree, plot.corr.LST.Veg, plot.corr.Tree.Veg, plot.lst.trend, plot.tree.trend, plot.veg.trend, plot.elev, plot.lst, plot.tree, plot.veg, veg.lst, veg.tree, tree.lst, veg.out, tree.out, sum.corrTreeLST, sum.corrVegLST, sum.corrVegTree, sum.modCity)
  
}	
