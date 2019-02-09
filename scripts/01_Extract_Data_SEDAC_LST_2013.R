# Exploring new dataset options thanks to NASA's Socioeconomic Data and Applicaitons Center (Hosted by CIESIN at Columbia)
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-summer-lst-2013
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013

# Summer Land Surface Temperature Grids for 2013:
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-summer-lst-2013
# -- Global Grid at ~1km resolution produced from MODIS
# -- July-August mean for N hemisphere, Jan=Feb for Southern

# Water Bodies from here: http://openstreetmapdata.com/data/water-reduced-polygons

# Extract info on tree cover and mean summer temp for major urban areas around the world
# Datasets to use:
# 1. Urban Areas: LandScan https://landscan.ornl.gov
#     - Filter to areas >= 1,000,000 people (698 regions)
# 2. Tree Cover Data: UMD Global Forest Change: http://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html
#     - Spatial Resolution: 30 m
#     - Temporal Extent: 2010
# 3. Climate Data:
#     - Plan A: Landsat; https://landsat.usgs.gov/landsat-surface-temperature
#         ***** can't access during gov't shutdown
#         - Spatial Resolution: 30 m 
#         - Temporal Extent: ???
#     - Plan B: WorldClim v2.0 http://worldclim.org/version2
#         - Spatial Resolution: 30 arcseconds (approx 1 km)
#         - Temporal Extent: mean 1970-2000


library(sp); library(rgdal); library(raster); library(rgeos); library(maps)

# Using the SDEI urban database since it has additional info
sdei.urb <- readOGR("/Volumes/Morton_SDM/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
summary(sdei.urb)
dim(sdei.urb)
summary(sdei.urb$SQKM_FINAL)

# data.frame(sdei.urb[sdei.urb$NAME=="Chicago",])

# These are now population estimates for the metropolitan areas (designated "Urban") -- we can scale back a bit
cities.use <- sdei.urb[sdei.urb$ES00POP>1e6 &  !is.na(sdei.urb$NAME) & sdei.urb$SQKM_FINAL>1e2 & sdei.urb$D_T_DIFF>0,]
dim(cities.use)
summary(cities.use)
summary(droplevels(cities.use$ISO3))
hist(cities.use$URB_N_MEAN)
data.frame(cities.use[cities.use$URB_N_MEAN<10,])

# Plot the map to get a better feel for geographic distribution
png("../data_processed/cities_used_spdei_v3.png", height=4, width=8, units="in", res=120)
map(col="red", lwd=0.5)
plot(cities.use, add=T)
dev.off()

# Cleaning up some names
cities.use$NAME <- as.character(cities.use$NAME)
cities.use[cities.use$NAME=="Xi'an", "NAME"] <- "Xian" # because syntax won't work with recode
cities.use$NAME <- car::recode(cities.use$NAME, "'?stanbul'='Istanbul'; 'S?o Paulo'='Sao Paulo'; '?zmir'='Izmir'")
cities.use$NAME <- gsub("?", "XX", cities.use$NAME)
cities.use$NAME <- gsub(" ", "", cities.use$NAME)
# cities.use$NAME <- gsub("  ", "", cities.use$NAME)

cities.use <- cities.use[order(cities.use$NAME), ]

# plot(cities.use[cities.use$NAME=="Chicago",])
# plot(cities.use)

# Going to need to mask out water, so lets load a layer
# water <- readOGR("/Volumes/Morton_SDM/water-polygons-split-4326/water_polygons.shp")
# summary(water)
# water <- readOGR("~/Desktop/occurrence.tif.lyr")
oceans <- readOGR("~/Desktop/ocean-polygons-reduced-3857/ocean_reduced_z5.shp")
lakes  <- readOGR("~/Desktop/lakes-polygons-reduced-3857/lakes_reduced_z5.shp")
rivers <- readOGR("~/Desktop/river-polygons-reduced-3857/river_reduced_z5.shp")

oceans <- gBuffer(oceans, byid=T, width=0)
lakes  <- gBuffer(lakes, byid=T, width=0)
rivers <- gBuffer(rivers, byid=T, width=0)

oceans <- spTransform(oceans, projection(cities.use))
lakes <- spTransform(lakes, projection(cities.use))
rivers <- spTransform(rivers, projection(cities.use))


# Doing some indexing of tree cover data
path.trees <- "/Volumes/Morton_SDM/TreeCover/"
files.trees <- dir(path.trees, ".tif")
ftree.df <- data.frame(file=files.trees)
ftree.df$lat.char <- as.factor(unlist(lapply(stringr::str_split(ftree.df$file, "_"), function(x){x[4]})))
ftree.df$lon.char <- as.factor(unlist(lapply(stringr::str_split(ftree.df$file, "_"), function(x){substr(x[5], 1, 4)})))
ftree.df$lat <- ifelse(substr(ftree.df$lat.char,3,3)=="S", 
                       -as.numeric(substr(ftree.df$lat.char,1,2)),
                       as.numeric(substr(ftree.df$lat.char,1,2)))
ftree.df$lon <- ifelse(substr(ftree.df$lon.char,4,4)=="W", 
                       -as.numeric(substr(ftree.df$lon.char,1,3)),
                       as.numeric(substr(ftree.df$lon.char,1,3)))
# ftree.df$lat <- as.numeric(lapply())
summary(ftree.df)

# Testing the temperature data -- July 
tmax <- raster("/Volumes/Morton_SDM/sdei-global-summer-lst-2013-global/sdei-global-summer-lst-2013-day-max-global.tif")
elev <- raster("/Volumes/Morton_SDM/Elevation_SRTM30/topo30/topo30.grd")

# -----------------------------------------
# Looping through Cities
# -----------------------------------------
# cities.use$NAME
path.save <- "../data_processed/cities_full_sdei_v3"
dir.create(path.save, recursive=T, showWarnings = F)
pb <- txtProgressBar(min=0, max=nrow(cities.use), style=3)
for(i in 1:nrow(cities.use)){
  # i=which(cities.use$NAME=="Chicago")
  
  setTxtProgressBar(pb, i)
  # Subset our shapefile
  city.sp <- cities.use[i,]
  # data.frame(city.sp)
  city.name <- city.sp$NAME
  city.name <- sub(c("[?]"), "X", city.name) # Getting rid of the ? infront of some cities
  city.name <- sub(c("[?]"), "X", city.name) # Being lazy and doing this twice just in case to get rid of the rare ??
  bb.city <- bbox(city.sp)
  
  ocean.city <- lakes.city <- river.city <- NULL
  ocean.city <- crop(oceans, extent(city.sp))
  lakes.city <- crop(lakes, extent(city.sp))
  river.city <- crop(rivers, extent(city.sp))
  
  # plot(city.sp);
  # if(length(ocean.city)>0) plot(ocean.city, add=T, col="blue3");
  # if(length(lakes.city)>0) plot(lakes.city, add=T, col="cadetblue2");
  # if(length(river.city)>0) plot(river.city, add=T, col="dodgerblue2")
  
  # ---------------
  # Extract the climate data
  # ---------------
  # -- transform the projection of the city --> not actulaly necessary

  # Cut down the dataset to a city-scale
  temp.city <- crop(tmax, city.sp)
  
  # Mask out water bodies
  if(length(ocean.city)>0) temp.city <- mask(temp.city, ocean.city, inverse=T)
  if(length(lakes.city)>0) temp.city <- mask(temp.city, lakes.city, inverse=T)
  if(length(river.city)>0) temp.city <- mask(temp.city, river.city, inverse=T)
  
  temp.city2 <- mask(temp.city, city.sp)
  temp.city2[temp.city2<=0] <- NA
  
  # Filter out things beyond 6 sigma
  vals.tmax <- getValues(temp.city2)
  tmax.mean <- mean(vals.tmax, na.rm=T)
  tmax.sd   <- sd(vals.tmax, na.rm=T)
  temp.city2[temp.city2>tmax.mean + 6*tmax.sd] <- NA
  temp.city2[temp.city2<tmax.mean - 6*tmax.sd] <- NA
  vals.tmax <- getValues(temp.city2)
  
  # plot(temp.city); plot(city.sp, add=T)
  # plot(temp.city2); plot(city.sp, add=T)
  # ---------------

  # ---------------
  # Elevation data
  # ---------------
  # Cut down the dataset to a city-scale
  # city.sp2 <- spTransform(city.sp, CRS(projection(elev)))
  # city.sp2 <- city.sp
  # extent(city.sp2)[1:2] <- extent(city.sp2)[1:2]+180
  ext.city <- extent(city.sp)
  if(all(ext.city[1:2]<0)) {
    extent.new <- ext.city+c(360, 360,0,0)
    elev.city <- crop(elev, extent.new)
    extent(elev.city) <- ext.city
  } else if(all(ext.city[1:2]>0)) {
    elev.city <- crop(elev, ext.city)
    extent(elev.city) <- ext.city
  } else { # What to do if a city strattles the prime meridian
    
    ext1 <- c(ext.city[1]+360,360,ext.city[3:4])
    ext2 <- c(0,ext.city[2],ext.city[3:4])
    
    city1 <- crop(elev, ext1)
    extent(city1) <- extent(ext.city[1],0,ext.city[3:4])
    
    city2 <- crop(elev, ext2)
    extent(city2) <- extent(0,ext.city[2],ext.city[3:4])
    
    elev.city <- mosaic(city1, city2, fun=mean)
  }
  
  
  elev.city <- resample(elev.city, temp.city)
  
  # Mask out water bodies
  if(length(ocean.city)>0) elev.city <- mask(elev.city, ocean.city, inverse=T)
  if(length(lakes.city)>0) elev.city <- mask(elev.city, lakes.city, inverse=T)
  if(length(river.city)>0) elev.city <- mask(elev.city, river.city, inverse=T)
  
  elev.city2 <- mask(elev.city, city.sp)
  # elev.city2[elev.city2<=0] <- NA
  
  # Filter out things beyond 6 sigma
  vals.elev <- getValues(elev.city2)
  elev.mean <- mean(vals.elev, na.rm=T)
  elev.sd   <- sd(vals.elev, na.rm=T)
  elev.city2[elev.city2>elev.mean + 6*elev.sd] <- NA
  elev.city2[elev.city2<elev.mean - 6*elev.sd] <- NA
  vals.elev <- getValues(elev.city2)
  
  # plot(elev.city); plot(city.sp, add=T)
  # plot(elev.city2); plot(city.sp, add=T)
  # ---------------
  
  # ---------------
  # Find our tree canopy file
  # ---------------
  f.city <- which(ftree.df$lat-10<=bb.city[2,1] & ftree.df$lat>=bb.city[2,2] &
                    ftree.df$lon<=bb.city[1,1] & ftree.df$lon+10>=bb.city[1,2])
  # ftree.df$file[f.city]
  if(length(f.city)==0){
    # Figure out which dimension fails
    test.lat <- which(ftree.df$lat-10<=bb.city[2,1] & ftree.df$lat>=bb.city[2,2])
    test.lon <- which(ftree.df$lon<=bb.city[1,1] & ftree.df$lon+10>=bb.city[1,2])
    
    # Note: this may bonk, but we'll try it for now
    if(length(test.lon)==0){
      test.lon <- which(ftree.df$lon<=bb.city[1,1]+10 & ftree.df$lon+10>=bb.city[1,2]-10)
      f.city <- test.lat[test.lat %in% test.lon]
    }
    if(length(test.lat)==0){
      test.lat <- which(ftree.df$lat-10<=bb.city[2,1]+10 & ftree.df$lat+10>=bb.city[2,2]-10)
      f.city <- test.lat[test.lat %in% test.lon]
    }
    
  }
  
  if(length(f.city)==0) next
  if(length(f.city)==1){
    tree.city <- raster(file.path(path.trees, ftree.df$file[f.city]))
    tree.city
  } else if(length(f.city)>=2) {
    tree.city <- raster(file.path(path.trees, ftree.df$file[f.city[1]]))
    
    ext.city <- extent(city.sp)
    
    # Setting up the temporary extent
    ext1 <- extent(tree.city)
    ext.temp <- c(max(ext1[1], ext.city[1]),
                  min(ext1[2], ext.city[2]),
                  max(ext1[3], ext.city[3]),
                  min(ext1[4], ext.city[4]))
    
    # If we end up with bounding issues, just take the full range of the raster
    # -- this will be slower, but more reliable for now
    if(ext.temp[1]>=ext.temp[2]) ext.temp[1:2] <- ext1[1:2]
    if(ext.temp[3]>=ext.temp[4]) ext.temp[3:4] <- ext1[3:4]
    
    tree.city <- crop(tree.city, extent(ext.temp))
    
    for(j in 2:length(f.city)){
      tree2 <- raster(file.path(path.trees, ftree.df$file[f.city[j]]))
      ext2 <- extent(tree2)
      
      # Setting up the temporary extent
      ext.temp <- c(max(ext2[1], ext.city[1]),
                    min(ext2[2], ext.city[2]),
                    max(ext2[3], ext.city[3]),
                    min(ext2[4], ext.city[4]))
      
      if(ext.temp[1]>=ext.temp[2]) ext.temp[1:2] <- ext2[1:2]
      if(ext.temp[3]>=ext.temp[4]) ext.temp[3:4] <- ext2[3:4]
      
      tree2 <- crop(tree2, extent(ext.temp))
      
      tree.city <- mosaic(tree.city, tree2, fun=mean)
    }
  } 
  
  tree.city <- crop(tree.city, city.sp)
  tree.city[tree.city<0] <- NA
  tree.city[tree.city>100] <- NA
  # plot(tree.city)
  
  tree.city <- resample(tree.city, temp.city)
  tree.city[tree.city<0] <- NA
  tree.city[tree.city>100] <- NA
  # plot(tree.city); plot(city.sp, add=T)
  
  # Mask out water bodies
  if(length(ocean.city)>0) tree.city <- mask(tree.city, ocean.city, inverse=T)
  if(length(lakes.city)>0) tree.city <- mask(tree.city, lakes.city, inverse=T)
  if(length(river.city)>0) tree.city <- mask(tree.city, river.city, inverse=T)
  
  
  
  tree.city2 <- mask(tree.city, city.sp)
  tree.city2[tree.city2<0] <- NA
  tree.city2[tree.city2>100] <- NA
  
# plot(tree.city2); plot(city.sp, add=T)
  
  vals.tree <- getValues(tree.city2)
  
  png(file.path(path.save, paste0(city.name, "_maps.png")), height=8, width=6, unit="in", res=180)
  par(mfrow=c(2,2))
  plot(temp.city2, main="Summer Day Max Temp\n2013, deg.C"); plot(city.sp, add=T)
  plot(tree.city2, main="Tree Cover\n 2000, perc. cover"); plot(city.sp, add=T)
  plot(elev.city2, main="Elevation; unknown units"); plot(city.sp, add=T)
  par(mfrow=c(1,1))
  dev.off()
  
  # test <- coordinates(tree.city2)
  
  df.city <- data.frame(Name=city.name, coordinates(temp.city2), cover.tree=vals.tree, temp.summer=vals.tmax, elevation=vals.elev)
  df.city <- df.city[complete.cases(df.city),]
  write.csv(df.city, file.path(path.save, paste0(city.name, "_data_full.csv")), row.names=F)
  # summary(df.city)
  lm.city <- lm(temp.summer ~ cover.tree, data=df.city)
  sum.lm <- summary(lm.city)
  
  # gam.test <- mgcv::gam(temp.summer ~ cover.tree + elevation + s(x,y), data=df.city)
  # plot(gam.test)
  # summary(gam.test)
  # 
  png(file.path(path.save, paste0(city.name, "_scatter.png")), height=4, width=4, unit="in", res=90)
  plot(temp.summer ~ cover.tree, data=df.city, cex=0.5,
       main=paste0("R2=",round(sum.lm$r.squared,2))); 
  abline(lm.city, col="red", lwd=2); 
  dev.off()
  
  
  cities.use[i,"tree.mean"] <- mean(df.city$cover.tree, na.rm=T)
  cities.use[i,"tree.sd"  ] <- sd(df.city$cover.tree, na.rm=T)
  cities.use[i,"tree.max" ] <- max(df.city$cover.tree, 0.90, na.rm=T)
  cities.use[i,"tree.min" ] <- min(df.city$cover.tree, 0.90, na.rm=T)
  cities.use[i,"elev.mean"] <- mean(df.city$elevation, na.rm=T)
  cities.use[i,"elev.sd"  ] <- sd(df.city$elevation, na.rm=T)
  cities.use[i,"elev.max" ] <- max(df.city$elevation, na.rm=T)
  cities.use[i,"elev.min" ] <- min(df.city$elevation, na.rm=T)
  cities.use[i,"temp.mean"] <- mean(df.city$temp.summer, na.rm=T)
  cities.use[i,"temp.sd"  ] <- sd(df.city$temp.summer, na.rm=T)
  cities.use[i,"temp.max" ] <- max(df.city$temp.summer, na.rm=T)
  cities.use[i,"temp.min" ] <- min(df.city$temp.summer, na.rm=T)
  # cities.use[i,"correlation"] <- sum.lm$r.squared
  # cities.use[i,"slope"] <- sum.lm$coefficients[2,1] 
  
  rm(ocean.city, lakes.city, river.city)
}
# cities.use <- cities.use[,!names(cities.use) %in% c("july.mean", "july.sd", "july.max", "july.min")]
summary(cities.use)
write.csv(data.frame(cities.use), "../data_processed/cities_summary_sdei_v3.csv", row.names=F)
# ---------------
# -----------------------------------------


# -----------------------------------------
# Looking at some quick summary stats
# -----------------------------------------
cities.use <- data.frame(cities.use)
summary(cities.use)

# -----------------------------------------
