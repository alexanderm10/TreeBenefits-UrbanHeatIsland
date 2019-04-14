# Exploring new dataset options thanks to NASA's Socioeconomic Data and Applicaitons Center (Hosted by CIESIN at Columbia)
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-summer-lst-2013
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013

# Summer Land Surface Temperature Grids for 2013: MODIS summer temperature
# Data downloaded from here: https://ladsweb.modaps.eosdis.nasa.gov/search/
#  https://ladsweb.modaps.eosdis.nasa.gov/api/v1/productPage/product=MOD11A2
# According to documentation here: the scale factor for land surface day temperature is 0.02: https://icess.eri.ucsb.edu/modis/LstUsrGuide/usrguide_1dtil.html#Table_9
# Rather relying SDEI product did not do higher-level processing and was sensitive to erronious (and impossible) values; Rather than rely on the SDEI product, I'm going the raw data and looking at July temperature for the Northern Hemisphere and January temperature for the Southern.  Still doing 2013 because that's what I'd already pulled for the new MODIS tree cover dataset.  Will process all tiles for both seasons becuase cities often cross the equator.

# Water Bodies from here: http://openstreetmapdata.com/data/water-reduced-polygons

# Extract info on tree cover and mean summer temp for major urban areas around the world
# Datasets to use:
# 1. Urban Areas: SDEI dataset
# 2. Tree Cover Data: MODIS MOD44B
# 3. Climate Data: MODIS MOD11A


library(sp); library(rgdal); library(raster); library(rgeos); library(maps)

# Using the SDEI urban database since it has additional info
sdei.urb <- readOGR("/Volumes/Morton_SDM/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
summary(sdei.urb)
dim(sdei.urb)
summary(sdei.urb$SQKM_FINAL)

# data.frame(sdei.urb[sdei.urb$NAME=="Chicago",])

# These are now population estimates for the metropolitan areas (designated "Urban") -- we can scale back a bit
cities.use <- sdei.urb[sdei.urb$ES00POP>1e6 &  !is.na(sdei.urb$NAME) & sdei.urb$SQKM_FINAL>1e2,]
dim(cities.use)
summary(cities.use)
summary(droplevels(cities.use$ISO3))
hist(cities.use$URB_N_MEAN)
data.frame(cities.use[cities.use$URB_N_MEAN<10,])

# Plot the map to get a better feel for geographic distribution
png("../data_processed/cities_used_spdei_v4.png", height=4, width=8, units="in", res=120)
map(col="red", lwd=0.5)
plot(cities.use, add=T)
dev.off()

# Cleaning up some names
cities.use$NAME <- as.character(cities.use$NAME)
cities.use[cities.use$NAME=="Xi'an", "NAME"] <- "Xian" # because syntax won't work with recode
cities.use$NAME[grepl("[?]", cities.use$NAME)]

cities.use$NAME <- car::recode(cities.use$NAME, "'?stanbul'='Istanbul'; 'S?o Paulo'='Sao Paulo'; '?zmir'='Izmir'; 'S?o Jos? dos Campos'='Sao Jose dos Campos'; 'Ni?nij Novgorod'='Nizhny Novgorod'")
cities.use$NAME <- gsub("[?]", "XX", cities.use$NAME)
cities.use$NAME <- gsub(" ", "", cities.use$NAME)
cities.use$NAME <- gsub("  ", "", cities.use$NAME)

cities.use <- cities.use[order(cities.use$NAME), ]

# plot(cities.use[cities.use$NAME=="Chicago",])
# plot(cities.use)

# Getting some baseline ecoregion info: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
# ecoregions <- readOGR("../data_raw/wwf_biomes_official/wwf_terr_ecos.shp")
ecoregions <- readOGR("../data_raw/global200ecoregions/g200_terr.shp")
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

# Going to need to mask out water, so lets load a layer
# water <- readOGR("/Volumes/Morton_SDM/water-polygons-split-4326/water_polygons.shp")
# summary(water)
# water <- readOGR("~/Desktop/occurrence.tif.lyr")
oceans <- readOGR("/Volumes/Morton_SDM/waterbodies_openstreet/ocean-polygons-reduced-3857/ocean_reduced_z5.shp")
lakes  <- readOGR("/Volumes/Morton_SDM/waterbodies_openstreet/lakes-polygons-reduced-3857/lakes_reduced_z5.shp")
rivers <- readOGR("/Volumes/Morton_SDM/waterbodies_openstreet/river-polygons-reduced-3857/river_reduced_z5.shp")

oceans <- gBuffer(oceans, byid=T, width=0)
lakes  <- gBuffer(lakes, byid=T, width=0)
rivers <- gBuffer(rivers, byid=T, width=0)

oceans <- spTransform(oceans, projection(cities.use))
lakes <- spTransform(lakes, projection(cities.use))
rivers <- spTransform(rivers, projection(cities.use))


# Doing some indexing of tree cover data
path.trees <- "/Volumes/Morton_SDM/TreeCover_MOD44Bv6/2013/HEGOUT/"
ftree <- dir(path.trees, ".tif")
ftree <- ftree[which(substr(ftree, nchar(ftree)-3, nchar(ftree))==".tif")] # ignore anything that's not a .tif
ftree.split <- stringr::str_split(ftree, "[.]")
ftree.df <- data.frame(file=ftree, matrix(unlist(ftree.split), ncol=length(ftree.split[[1]]), byrow = T))
names(ftree.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
ftree.df$year <- as.numeric(substr(ftree.df$date.stamp, 2, 5))
ftree.df <- ftree.df[ftree.df$year==2013,]
summary(ftree.df)

ftree.df[,c("xmin", "xmax", "ymin", "ymax")] <- NA
for(i in 1:nrow(ftree.df)){
  tmp <- raster(file.path(path.trees, ftree.df$file[i]))
  
  ftree.df[i,c("xmin", "xmax", "ymin", "ymax")] <- extent(tmp)
}
summary(ftree.df)


# Setting up file paths and directories for the temperature data
path.01 <- "/Volumes/Morton_SDM/SurfTemp_MODIS_MODA2/2013/01-02/HEGOUT/"
path.07 <- "/Volumes/Morton_SDM/SurfTemp_MODIS_MODA2/2013/07-08/HEGOUT/"

fjan <- dir(path.01, ".tif")
fjan <- fjan[which(substr(fjan, nchar(fjan)-3, nchar(fjan))==".tif")] # ignore anything that's not a .tif
fjan.split <- stringr::str_split(fjan, "[.]")
fjan.df <- data.frame(file=fjan, matrix(unlist(fjan.split), ncol=length(fjan.split[[1]]), byrow = T))
names(fjan.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
summary(fjan.df)

fjan.df[,c("xmin", "xmax", "ymin", "ymax")] <- NA
for(i in 1:nrow(fjan.df)){
  tmp <- raster(file.path(path.01, fjan.df$file[i]))
  
  fjan.df[i,c("xmin", "xmax", "ymin", "ymax")] <- extent(tmp)
}
summary(fjan.df)

fjul <- dir(path.07, ".tif")
fjul <- fjul[which(substr(fjul, nchar(fjul)-3, nchar(fjul))==".tif")] # ignore anything that's not a .tif
fjul.split <- stringr::str_split(fjul, "[.]")
fjul.df <- data.frame(file=fjul, matrix(unlist(fjul.split), ncol=length(fjul.split[[1]]), byrow = T))
names(fjul.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
summary(fjul.df)

fjul.df[,c("xmin", "xmax", "ymin", "ymax")] <- NA
for(i in 1:nrow(fjul.df)){
  tmp <- raster(file.path(path.07, fjul.df$file[i]))
  
  fjul.df[i,c("xmin", "xmax", "ymin", "ymax")] <- extent(tmp)
}
summary(fjul.df)


# Elevation as an additional predictor to account for potential bias in tree cover distribution
elev <- raster("/Volumes/Morton_SDM/Elevation_SRTM30/topo30/topo30.grd")


# Setting up a function to iteratively remove outliers since we do it a LOT
# Iteratively removing 6-sigma outliers
#  -- not great, but necessary to do at the large scale before clipping to cities with lower SD
#  -- in my test, this kept ~95% of data with the biggest outliers being on the coast;
# if this becomes too problematic, can do deviation from tile mean and do the multi-time analysis
#      with na.rm=T, but that might need to happen at a city-scale
#  but we'll need to
filter.outliers <- function(RASTER, n.sigma=6){
  vals.tmp <- getValues(RASTER); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
  while(length(which(vals.tmp < tmp.x-n.sigma*tmp.sd | vals.tmp > tmp.x+n.sigma*tmp.sd))>0){
    RASTER[RASTER < tmp.x-6*tmp.sd]  <- NA
    RASTER[RASTER > tmp.x+6*tmp.sd]  <- NA
    
    vals.tmp <- getValues(RASTER); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
  } # end while loop
  
  return(RASTER)
} # End function

# -----------------------------------------
# Looping through Cities
# -----------------------------------------
# cities.use$NAME
path.save <- "../data_processed/cities_full_sdei_v4"
dir.create(path.save, recursive=T, showWarnings = F)
pb <- txtProgressBar(min=0, max=nrow(cities.use), style=3)
# for(i in 1:nrow(cities.use)){
for(i in 1:nrow(cities.use)){
  # i=which(cities.use$NAME=="Chicago")
  # i=which(cities.use$NAME=="Manaus")
  # i=which(cities.use$NAME=="Atlanta")
  # i=which(cities.use$NAME=="Amsterdam")
  # i=which(cities.use$NAME=="Chongqing")
  # i=which(cities.use$NAME=="Barranquilla")
  
  
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
  # 1. Find whether to use January or July 
  # 2. Figure out which rasters to process
  # 3. For *each date* 
  #    3.1. moasaic, then filter, then mask
  #    3.2. calculate spatial deviation from city mean
  # 4. Find & store mean deviation (& mean temp?)
  # ---------------
  # 1. Find whether to use January or July 
  if(mean(extent(city.sp)[3:4])>0) {
    # If mostly in northern hemishpere use July
    met.path <- path.07
    met.df <- fjul.df
  } else {
    # Else use January
    met.path <- path.01
    met.df <- fjan.df
  } # End N/S identification
  
  # 2. Figure out which rasters to process
  f.met <- which(((met.df$ymin<=bb.city[2,1] & met.df$ymax>=bb.city[2,1]) |  # raster min < min & raster min > min
                      (met.df$ymax>=bb.city[2,2] & met.df$ymin<=bb.city[2,2])) & # max is greater than both
                   ((met.df$xmin<=bb.city[1,1] & met.df$xmax>=bb.city[1,1]) |  # raster min < min & raster min > min
                      (met.df$xmax>=bb.city[1,2] & met.df$xmin<=bb.city[1,2]))
                   
                 )
  
  f.met <- f.met[order(met.df$ymax[f.met], met.df$xmax[f.met], decreasing=T)]
  
  # 3. For *each date* 
  #    3.1. moasaic, then filter, then mask
  #    3.2. calculate spatial deviation from city mean
  tmax <- stack()
  tdev <- stack()
  for(NOW in unique(met.df[f.met, "date.stamp"])){
    files.now <- met.df[f.met,]
    files.now <- files.now[files.now$date.stamp==NOW,"file"]
    
    if(length(files.now)==1){
      met.city <- raster(file.path(met.path, files.now))
      met.city <- met.city*0.02
      met.city[met.city<=250] <- NA # 250k = -23˚C = -9.7F; almost certainly not a sumemr temperature
      # plot(met.city); plot(city.sp, add=T)
    } else {
      met.city <- raster(file.path(met.path, files.now[1]))
      met.city <- met.city*0.02 # Scale factor from documentation; now in Kelvin
      met.city[met.city<=250] <- NA # 250k = -23˚C = -9.7F; almost certainly not a sumemr temperature
      # plot(met.city); plot(city.sp, add=T)
      
      for(j in 2:length(files.now)){
        met2 <- raster(file.path(met.path,files.now[j]))
        met2 <- met2*0.02 # Scale factor from documentation; now in Kelvin
        met2[met2<=250] <- NA # 250k = -23˚C = -9.7F; almost certainly not a sumemr temperature
        # plot(met2); plot(city.sp, add=T)
        
        # met2 <- resample(met2, met.city[[1]])
        ext1 <- extent(met.city)
        ext2 <- extent(met2)
        ext.temp <- c(min(ext1[1], ext2[1]),
                      max(ext1[2], ext2[2]),
                      min(ext1[3], ext2[3]),
                      max(ext1[4], ext2[4]))
        met.city <- extend(met.city, ext.temp)
        # plot(met.city); plot(city.sp, add=T)
        
        met2 <- resample(met2, met.city)
        met.city <- mosaic(met.city, met2, fun=mean, na.rm=T, tolerance=0.2)
        # plot(met.city); plot(city.sp, add=T)
      } # end j loop
    }# end ifelse multiple file mosaicing

    # Iteratively removing 6-sigma outliers
    met.city <- filter.outliers(RASTER=met.city, n.sigma = 6)
    # plot(met.city); plot(city.sp, add=T)

    # Crop & mask out our city area
    met.city <- crop(met.city, city.sp)
    met.city <- mask(met.city, city.sp)
    
    # Mask out water bodies
    if(length(ocean.city)>0) met.city <- mask(met.city, ocean.city, inverse=T)
    if(length(lakes.city)>0) met.city <- mask(met.city, lakes.city, inverse=T)
    if(length(river.city)>0) met.city <- mask(met.city, river.city, inverse=T)
    
    met.dev <- met.city - mean(getValues(met.city), na.rm=T)
    # plot(met.city); plot(city.sp, add=T)
    # plot(met.dev); plot(city.sp, add=T)
    
    tmax <- addLayer(tmax, met.city)
    tdev <- addLayer(tdev, met.dev)
  } # End Time loop
  # plot(tmax)
  # plot(tdev)
  
  # Filter outliers one more time before taking the mean; because we have multiple time bands, it's less likely to remove true data
  tmax <- filter.outliers(RASTER=tmax, n.sigma=6)
  tdev <- filter.outliers(RASTER=tdev, n.sigma=6)

  # 4. Find & store mean from all time points
  tmax <- mean(tmax, na.rm=T)
  tdev <- mean(tdev, na.rm=T)
  # plot(tmax); plot(city.sp, add=T)
  # plot(tdev); plot(city.sp, add=T)
  
  # Filter out things beyond 6 sigma
  # tmax <- filter.outliers(RASTER = tmax, n.sigma = 6)
  vals.tmax <- getValues(tmax)

  # tdev <- filter.outliers(RASTER = tdev, n.sigma = 6)
  # tdev2 <- filter.outliers(RASTER = tdev, n.sigma = 6)
  vals.tdev <- getValues(tdev)
  
  # If we can't get good temperature data, skip this; right now this is a pretty low bar, but we'll see
  if(length(which(!is.na(vals.tdev)))<50 | length(which(!is.na(vals.tmax)))<25) next
  # plot(tmax); plot(city.sp, add=T)
  # plot(tdev); plot(city.sp, add=T)
  # ---------------

  # ---------------
  # Elevation data
  # ---------------
  # Cut down the dataset to a city-scale
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
  
  
  elev.city <- filter.outliers(RASTER = elev.city, n.sigma = 6)
  elev.city <- resample(elev.city, tdev)
  
  elev.city <- mask(elev.city, city.sp)
  
  # Mask out water bodies
  if(length(ocean.city)>0) elev.city <- mask(elev.city, ocean.city, inverse=T)
  if(length(lakes.city)>0) elev.city <- mask(elev.city, lakes.city, inverse=T)
  if(length(river.city)>0) elev.city <- mask(elev.city, river.city, inverse=T)
  

  # Filter out things beyond 6 sigma
  # elev.city <- filter.outliers(RASTER = elev.city, n.sigma = 6)
  vals.elev <- getValues(elev.city)
  
  # plot(elev.city); plot(city.sp, add=T)
  # ---------------
  
  # ---------------
  # Tree Canopy
  # --------------
  f.city <- which(((ftree.df$ymin<=bb.city[2,1] & ftree.df$ymax>=bb.city[2,1]) |  # raster min < min & raster min > min
                    (ftree.df$ymax>=bb.city[2,2] & ftree.df$ymin<=bb.city[2,2])) & # max is greater than both
                   ((ftree.df$xmin<=bb.city[1,1] & ftree.df$xmax>=bb.city[1,1]) |  # raster min < min & raster min > min
                      (ftree.df$xmax>=bb.city[1,2] & ftree.df$xmin<=bb.city[1,2]))
                  )
  
  f.city <- f.city[order(ftree.df$ymax[f.city], ftree.df$xmax[f.city], decreasing=T)]

  if(length(f.city)==0) next
  if(length(f.city)==1){
    tree.city <- raster(file.path(path.trees, ftree.df$file[f.city]))
    tree.city[tree.city>100] <- NA
    tree.city[tree.city==0] <- NA
    
    # tree.city
  } else {
    tree.city <- raster(file.path(path.trees, ftree.df$file[f.city[1]]))
    tree.city[tree.city>100] <- NA
    tree.city[tree.city<=0] <- NA
    # plot(tree.city); plot(city.sp, add=T)
    
    for(j in 2:length(f.city)){
      tree2 <- raster(file.path(path.trees, ftree.df$file[f.city[j]]))
      tree2[tree2>100] <- NA
      tree2[tree2<=0] <- NA
      # plot(tree2, add=T); plot(city.sp, add=T)
      
      ext1 <- extent(tree.city)
      ext2 <- extent(tree2)
      ext.temp <- c(min(ext1[1], ext2[1]),
                    max(ext1[2], ext2[2]),
                    min(ext1[3], ext2[3]),
                    max(ext1[4], ext2[4]))
      tree.city <- extend(tree.city, ext.temp)
      
      tree2 <- resample(tree2, tree.city)
      # plot(tree2); plot(city.sp, add=T)
      
      tree.city <- mosaic(tree.city, tree2, fun=mean, na.rm=T, tolerance=0.2)
      # plot(tree.city); plot(city.sp, add=T)
    }
  } 
  
  # Iteratively removing 6-sigma outliers from large scene; not local area
  tree.city <- filter.outliers(RASTER = tree.city, n.sigma = 6)

  # tree.city <- crop(tree.city, extent(city.sp)+c(-1,1-1,1)) # crop to reduce our area, but leave a buffer for the resamp
  tree.city[is.na(tree.city)] <- 0 # anything not with trees, should be 0 bc land w/ no trees or water

  tree.city <- resample(tree.city, tdev, na.rm=F) # Do this next to make similar to surface temp
  tree.city <- crop(tree.city, extent(city.sp)) # Re-crop now that we've resampled & don't have edge effects
  
  tree.city <- mask(tree.city, city.sp)
  
  # Mask out water bodies
  if(length(ocean.city)>0) tree.city <- mask(tree.city, ocean.city, inverse=T)
  if(length(lakes.city)>0) tree.city <- mask(tree.city, lakes.city, inverse=T)
  if(length(river.city)>0) tree.city <- mask(tree.city, river.city, inverse=T)
  # plot(tree.city); plot(city.sp, add=T)
  
  vals.tree <- getValues(tree.city)
  # ---------------
  
  # ---------------
  # Package everything together for more robust analysis
  # ---------------
  png(file.path(path.save, paste0(city.name, "_maps.png")), height=8, width=6, unit="in", res=180)
  par(mfrow=c(2,2))
  plot(tmax, main="Summer Day Max Temp\n2013, deg.C"); plot(city.sp, add=T)
  plot(tdev, main="Summer Day Max Dev\n2013, deg.C"); plot(city.sp, add=T)
  plot(tree.city, main="Tree Cover\n 2013, perc. cover"); plot(city.sp, add=T)
  plot(elev.city, main="Elevation; unknown units"); plot(city.sp, add=T)
  par(mfrow=c(1,1))
  dev.off()
  
  # test <- coordinates(tree.city2)
  
  df.city <- data.frame(Name=city.name, coordinates(tdev), cover.tree=vals.tree, temp.summer=vals.tmax, temp.dev.summer=vals.tdev, elevation=vals.elev)
  df.city <- df.city[!is.na(df.city$elevation),]
  write.csv(df.city, file.path(path.save, paste0(city.name, "_data_full.csv")), row.names=F)
  # summary(df.city)
  lm.city <- lm(temp.dev.summer ~ cover.tree, data=df.city)
  sum.lm <- summary(lm.city)
  
  # gam.test <- mgcv::gam(temp.summer ~ cover.tree + elevation + s(x,y), data=df.city)
  # plot(gam.test)
  # summary(gam.test)
  # 
  png(file.path(path.save, paste0(city.name, "_scatter.png")), height=4, width=4, unit="in", res=90)
  plot(temp.dev.summer ~ cover.tree, data=df.city, cex=0.5,
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
  cities.use[i,"tdev.mean"] <- mean(df.city$temp.dev.summer, na.rm=T)
  cities.use[i,"tdev.sd"  ] <- sd(df.city$temp.dev.summer, na.rm=T)
  cities.use[i,"tdev.max" ] <- max(df.city$temp.dev.summer, na.rm=T)
  cities.use[i,"tdev.min" ] <- min(df.city$temp.dev.summer, na.rm=T)
  # cities.use[i,"correlation"] <- sum.lm$r.squared
  # cities.use[i,"slope"] <- sum.lm$coefficients[2,1] 
  
  write.csv(data.frame(cities.use), "../data_processed/cities_summary_sdei_v4.csv", row.names=F)
  rm(ocean.city, lakes.city, river.city)
} # End city loop
# cities.use <- cities.use[,!names(cities.use) %in% c("july.mean", "july.sd", "july.max", "july.min")]
summary(cities.use)
# ---------------
# -----------------------------------------


# -----------------------------------------
# Looking at some quick summary stats
# -----------------------------------------
cities.use <- data.frame(cities.use)
summary(cities.use)

# -----------------------------------------
