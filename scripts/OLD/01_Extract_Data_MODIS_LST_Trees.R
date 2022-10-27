# Extract informaiton on surface temperature 

# City boudnaries from NASA's Socioeconomic Data and Applicaitons Center (Hosted by CIESIN at Columbia)
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013

# Water Bodies from here: http://openstreetmapdata.com/data/water-reduced-polygons

# Summer Land Surface Temperature Grids for 2011-2015: MODIS summer temperature; MOD11A2
# Data downloaded from here: https://ladsweb.modaps.eosdis.nasa.gov/search/
#  https://ladsweb.modaps.eosdis.nasa.gov/api/v1/productPage/product=MOD11A2


# Extract info on tree cover and mean summer temp for major urban areas around the world
# Datasets to use:
# 1. Urban Areas: SDEI dataset
# 2. Tree Cover Data: MODIS MOD44B
# 3. Climate Data: MODIS MOD11A


library(sp); library(rgdal); library(raster); library(rgeos); library(maps)

# -------------------
# Setting up cities to analyze
# -------------------
# Set the buffer size to re-calculate urban heat island effect
city.buff <- 10e3 # in meters; sdei dataset foudn 10km to have the least overlap with nearby cities

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
# hist(cities.use$D_T_DIFF)
# data.frame(cities.use[cities.use$URB_N_MEAN<10,])

# Plot the map to get a better feel for geographic distribution
png("../data_processed/cities_used_spdei_v6.png", height=4, width=8, units="in", res=120)
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

write.csv(data.frame(cities.use), "../data_processed/cities_summary_sdei_v6.csv", row.names=F)
# -------------------

# -------------------
# Geographic features
# -------------------
# Elevation as an additional predictor to account for potential bias in tree cover distribution
elev <- raster("/Volumes/Morton_SDM/Elevation_SRTM30/topo30/topo30.grd")


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
# -------------------

# -------------------
# Setting up Vegetation cover
# -------------------
# Doing some indexing of tree cover data; all of these were extracted from the same file & should have the same indices
cover.base <- "/Volumes/Morton_SDM/ContinuousVegFields_MOD44Bv6/geotiff"
path.trees <- file.path(cover.base, "Cover_Veg_Tree") # Percent Tree Cover
path.veg <- file.path(cover.base, "Cover_Veg_Non-Tree") # Percent cover of non-tree vegetation
path.noveg <- file.path(cover.base, "Cover_Non-Veg") # Percent cover of unvegetated surfaces

# Generating file lists for all cover types
ftree <- dir(path.trees, ".tif")
ftree <- ftree[which(substr(ftree, nchar(ftree)-3, nchar(ftree))==".tif")] # ignore anything that's not a .tif
fveg <- dir(path.veg, ".tif")
fveg <- fveg[which(substr(fveg, nchar(fveg)-3, nchar(fveg))==".tif")] # ignore anything that's not a .tif
fnoveg <- dir(path.noveg, ".tif")
fnoveg <- fnoveg[which(substr(fnoveg, nchar(fnoveg)-3, nchar(fnoveg))==".tif")] # ignore anything that's not a .tif

# Using tree cover to set up our file index
ftree.split <- stringr::str_split(ftree, "[.]")
ftree.df <- data.frame(file=ftree, matrix(unlist(ftree.split), ncol=length(ftree.split[[1]]), byrow = T))
names(ftree.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
ftree.df$date <- strptime(as.numeric(substr(ftree.df$date.stamp, 2, 8)), "%Y%j")
ftree.df$year <- lubridate::year(ftree.df$date)
ftree.df$month <- lubridate::month(ftree.df$date)
ftree.df$yday <- lubridate::yday(ftree.df$date)
summary(ftree.df)
# -------------------

# -------------------
# Handy functions to help with processing
# -------------------
# Setting up a function to iteratively remove outliers since we do it a LOT
# Iteratively removing 6-sigma outliers
#  -- not great, but necessary to do at the large scale before clipping to cities with lower SD
#  -- in my test, this kept ~95% of data with the biggest outliers being on the coast;
# if this becomes too problematic, can do deviation from tile mean and do the multi-time analysis
#      with na.rm=T, but that might need to happen at a city-scale
#  but we'll need to
filter.outliers <- function(RASTER, n.sigma=6, max.iters=10){
  vals.tmp <- getValues(RASTER); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
  ITER=1
  while(length(which(vals.tmp < tmp.x-n.sigma*tmp.sd | vals.tmp > tmp.x+n.sigma*tmp.sd))>0 & ITER<max.iters){
    RASTER[RASTER < tmp.x-n.sigma*tmp.sd]  <- NA
    RASTER[RASTER > tmp.x+n.sigma*tmp.sd]  <- NA
    
    vals.tmp <- getValues(RASTER); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
    ITER=ITER+1
  } # end while loop
  
  return(RASTER)
} # End function

# Stealing a home-brew standard deviation funciton because raster functions won't pass na.rm=T
rast.sd=function(x, na.rm=F){
  v = var(x,na.rm = na.rm)
  l = if(na.rm) sum(!is.na(x)) else length(x)
  return(sqrt(v*(l-1)/l))
}

# Source a file where we figure out and set our encoding flags
source("MODIS_QA_Flags_Encoding.R") 
# -------------------

# -----------------------------------------
# Looping through Cities
# -----------------------------------------
# cities.use$NAME
save.base <- "../data_processed/cities_full_sdei_v6"

# yr.process <- 2011:2015
yr.process=2014
for(YEAR in yr.process){
  path.save <- file.path(save.base, YEAR)
  dir.create(path.save, recursive=T, showWarnings = F)
  
    
  # -------------------
  # Setting up an index for our met files
  # -------------------
  # Setting up file paths and directories for the temperature data; QA files should have same indexing
  met.base <- file.path("/Volumes/Morton_SDM/SurfTemp_MODIS_MODA2/processed", YEAR, "geotiff")
  path.met <- file.path(met.base, "MOD11A2_LST_Day")
  path.qa <- file.path(met.base, "MOD11A2_LST_Day_QA")
  
  fmet.all <- dir(path.met, ".tif")
  fmet.all <- fmet.all[which(substr(fmet.all, nchar(fmet.all)-3, nchar(fmet.all))==".tif")] # ignore anything that's not a .tif
  
  fmet.qa.all <- dir(path.qa, ".tif")
  fmet.qa.all <- fmet.qa.all[which(substr(fmet.qa.all, nchar(fmet.qa.all)-3, nchar(fmet.qa.all))==".tif")] # ignore anything that's not a .tif
  
  fmet.split <- stringr::str_split(fmet.all, "[.]")
  fmet.df <- data.frame(file=fmet.all, matrix(unlist(fmet.split), ncol=length(fmet.split[[1]]), byrow = T))
  names(fmet.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
  fmet.df$date <- strptime(as.numeric(substr(fmet.df$date.stamp, 2, 8)), "%Y%j")
  fmet.df$year <- lubridate::year(fmet.df$date)
  fmet.df$month <- lubridate::month(fmet.df$date)
  fmet.df$yday <- lubridate::yday(fmet.df$date)
  summary(fmet.df)
  summary(fmet.df$tile)
  # length(unique(fmet.df$date.stamp))
  # -------------------
  
  
  # -------------------
  # Creating an index of MODIS tiles so we can index off of that alone
  # -------------------
  # Using our land surface temperature to determine what tiles we have
  # tile1 <- unique(fmet.df[fmet.df$date==min(fmet.df$date),"tile"])
  # tile2 <- unique(fmet.df[fmet.df$date==max(fmet.df$date),"tile"])
  
  # tile1[!tile1 %in% tile2]
  # tile2[!tile2 %in% tile1]
  modis.df <- fmet.df[fmet.df$date==max(fmet.df$date),]
  summary(modis.df); dim(modis.df)
  
  modis.df[,c("xmin", "xmax", "ymin", "ymax")] <- NA
  for(i in 1:nrow(modis.df)){
    tmp <- raster(file.path(path.met, modis.df$file[i]))
    
    modis.df[i,c("xmin", "xmax", "ymin", "ymax")] <- extent(tmp)
  }
  summary(modis.df)
  # -------------------
  
  
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
    city.raw <- cities.use[i,]
    # data.frame(city.sp)
    city.name <- city.raw$NAME
    city.name <- sub(c("[?]"), "X", city.name) # Getting rid of the ? infront of some cities
    city.name <- sub(c("[?]"), "X", city.name) # Being lazy and doing this twice just in case to get rid of the rare ??
    
    # Check to see if we should jump to the next file
    if(file.exists(file.path(path.save, paste0(city.name, "_data_full.csv")))) next
    
    
    # Create a 10km buffer around city area to analyze UHI
    # -- Note: To do this we need to take make it into a projection with m as using to do this
    # Stealing from the oceans projection
    city.sp0 <- spTransform(city.raw, CRS("+init=epsg:3347"))
    
    # Add buffers ranging from 5 to 25 km; using the largest for our base
    city.buff05 <- gBuffer(city.sp0, width=5e3)
    city.buff10 <- gBuffer(city.sp0, width=10e3)
    city.buff15 <- gBuffer(city.sp0, width=15e3)
    city.buff20 <- gBuffer(city.sp0, width=20e3)
    city.sp <- gBuffer(city.sp0, width=25e3)
    
    # Transform back to the projection of everything else
    city.buff05 <- spTransform(city.buff05, projection(city.raw))
    city.buff10 <- spTransform(city.buff10, projection(city.raw))
    city.buff15 <- spTransform(city.buff15, projection(city.raw))
    city.buff20 <- spTransform(city.buff20, projection(city.raw))
    city.sp <- spTransform(city.sp, projection(city.raw))
    
    # Use our largest extraction for our base
    bb.city <- bbox(city.sp) # Use the largest buffer
    # plot(city.sp, col="blue"); plot(city.raw, col="red", add=T)
    
    
    ocean.city <- lakes.city <- river.city <- NULL
    ocean.city <- crop(oceans, extent(city.sp))
    lakes.city <- crop(lakes, extent(city.sp))
    river.city <- crop(rivers, extent(city.sp))
    
    # plot(city.sp); plot(city.raw, add=T)
    # if(length(ocean.city)>0) plot(ocean.city, add=T, col="blue3");
    # if(length(lakes.city)>0) plot(lakes.city, add=T, col="cadetblue2");
    # if(length(river.city)>0) plot(river.city, add=T, col="dodgerblue2")
    # plot(city.sp, add=T); plot(city.raw, add=T)
    
    # 2. Figure out which rasters to process
    mod.ind <- which(((modis.df$ymin<=bb.city[2,1] & modis.df$ymax>=bb.city[2,1]) |  # raster min < min & raster min > min
                      (modis.df$ymax>=bb.city[2,2] & modis.df$ymin<=bb.city[2,2])) & # max is greater than both
                      ((modis.df$xmin<=bb.city[1,1] & modis.df$xmax>=bb.city[1,1]) |  # raster min < min & raster min > min
                        (modis.df$xmax>=bb.city[1,2] & modis.df$xmin<=bb.city[1,2])))
    
    modis.tiles <- modis.df[mod.ind,"tile"]
    
    
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
      mos.use <- 7:8
    } else {
      # Else use January
      mos.use <- 1:2
    } # End N/S identification
    
    # 2. Figure out which rasters to process & make sure we have all of the time slices
    f.met <- which(fmet.df$tile %in% modis.tiles & fmet.df$year==YEAR & fmet.df$month %in% mos.use)
    if(length(unique(fmet.df[f.met, "date.stamp"])) < length(unique(fmet.df[fmet.df$year==YEAR & fmet.df$month %in% mos.use,"date.stamp"]))) stop(paste("missing scenes for tile:", modis.tiles))
    
    # 3. For *each date* 
    #    3.1. moasaic, then filter, then mask
    #    3.2. calculate spatial deviation from city mean
    tmax <- stack()
    for(NOW in unique(fmet.df[f.met, "date.stamp"])){
      df.now <- fmet.df[f.met,]
      ind.now <- which(df.now$date.stamp==NOW)
      
      files.now <- fmet.all[f.met[ind.now]]
      
      qa.now <- fmet.qa.all[f.met[ind.now]]
      
      if(length(files.now)==1){
        met.city <- raster(file.path(path.met, files.now))
        met.city[met.city==00] <- NA # Going back to our 250 cutoff because things aren't looking right
        met.city <- met.city*0.02
        
        qa.city <- raster(file.path(path.qa, qa.now))
        qa.city[is.na(met.city)] <- NA
        
        met.city[!(qa.city %in% flags.good)] <- NA
        # plot(met.city); plot(city.sp, add=T)
        
      } else {
        met.city <- raster(file.path(path.met, files.now[1]))
        met.city <- met.city*0.02 # Scale factor from documentation; now in Kelvin
        met.city[met.city==0] <- NA # 0 = missing data
        # plot(met.city); plot(city.sp, add=T)
        
        qa.city <- raster(file.path(path.qa, qa.now[1]))
        qa.city[is.na(met.city)] <- NA # Make this match our missing data
        # vals.qa <- getValues(qa.city)
        # unique(vals.qa)
        # plot(qa.city); plot(city.sp, add=T)
        
        met.city[!(qa.city %in% flags.good)] <- NA
        # plot(qa.city2); plot(city.sp, add=T)
        # plot(met.city); plot(city.sp, add=T)
        
        for(j in 2:length(files.now)){
          met2 <- raster(file.path(path.met,files.now[j]))
          met2 <- met2*0.02 # Scale factor from documentation; now in Kelvin
          met2[met2==0] <- NA # 250k = -23˚C = -9.7F; almost certainly not a sumemr temperature
          qa2 <- raster(file.path(path.qa, qa.now[j]))
          qa2[is.na(met2)] <- NA # Make this match our missing data
          
          met2[!(qa2 %in% flags.good)] <- NA
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
      met.city <- filter.outliers(RASTER=met.city, n.sigma=4)
      # plot(met.city); plot(city.sp, add=T)
      
      # Crop & mask out our city area
      met.city <- crop(met.city, city.sp)
      met.city <- mask(met.city, city.sp)
      
      # Mask out water bodies
      if(length(ocean.city)>0) met.city <- mask(met.city, ocean.city, inverse=T)
      if(length(lakes.city)>0) met.city <- mask(met.city, lakes.city, inverse=T)
      if(length(river.city)>0) met.city <- mask(met.city, river.city, inverse=T)
      
      # plot(met.city); plot(city.sp, add=T)
      
      tmax <- addLayer(tmax, met.city)
      
      rm(met.city)
    } # End Time loop
    # plot(tmax)
    
    # Filter outliers one more time before taking the mean; because we have multiple time bands, it's less likely to remove true data
    tmax <- filter.outliers(RASTER=tmax, n.sigma=4)
    
    # Calculate deviations on the post-filtered tmax
    tdev <- stack()
    for(LAY in 1:nlayers(tmax)){
      # mean.now <- mean(getValues(tmax[[i]]), na.rm=T)
      tdev <- addLayer(tdev, tmax[[LAY]] - mean(getValues(tmax[[LAY]]), na.rm=T))
    }
    
    # setting up to look at the number of slcies
    tn <- tmax
    tn[!is.na(tn)] <- 1
    # plot(tmax)
    # plot(tdev)
    # plot(tn)
    
    # 4. Find & store mean from all time points
    tmax.sd <- calc(tmax, fun=rast.sd, na.rm=T)
    tmax <- mean(tmax, na.rm=T)
    tdev.sd <- calc(tdev, fun=rast.sd, na.rm=T)
    tdev <- mean(tdev, na.rm=T)
    tn <- sum(tn, na.rm=T)
    # plot(tmax); plot(city.raw, add=T)
    # plot(tmax.sd); plot(city.raw, add=T)
    # plot(tdev); plot(city.raw, add=T)
    # plot(tdev.sd); plot(city.raw, add=T)
    # plot(tn); plot(city.raw, add=T)
    
    
    # Save values as vectors
    vals.tmax <- getValues(tmax)
    vals.tmax.sd <- getValues(tmax.sd)
    vals.tdev <- getValues(tdev)
    vals.tdev.sd <- getValues(tdev.sd)
    vals.tn <- getValues(tn)
    
    # If we can't get good temperature data, skip this; right now this is a pretty low bar, but we'll see
    if(length(which(!is.na(vals.tdev)))<10) next
    # plot(tmax); plot(city.sp, add=T)
    # plot(tdev); plot(city.sp, add=T)
    # plot(tn); plot(city.sp, add=T)    
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
      extent(city1) <- extent(extent(city1)[1:2]-360,extent(city1)[3:4])
      # city1 <- extend(city1, ext.city)
      
      city2 <- crop(elev, ext2)
      # extent(city2) <- extent(extent(city2)[1:2]-360,extent(city2)[3:4])
      # extent(city2) <- extent(0,ext.city[2],ext.city[3:4])
      # city2 <- extend(city2, ext.city)
      
      elev.city <- mosaic(city1, city2, fun=mean, tolerance=0.2)
    }
    
    # elev.city <- filter.outliers(RASTER = elev.city, n.sigma=4)
    elev.city <- resample(elev.city, tdev)
    elev.city <- crop(elev.city, tdev)
    elev.city <- mask(elev.city, city.sp)
    
    # Mask out water bodies
    if(length(ocean.city)>0) elev.city <- mask(elev.city, ocean.city, inverse=T)
    if(length(lakes.city)>0) elev.city <- mask(elev.city, lakes.city, inverse=T)
    if(length(river.city)>0) elev.city <- mask(elev.city, river.city, inverse=T)
    # plot(elev.city); plot(city.raw, add=T)
    
    # Filter out things beyond 6 sigma
    # elev.city <- filter.outliers(RASTER = elev.city, n.sigma = 6)
    vals.elev <- getValues(elev.city)
    
    # Use the elevation raster (which is the most reliable) to generate our city/buffer vector
    elev.raw <- mask(elev.city, city.raw)
    elev.05 <- mask(elev.city, city.buff05)
    elev.10 <- mask(elev.city, city.buff10)
    elev.15 <- mask(elev.city, city.buff15)
    elev.20 <- mask(elev.city, city.buff20)
    
    elev.raw <- getValues(elev.raw)
    elev.05 <- getValues(elev.05)
    elev.10 <- getValues(elev.10)
    elev.15 <- getValues(elev.15)
    elev.20 <- getValues(elev.20)
    
    city.buff <- ifelse(is.na(vals.elev), NA, 
                        ifelse(!is.na(elev.raw), 0, 
                               ifelse(!is.na(elev.05), 5, 
                                      ifelse(!is.na(elev.10), 10, 
                                             ifelse(!is.na(elev.15), 15, 
                                                    ifelse(!is.na(elev.20), 20, 25))))))
    # summary(as.factor(city.buff))
    # plot(elev.city); plot(city.raw, add=T)
    # ---------------
    
    # ---------------
    # Tree Canopy
    # --------------
    # 2. Figure out which rasters to process
    f.veg <- which(ftree.df$tile %in% modis.tiles & ftree.df$year==YEAR)
    
    # For some reason we have multiple files for some of our tree covers
    # These have different process time stamps; lets go with whatever is most recent
    if(length(f.veg)>length(modis.tiles)){
      process.stamps <- as.numeric(substr(ftree.df[f.veg,"process.stamp"], 1, 6))
      f.veg <- f.veg[process.stamps==max(process.stamps)]
    }
    
    if(length(f.veg)==0) next
    if(length(f.veg)==1){
      tree.city <- raster(file.path(path.trees, ftree[f.veg]))
      tree.city[tree.city>100] <- NA
      tree.city[tree.city==0] <- NA
      
      veg.city <- raster(file.path(path.veg, fveg[f.veg]))
      veg.city[veg.city>100] <- NA
      veg.city[veg.city==0] <- NA
      
      noveg.city <- raster(file.path(path.noveg, fnoveg[f.veg]))
      noveg.city[noveg.city>100] <- NA
      noveg.city[noveg.city==0] <- NA
      
      # tree.city
    } else {
      tree.city <- raster(file.path(path.trees, ftree[f.veg[1]]))
      tree.city[tree.city>100] <- NA
      tree.city[tree.city<=0] <- NA
      
      veg.city <- raster(file.path(path.veg, fveg[f.veg[1]]))
      veg.city[veg.city>100] <- NA
      veg.city[veg.city==0] <- NA
      
      noveg.city <- raster(file.path(path.noveg, fnoveg[f.veg[1]]))
      noveg.city[noveg.city>100] <- NA
      noveg.city[noveg.city==0] <- NA
      # plot(tree.city); plot(city.sp, add=T)
      # plot(veg.city); plot(city.sp, add=T)
      # plot(noveg.city); plot(city.sp, add=T)
      
      for(j in 2:length(f.veg)){
        tree2 <- raster(file.path(path.trees, ftree[f.veg[j]]))
        tree2[tree2>100] <- NA
        tree2[tree2<=0] <- NA
        # plot(tree2, add=T); plot(city.sp, add=T)
        
        veg2 <- raster(file.path(path.veg, fveg[f.veg[j]]))
        veg2[veg2>100] <- NA
        veg2[veg2<=0] <- NA
        # plot(veg2, add=T); plot(city.sp, add=T)
        
        noveg2 <- raster(file.path(path.noveg, fnoveg[f.veg[j]]))
        noveg2[noveg2>100] <- NA
        noveg2[noveg2<=0] <- NA
        # plot(noveg2, add=T); plot(city.sp, add=T)
        
        ext1 <- extent(tree.city)
        ext2 <- extent(tree2)
        ext.temp <- c(min(ext1[1], ext2[1]),
                      max(ext1[2], ext2[2]),
                      min(ext1[3], ext2[3]),
                      max(ext1[4], ext2[4]))
        tree.city <- extend(tree.city, ext.temp)
        veg.city <- extend(veg.city, ext.temp)
        noveg.city <- extend(noveg.city, ext.temp)
        
        tree2 <- resample(tree2, tree.city)
        veg2 <- resample(veg2, tree.city)
        noveg2 <- resample(noveg2, tree.city)
        # plot(tree2); plot(city.sp, add=T)
        
        tree.city <- mosaic(tree.city, tree2, fun=mean, na.rm=T, tolerance=0.2)
        veg.city <- mosaic(veg.city, veg2, fun=mean, na.rm=T, tolerance=0.2)
        noveg.city <- mosaic(noveg.city, noveg2, fun=mean, na.rm=T, tolerance=0.2)
      }
    } 
    # plot(tree.city); plot(city.sp, add=T)
    # plot(veg.city); plot(city.sp, add=T)
    # plot(noveg.city); plot(city.sp, add=T)
    
    # anything not with trees, should be 0 bc land w/ no trees or water; note: do this before resampling otherwise outliers become 0 instead of NA
    tree.city[is.na(tree.city)] <- 0 
    veg.city[is.na(veg.city)] <- 0 
    noveg.city[is.na(noveg.city)] <- 0 
    
    # Iteratively removing 6-sigma outliers from large scene; not local area
    # tree.city <- filter.outliers(RASTER = tree.city, n.sigma=6)
    
    # Resample to make similar to surface temp
    tree.city <- resample(tree.city, tdev, na.rm=F) 
    tree.city <- crop(tree.city, extent(city.sp)) 
    
    veg.city <- resample(veg.city, tdev, na.rm=F) 
    veg.city <- crop(veg.city, extent(city.sp)) 
    
    noveg.city <- resample(noveg.city, tdev, na.rm=F) 
    noveg.city <- crop(noveg.city, extent(city.sp)) 
    
    # Masking
    tree.city <- mask(tree.city, city.sp)
    veg.city <- mask(veg.city, city.sp)
    noveg.city <- mask(noveg.city, city.sp)
    
    # Mask out water bodies
    if(length(ocean.city)>0){
      tree.city <- mask(tree.city, ocean.city, inverse=T)
      veg.city <- mask(veg.city, ocean.city, inverse=T)
      noveg.city <- mask(noveg.city, ocean.city, inverse=T)
    } 
    if(length(lakes.city)>0){
      tree.city <- mask(tree.city, lakes.city, inverse=T)
      veg.city <- mask(veg.city, lakes.city, inverse=T)
      noveg.city <- mask(noveg.city, lakes.city, inverse=T)
    } 
    if(length(river.city)>0){
      tree.city <- mask(tree.city, river.city, inverse=T)
      veg.city <- mask(veg.city, river.city, inverse=T)
      noveg.city <- mask(noveg.city, river.city, inverse=T)
    } 
    # plot(tree.city); plot(city.raw, add=T)
    # plot(veg.city); plot(city.raw, add=T)
    # plot(noveg.city); plot(city.raw, add=T)
    
    vals.tree <- getValues(tree.city)
    vals.veg <- getValues(veg.city)
    vals.noveg <- getValues(noveg.city)
    # ---------------
    
    # ---------------
    # Package everything together for more robust analysis
    # ---------------
    png(file.path(path.save, paste0(city.name, "_maps.png")), height=8, width=11, unit="in", res=180)
    par(mfrow=c(2,4))
    plot(tn, main=paste("Number Temp Slices\n", YEAR)); plot(city.raw, add=T)
    plot(tmax, main=paste("Summer Day Temp\n", YEAR, "deg. C")); plot(city.raw, add=T)
    plot(tmax.sd, main=paste("Summer Day Temp SD\n", YEAR, "deg. C")); plot(city.raw, add=T)
    plot(tdev, main=paste("Summer Day Mean Dev\n", YEAR, "deg. C")); plot(city.raw, add=T)
    plot(elev.city, main="Elevation (m)"); plot(city.raw, add=T)
    plot(tree.city, main=paste("Veg. Cover: Trees\n", YEAR, "% cover")); plot(city.raw, add=T)
    plot(veg.city, main=paste("Veg. Cover: Non-Tree\n", YEAR, "% cover")); plot(city.raw, add=T)
    plot(noveg.city, main=paste("Non-Veg. Cover\n", YEAR, "% cover")); plot(city.raw, add=T)
    par(mfrow=c(1,1))
    dev.off()
    
    # test <- coordinates(tree.city2)  
    df.city <- data.frame(Name=city.name, coordinates(tdev), 
                          location=city.buff, 
                          elevation=vals.elev, # Elevation in meters
                          temp.n = vals.tn, # Number of temperature scenes used
                          temp.summer=vals.tmax, 
                          temp.summer.sd=vals.tmax.sd, 
                          temp.dev.summer=vals.tdev,
                          temp.dev.summer.sd=vals.tdev.sd,
                          cover.tree=vals.tree, 
                          cover.veg=vals.veg,
                          cover.noveg=vals.noveg
    )
    df.city <- df.city[!is.na(df.city$location),]
    summary(df.city)
    write.csv(df.city, file.path(path.save, paste0(city.name, "_data_full.csv")), row.names=F)
    
    
    # summary(df.city)
    lm.city <- lm(temp.dev.summer ~ cover.tree, data=df.city)
    sum.lm <- summary(lm.city)
    
    # gam.test <- mgcv::gam(temp.summer ~ cover.tree + elevation + s(x,y), data=df.city)
    # plot(gam.test)
    # summary(gam.test)
    # 
    png(file.path(path.save, paste0(city.name, "_scatter.png")), height=8, width=8, unit="in", res=120)
    plot(temp.dev.summer ~ cover.tree, data=df.city, cex=0.5,
         main=paste0("R2=",round(sum.lm$r.squared,2))); 
    abline(lm.city, col="red", lwd=2); 
    dev.off()
    
    
    # write.csv(data.frame(cities.use), "../data_processed/cities_summary_sdei_v6.csv", row.names=F)
    rm(ocean.city, lakes.city, river.city, tmax, tmax.sd, tdev, tdev.sd, tn, tree.city, veg.city, noveg.city)
  } # End city loop

} # End YEAR loop
# cities.use <- cities.use[,!names(cities.use) %in% c("july.mean", "july.sd", "july.max", "july.min")]
summary(cities.use)
# ---------------
# -----------------------------------------

