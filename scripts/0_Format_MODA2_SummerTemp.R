# Format MODIS summer temperature
# Data downloaded from here: https://ladsweb.modaps.eosdis.nasa.gov/search/
#  https://ladsweb.modaps.eosdis.nasa.gov/api/v1/productPage/product=MOD11A2
# According to documentation here: the scale factor for land surface day temperature is 0.02: https://icess.eri.ucsb.edu/modis/LstUsrGuide/usrguide_1dtil.html#Table_9
# Rather relying on the apparently buggy SDEI product, I'm going the raw data and looking at July temperature for the Northern Hemisphere and January temperature for the Southern.  Still doing 2013 because that's what I'd already pulled for the new MODIS tree cover dataset.  Will process all tiles for both seasons becuase cities often cross the equator.

library(raster)

path.01 <- "/Volumes/Morton_SDM/SurfTemp_MODIS_MODA2_2013-01/HEGOUT/"
path.07 <- "/Volumes/Morton_SDM/SurfTemp_MODIS_MODA2_2013-07/HEGOUT/"

fjan <- dir(path.01, ".tif")
fjan <- fjan[which(substr(fjan, nchar(fjan)-3, nchar(fjan))==".tif")] # ignore anything that's not a .tif
fjan.split <- stringr::str_split(fjan, "[.]")
fjan.df <- data.frame(file=fjan, matrix(unlist(fjan.split), ncol=length(fjan.split[[1]]), byrow = T))
names(fjan.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
summary(fjan.df)

fjul <- dir(path.07, ".tif")
fjul <- fjul[which(substr(fjul, nchar(fjul)-3, nchar(fjul))==".tif")] # ignore anything that's not a .tif
fjul.split <- stringr::str_split(fjul, "[.]")
fjul.df <- data.frame(file=fjul, matrix(unlist(fjul.split), ncol=length(fjul.split[[1]]), byrow = T))
names(fjul.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
summary(fjul.df)

# Loop through and find tile averages for each month
# Start with Jan
for(TILE in unique(fjan.df$tile)){
  ftile <- fjan.df[fjan.df$tile==TILE, "file"]
  
  tmp <- stack(file.path(path.01, ftile[1]))
  tmp[tmp==0] <- NA # This would be absoltue 0 --> impossible!!
  tmp <- tmp*0.02 # Scale factor from documentation; now in Kelvin
  
  # Iteratively removing 6-sigma outliers 
  #  -- not great, but necessary to do at the large scale before clipping to cities with lower SD
  #  -- in my test, this kept ~95% of data with the biggest outliers being on the coast
  vals.tmp <- getValues(tmp); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
  while(length(which(vals.tmp < tmp.x-6*tmp.sd | vals.tmp > tmp.x+6*tmp.sd))>0){
    tmp[tmp < tmp.x-6*tmp.sd]  <- NA
    tmp[tmp > tmp.x+6*tmp.sd]  <- NA
    
    vals.tmp <- getValues(tmp); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
  }
  
  tile.tmp <- stack(tmp)
  for(i in 2:length(ftile)){
    tmp <- raster(file.path(path.01, ftile[i]))
    tmp[tmp==0] <- NA
    tmp <- tmp*0.02 # Scale factor from documentation; now in Kelvin
    
    tmp <- resample(tmp, tile.tmp[[1]])
    
    # Iteratively removing 6-sigma outliers 
    #  -- not great, but necessary to do at the large scale before clipping to cities with lower SD
    #  -- in my test, this kept ~95% of data with the biggest outliers being on the coast; 
    # if this becomes too problematic, can do deviation from tile mean and do the multi-time analysis 
    #      with na.rm=T, but that might need to happen at a city-scale
    #  but we'll need to 
    vals.tmp <- getValues(tmp); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
    while(length(which(vals.tmp < tmp.x-6*tmp.sd | vals.tmp > tmp.x+6*tmp.sd))>0){
      tmp[tmp < tmp.x-6*tmp.sd]  <- NA
      tmp[tmp > tmp.x+6*tmp.sd]  <- NA
      
      vals.tmp <- getValues(tmp); tmp.x <- mean(vals.tmp, na.rm=T); tmp.sd <- sd(vals.tmp, na.rm=T)
    }
    
    tile.tmp <- stack(tile.tmp, tmp)
  } # End additional layers
  tile.tmean <- mean(tile.tmp)
  plot(tile.tmean)
  # plot(tile.tmean, xlim=c(-113, -111), ylim=c(24,26))
} # End tile loop

