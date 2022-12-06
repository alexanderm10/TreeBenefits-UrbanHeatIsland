# Looking at the impact of trees on reducing temperatures to <35˚C
# Note: 35˚C 'wet bulb' temperature is a biological health threshold; 
#       Sattelite data probably gives us closest to dry-bulb estimates
# 
# To Calculate Wet Bulb Temperature according to Stull 2011, we need: 
#  1. Air Temperature
#  2. Relative Humidity
#     - To Caluclate RH, we need specific humidity (Qair), Air Temperature, and Pressure
#     - We can get those values from GLDAS, but we really just want specific humidity & 
#       pressure so that we can include elevation changes for our pressure (since some areas 
#       have massive elevation effects)
#     - *KEY ASSUMPTION* we'll make the assumption that the whole area has the same SPECIFIC 
#       HUMIDITY and that RELATIVE HUMIDITY will be a function of temperature (plants!) and 
#       elevation
#
# To extract SPECIFIC HUMIDITY from GLDAS
#  - we just need 1 value per summer since that's what we average over for analyses
#  - Windows for Extraction: 
#     - Northern Hemisphere: July 1 (182) - August 31 (243)
#     - Southern Hemisphere: January 1 (1) - Feb 28 (59)
#     - Time Offset: ~15 degrees per hour --> 45˚ bands; Heading East --> get Earlier
#         - GLDAS Slices: 0, 3, 6, 9, 12, 15, 18, 21
#         - Prime Meridian: Lat >=-45/2 | LAT <=45/2: use 9-15
#         - East 1 = 45/2 to 45+45/2: 6-12
#         - East 2 = 45+45/2 to 45*2+45/2: 3-9
#         - East 3 = 45*2+45/2 to 45*3+45/2: 0-6
#         - West 1 = -45/2 to -45-45/2: 12-18
#         - West 2 = -45-45/2 to -45*2-45/2: 15-21
#         - West 3 = -45*2-45/2 to -45*3-45/2: 18, 21, 0
#         - East 4 = greater than 45*3+45/2: 21, 0, 3
#         - West 4 = greater than -45*3-45/2: 21, 0, 3




path.GLDAS <- "/Volumes/Celtis/Meteorology/LDAS/GLDAS_NOAH025_3H.2.1/"
yrs.use <- 2011:2015

# Read in our city list
dat.uhi <- read.csv("../data_processed/analysis_cities_summary_sdei_v6.csv")
summary(dat.uhi)

# To make code more efficient, we'll group our cities based on the time slices they need
dat.uhi$HEMI.NS <- ifelse(dat.uhi$LATITUDE>=0, "NORTH", "SOUTH")
dat.uhi$BAND.ES[dat.uhi$LONGITUDE>=-45/2 & dat.uhi$LONGITUDE<=45/2] <- "0900-1200-1500"
dat.uhi$BAND.ES[dat.uhi$LONGITUDE>45/2 & dat.uhi$LONGITUDE<=45+45/2] <- "1200-1500-1800"
dat.uhi$BAND.ES[dat.uhi$LONGITUDE>45 + 45/2 & dat.uhi$LONGITUDE<=45*2 + 45/2] <- "1500-1800-2100"
dat.uhi$BAND.ES[dat.uhi$LONGITUDE>45*2 + 45/2 & dat.uhi$LONGITUDE<=45*3 + 45/2] <- "1800-2100-0000"
dat.uhi$BAND.ES[dat.uhi$LONGITUDE>45*3 + 45/2 | dat.uhi$LONGITUDE< -45*3 - 45/2] <- "2100-0000-0300"
dat.uhi$BAND.ES[dat.uhi$LONGITUDE< -45/2 & dat.uhi$LONGITUDE>= -45 - 45/2] <- "0600-0900-1200"
dat.uhi$BAND.ES[dat.uhi$LONGITUDE< -45 - 45/2 & dat.uhi$LONGITUDE>= -45*2 - 45/2] <- "0300-0600-0900"
dat.uhi$BAND.ES[dat.uhi$LONGITUDE< -45*2 - 45/2 & dat.uhi$LONGITUDE>= -45*3 - 45/2] <- "0000-0300-0600"

dat.uhi$HEMI.NS <- as.factor(dat.uhi$HEMI.NS)
dat.uhi$BAND.ES <- as.factor(dat.uhi$BAND.ES)
summary(dat.uhi)

# Creating an easy index for GLDAS lat/lon bins
gldas.lat <- seq(-59.875, 89.875, by=0.25)
gldas.lon <- seq(-179.875, 179.875, by=0.25)

for(i in 1:nrow(dat.uhi)){
  dat.uhi$LAT.ind[i] <- which(gldas.lat-0.125<=dat.uhi$LATITUDE[i] & gldas.lat+0.125>=dat.uhi$LATITUDE[i])
  dat.uhi$LON.ind[i] <- which(gldas.lon-0.125<=dat.uhi$LONGITUDE[i] & gldas.lon+0.125>=dat.uhi$LONGITUDE[i])
}
summary(dat.uhi)

vars.extract <- c("Qair", "Psurf", "Tair", "Tsurf")

# Create a place holder data frame
dat.out <- merge(dat.uhi, data.frame(year=yrs.use))

for(HEMI in unique(dat.uhi$HEMI.NS)){
  if(HEMI=="NORTH"){
    days.use <- stringr::str_pad(182:243, 3, "left", 0)
  } else {
    days.use <- stringr::str_pad(1:59, 3, "left", 0)
  }
  
  print(HEMI)
  for(BAND in unique(dat.uhi$BAND.ES[dat.uhi$HEMI.NS==HEMI])){
    string.hrs <- strsplit(paste(BAND), "-")[[1]]
    
    cities.ind <- which(dat.uhi$BAND.ES==BAND & dat.uhi$HEMI.NS==HEMI)
    
    print(BAND)
    pb.yr <- txtProgressBar(min=0, max=length(days.use)*length(string.hrs)*length(yrs.use), style=3)
    pb.ind <- 0
    for(YR in yrs.use){
      # Create a placeholder array for this year to make the summarizing eaiser
      # Note: we only want 1 value per year
      array.tmp <- array(dim=c(length(cities.ind), length(vars.extract), length(days.use), length(string.hrs)))
      dimnames(array.tmp) <- list(CITY=dat.uhi$NAME[cities.ind],
                                  VAR=vars.extract,
                                  DAY=days.use,
                                  HOUR=string.hrs)
      # dim(array.tmp)
      
      # Loop through files in the year and store them
      for(DOY in days.use){
        for(HR in string.hrs){
          pb.ind=pb.ind+1
          setTxtProgressBar(pb.yr, pb.ind)
          dat.str <- as.Date(paste0(YR,DOY), "%Y%j")
          dat.str <- gsub("-", "", dat.str)
          
          # Open up the file
          ncT <- ncdf4::nc_open(file.path(path.GLDAS, YR, DOY, paste0("GLDAS_NOAH025_3H.A", dat.str, ".", HR, ".021.nc4")))
          Tsurf <- ncdf4::ncvar_get(ncT, "AvgSurfT_inst")
          Tair <- ncdf4::ncvar_get(ncT, "Tair_f_inst")
          Qair <- ncdf4::ncvar_get(ncT, "Qair_f_inst")
          Psurf <- ncdf4::ncvar_get(ncT, "Psurf_f_inst")
          ncdf4::nc_close(ncT)
          
          # Probably not the most efficient way to do this, but lets just loop through each city
          for(i in 1:length(cities.ind)){
            array.tmp[i,"Qair",paste(DOY),paste(HR)] <- Qair[dat.uhi$LON.ind[cities.ind[i]],dat.uhi$LAT.ind[cities.ind[i]]]
            array.tmp[i,"Psurf",paste(DOY),paste(HR)] <- Psurf[dat.uhi$LON.ind[cities.ind[i]],dat.uhi$LAT.ind[cities.ind[i]]]
            array.tmp[i,"Tair",paste(DOY),paste(HR)] <- Tair[dat.uhi$LON.ind[cities.ind[i]],dat.uhi$LAT.ind[cities.ind[i]]]
            array.tmp[i,"Tsurf",paste(DOY),paste(HR)] <- Tsurf[dat.uhi$LON.ind[cities.ind[i]],dat.uhi$LAT.ind[cities.ind[i]]]
          }
          # summary(array.tmp)
        } # End Hour
      } # End Day
      
      # Find the year mean
      yr.sum <- data.frame(apply(array.tmp, c(1,2), mean))
      # names(yr.sum) <- paste0("GLDAS.", names(yr.sum))
      # yr.sum$year <- YR
      # yr.sum$NAME <- row.names(yr.sum)
      summary(yr.sum)
      
      dat.out[dat.out$BAND.ES==BAND & dat.out$HEMI.NS==HEMI & dat.out$year==YR,paste0("GLDAS.", names(yr.sum))] <- yr.sum
      summary(dat.out)
    } # End Year
  }
}
write.csv(dat.out, "../data_processed/cities_full_sdei_v6/analysis_all_years/Cities_GLDAS_extract_2011-2015.csv", row.names=F)

library(ggplot2)

summary(dat.out[!is.na(dat.out$GLDAS.Psurf),])
ggplot(data=dat.out) +
  facet_wrap(~year, ncol=1) +
  coord_equal() +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=GLDAS.Qair), size=2)


ggplot(data=dat.out) +
  facet_wrap(~year, ncol=1) +
  coord_equal() +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=GLDAS.Psurf), size=2)

ggplot(data=dat.out) +
  facet_wrap(~year, ncol=1) +
  coord_equal() +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=GLDAS.Tair), size=2)

ggplot(data=dat.out) +
  facet_wrap(~year, ncol=1) +
  coord_equal() +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=GLDAS.Tsurf), size=2)
