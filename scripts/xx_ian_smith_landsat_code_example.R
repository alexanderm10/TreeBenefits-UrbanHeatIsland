# Code sent over from Iain Smith (UBoston)
# https://code.earthengine.google.com/807ef02d3fc7a194a7604f289f85a56c

# adapting to combine the AOI selection from the MODIS analysis to pull appropriate data for Landsat8

# Selecting cities (AOI) on 50K or more people across 100sqkm or more area
##################### 
# 0. Initializing google earth engine.
#####################
library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'malexander@anl.gov', drive=T, project="nbs2023-malexander")
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "northstar2023"
assetHome <- ee_get_assethome()

##################### 
# 1. Load and select cities
#####################
sdei.df <- data.frame(vect("input_data/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))

# Subsetting the data to be just the US cities
sdei.df2 <- sdei.df[sdei.df$ISO3=="USA",]

sdei.df3 <- sdei.df2[sdei.df2$ES00POP>=50e3 & sdei.df2$SQKM_FINAL>=1e2,] # changed the filter to be 50K people over 100Sq km
cityIDsAll <- sdei.df3$ISOURBID

sdei <- ee$FeatureCollection('users/malexander/nbs2023-malexander');
# print(sdei.first())

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 50e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
# ee_print(citiesUse) # Thsi function gets the summary stats; this gives us 2,682 cities

# Use map to go ahead and create the buffer around everything
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})
# ee_print(citiesUse)
##################### 

##################### 
# 2. Query GEE for Summer Landsat8 data from 2014-2022; for the cities of interest
#####################

collection <- ee$ImageCollection("LANDSAT/LE08/CO2/T1_L2")$filter(ee$Filter$dayOfYear(181,240))$filter(ee$Filter$date("2014-01-01", "2022-12-31"))$filter(ee$Filter$dayOfYear(181,240))$filter(ee$Filter$bounds(citiesUse))


##################### 
# 3. Download to google drive; filter for clouds/water, scale (to units of K), and crop to AOI each image in the collection
#####################

# I think that TEMPERATURE in the function below will be the 'collection' object specified above.

data.exract <- function(CitySP, CityNames, TEMPERATURE, GoogleFolderSave, overwrite=F, ...){
  # cityseq <- seq_len(CITIES$length()$getInfo())
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    cityID <- CityNames[i]
    CityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    
    # Up to here I have a single city selected from both the list of cities and the ee object shape file.

    ### Start Ian's Code below    
collection.evaluate(function(collection) { # Not sure what these two lines are doing
  collection.features.forEach(function(feature) {
    
    # // define image for each date
    image <- ee$Image(feature$id); # ee$Feature$id(collection);
    
    # // extract the date information
    date <-  ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd'); # ee$Date('system:time_start')$format('YYY-MM-dd')
    
    # // create a filename based on the data
    filename = 'landsat_8_' + date.getInfo(); # filename <- paste0('landsat_8_', i, date$getInfo());
    
    # // function to mask based on the QA_PIXEL band. 1 = good data, 0 = cloudy/water
    # // QA_PIXEL value interpretations from Table 5.6 of product guide (https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/media/files/LSDS-1618_Landsat-4-7_C2-L2-ScienceProductGuide-v4.pdf)
    # // QA_PIXEL values can change depending on application
     qa <- function(image) {
      return(image$where(image$select('QA_PIXEL')$neq(1)$and(image$select('QA_PIXEL')$neq(5440)), 0));
      return(image$where(image$select('QA_PIXEL')$eq(1)4$or(image$select('QA_PIXEL')$eq(5440)), 1));
    };
    
    # // create the mask
    image_qa <- ee$qa(image)
    
    
    # // apply the mask
    image_qa <- image_qa$updateMask(image_qa$select('QA_PIXEL'))
    
    # // create new raster with only the LST band and QA_PIXEL band
    lst <- image_qa$select(['ST_B6', 'QA_PIXEL']);
    
    # // clip to the aoi
    var lst_clip <- lst$clip(CityNow); lst_clip <- lst$clip(CityNow)
    
    # // Apply the scaling factor to the filtered, clipped data
    var lst_k = lst_clip$select('ST_B6')$multiply(0.00341802)$add(149)$rename('lst_k');
    
    # // visualize; MRA: Not sure I need this for the R script
    Map.addLayer(lst_k)
    
    # // export to google drive folder
    Export.image.toDrive({
      image: lst_k.toFloat(),
      description: filename,
      folder:'nbs2023-malexander', # // update for name of google drive folder
      scale: 30,
      region: CityNow,
      maxPixels: 1e13,
      formatOptions: {
        cloudOptimized: true
      }
    });
  });
});

# tempJulAug <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2014-01-01", "2022-12-31"))$map(addTime);

