# resaving the all_cities.csv in a format that will go into the db a bit easier

city.dat <- read.csv("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/processed_cities/city_stats_all2.csv",header=T)

head(city.dat)

city.dat$fema.region <- car::recode(city.dat$fema.region, "'region1'='Region 1';'region2'='Region 2';'region3'='Region 3';
                                         'region4'='Region 4';'region5'='Region 5';'region6'='Region 6';'region7'='Region 7';'region8'='Region 8';
                                         'region9'='Region 9';'region10'='Region 10'")
city.dat.db <- city.dat

# Replacing NA's with blank string
city.dat.db[is.na(city.dat.db)] <- ""


# reading in cities that passed QA/QC checks
sig.cities <- read.csv("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/sig_analyzed_cities.csv", header=T)

city.dat.db$analyzed <- ifelse(city.dat.db$ISOURBID %in% sig.cities$ISOURBID, "Y", "N")
head(city.dat.db)

saveRDS(city.dat.db, "C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/processed_cities/city_stats_all_db.rds")
write.csv(city.dat.db, "C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/processed_cities/city_stats_all_db.csv", row.names=F)

