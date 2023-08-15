# resaving the all_cities.csv in a format that will go into the db a bit easier

city.dat <- read.csv("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/processed_cities/city_stats_all2.csv",header=T)

head(city.dat)


city.dat.db <- city.dat

# Replacing NA's with blank string
city.dat.db[is.na(city.dat.db)] <- ""


saveRDS(city.dat.db, "C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/processed_cities/city_stats_all_db.rds")
write.csv(city.dat.db, "C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/processed_cities/city_stats_all_db.csv", row.names=F)
