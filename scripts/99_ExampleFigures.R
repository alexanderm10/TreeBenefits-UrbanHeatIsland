# Example figures from Chicago
library(ggplot2)

Chicago = "USA26687"

path.cities <- "/Volumes/GoogleDrive/Shared drives/Urban Ecological Drought/Trees-UHI Manuscript/Analysis/data_processed_final"

dat.chi <- read.csv(file.path(path.cities, "data_cities_all", Chicago, paste0(Chicago, "_CityStats_Pixels.csv")))