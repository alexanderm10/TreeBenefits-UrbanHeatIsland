cityAll.stats <- read.csv(file.path(path.cities, "..", "city_stats_all.csv"))
summary(cityAll.stats[!is.na(cityAll.stats$model.R2adj),])

cityAll.stats$biome <- gsub("flodded", "flooded", cityAll.stats$biome) # Whoops, had a typo!  Not going to reprocess now.
summary(as.factor(cityAll.stats$biome))

CityBuffStats <- read.csv(file.path(path.cities, "city_stats_core-buffer.csv"))
CityBuffStats$factor <- factor(CityBuffStats$factor, levels=c("LST", "tree", "other veg"))
CityBuffStats <- merge(CityBuffStats, cityAll.stats[,c("ISOURBID", "NAME", "LONGITUDE", "LATITUDE", "biomeName", "ES00POP")], all.x=T)
summary(CityBuffStats)

cityTree <- CityBuffStats[CityBuffStats$factor=="tree", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityTree) <- gsub("mean", "tree", names(cityTree))
names(cityTree)[which(names(cityTree)=="trend.p.core")] <- "trend.tree.p.core"

cityOther <- CityBuffStats[CityBuffStats$factor=="other veg", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityOther) <- gsub("mean", "other", names(cityOther))
names(cityOther)[which(names(cityOther)=="trend.p.core")] <- "trend.other.p.core"

cityVeg <- merge(cityTree, cityOther, all=T)
summary(cityVeg)

cityLST <- CityBuffStats[CityBuffStats$factor=="LST", c("ISOURBID", "value.mean.core", "value.mean.diff", "value.mean.diff.p", "trend.mean.core", "trend.p.core", "trend.mean.diff", "trend.mean.diff.p")]
names(cityLST) <- gsub("mean", "LST", names(cityLST))
names(cityLST)[which(names(cityLST)=="trend.p.core")] <- "trend.LST.p.core"

# summary(CityBuffStats[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biomeName", "model.R2adj", "model.tree.slope", "model.tree.p", "model.veg.slope", "model.veg.p")])

StatsCombined <- merge(cityAll.stats[,c("ISOURBID", "ISO3", "NAME", "LATITUDE", "LONGITUDE", "biome", "biome.prop", "model.R2adj", "model.tree.slope", "model.tree.p", "model.veg.slope", "model.veg.p")],
                       cityVeg, all=T)
StatsCombined <- merge(StatsCombined, cityLST, all=T)
summary(StatsCombined)


StatsCombined$TreeCooling <- StatsCombined$value.tree.core*StatsCombined$model.tree.slope
StatsCombined$TreeCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$model.tree.slope
StatsCombined$OtherCoverUHINeed <- -StatsCombined$value.LST.diff/StatsCombined$model.veg.slope
StatsCombined$TreeCoverTargetUHI <- StatsCombined$TreeCoverUHINeed + StatsCombined$value.tree.core
StatsCombined$OtherCoverTargetUHI <- StatsCombined$OtherCoverUHINeed + StatsCombined$value.other.core



StatsCombined[StatsCombined$NAME=="Chicago" & !is.na(StatsCombined$NAME),]

