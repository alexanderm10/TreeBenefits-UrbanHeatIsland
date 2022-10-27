# Checking to make sure we have all the met files we need
cover.base <- "/Volumes/Morton_SDM/ContinuousVegFields_MOD44Bv6/hdf_raw/"
ftree <- dir(cover.base, ".hdf")
ftree <- ftree[which(substr(ftree, nchar(ftree)-3, nchar(ftree))==".hdf")] # ignore anything that's not a .tif

# Using tree cover to set up our file index
ftree.split <- stringr::str_split(ftree, "[.]")
ftree.df <- data.frame(file=ftree, matrix(unlist(ftree.split), ncol=length(ftree.split[[1]]), byrow = T))
names(ftree.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
ftree.df$date <- strptime(as.numeric(substr(ftree.df$date.stamp, 2, 8)), "%Y%j")
ftree.df$year <- lubridate::year(ftree.df$date)
ftree.df$month <- lubridate::month(ftree.df$date)
ftree.df$yday <- lubridate::yday(ftree.df$date)
summary(ftree.df)

tree.check <- aggregate(ftree.df$year, by=list(ftree.df$tile), FUN=length)
summary(tree.check)

tree.check[tree.check$x<max(tree.check$x),] 
# ftree.df[ftree.df$tile=="h07v06","year"]
# Problematic tiles: 
# -- h07v06: missing 2011
# -- h11v12: missing 2013
# -- h12v05: missing 2011, 2012, 2014, 2015
# -- h18v09: missing 2011, 2012, 2014, 2015
# -- h33v09: missing 2013

tree.check[tree.check$x<max(tree.check$x),"Group.1"]


# --------------
# Processed 2011-2014
# --------------
path.met <- "/Volumes/Morton_SDM/SurfTemp_MODIS_MODA2/processed/2014/hdf_raw/"

fmet.all <- dir(path.met, ".hdf")
fmet.all <- fmet.all[which(substr(fmet.all, nchar(fmet.all)-3, nchar(fmet.all))==".hdf")] # ignore anything that's not a .tif

fmet.split <- stringr::str_split(fmet.all, "[.]")
fmet.df <- data.frame(file=fmet.all, matrix(unlist(fmet.split), ncol=length(fmet.split[[1]]), byrow = T))
names(fmet.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
fmet.df$date <- strptime(as.numeric(substr(fmet.df$date.stamp, 2, 8)), "%Y%j")
fmet.df$year <- lubridate::year(fmet.df$date)
fmet.df$month <- lubridate::month(fmet.df$date)
fmet.df$yday <- lubridate::yday(fmet.df$date)
summary(fmet.df)

# Check against trees
tile.check <- aggregate(fmet.df$year, by=list(fmet.df$tile), FUN=length)
tile.check[tile.check$x < max(tile.check$x) ,]
summary(tile.check)

tree.check[!tree.check$Group.1 %in% tile.check$Group.1,]
# 2011 met: h12v05; h18v09
# --------------

# --------------
# 2015
# --------------
path.met <- "/Volumes/Morton_SDM/SurfTemp_MODIS_MODA2/FILES_CONVERT/2015/01-02/"

fmet.all <- dir(path.met, ".hdf")
fmet.all <- fmet.all[which(substr(fmet.all, nchar(fmet.all)-3, nchar(fmet.all))==".hdf")] # ignore anything that's not a .tif

fmet.split <- stringr::str_split(fmet.all, "[.]")
fmet.df <- data.frame(file=fmet.all, matrix(unlist(fmet.split), ncol=length(fmet.split[[1]]), byrow = T))
names(fmet.df) <- c("file", "dataset", "date.stamp", "tile", "version", "process.stamp", "extension")
fmet.df$date <- strptime(as.numeric(substr(fmet.df$date.stamp, 2, 8)), "%Y%j")
fmet.df$year <- lubridate::year(fmet.df$date)
fmet.df$month <- lubridate::month(fmet.df$date)
fmet.df$yday <- lubridate::yday(fmet.df$date)
summary(fmet.df)

# Check against trees
tile.check <- aggregate(fmet.df$year, by=list(fmet.df$tile), FUN=length)
summary(tile.check)
tile.check[tile.check$x < max(tile.check$x) & tile.check$Group.1 %in% tree.check$Group.1,]

tree.check[!tree.check$Group.1 %in% tile.check$Group.1,]
# 2011 met: h12v05; h18v09
# --------------
