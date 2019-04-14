library(raster)

path.trees <- "/Volumes/Morton_SDM/TreeCover_MOD44Bv6/2013/HEGOUT/"
ftree <- dir(path.trees, ".tif")
ftree <- ftree[which(substr(ftree, nchar(ftree)-3, nchar(ftree))==".tif")] # ignore anything that's not a .tif

# Creating a metadata sheet to keep track of bounding boxes
tree.meta <- data.frame(file=ftree, xmin=NA, xmax=NA, ymin=NA, ymax=NA)
for(i in 1:length(ftree)){
  tmp <- raster(file.path(path.trees, ftree[i]))
  
  tree.meta[i,c("xmin", "xmax", "ymin", "ymax")] <- extent(tmp)
}
summary(tree.meta)
dim(tree.meta)

bands.narrow <- which(tree.meta$ymin<=40 & tree.meta$ymax>=43 & tree.meta$xmin<=-91 & tree.meta$xmax>=-86)
summary(tree.meta[bands.narrow,])
tree.meta[bands.narrow,]


tree.rast <- raster(ext=extent(c(min(tree.meta$xmin[bands.narrow]), max(tree.meta$xmax[bands.narrow]),
                                 min(tree.meta$ymin[bands.narrow]), max(tree.meta$ymax[bands.narrow]))),
                    resolution=res(tmp))
tree.list <- list()
par(mfrow=c(length(bands.narrow)/2,length(bands.narrow)/2))
for(i in 1:length(bands.narrow)){
  # tree.list[[i]] 
  tmp <- raster(file.path(path.trees, ftree[bands.narrow[i]]))
  # tmp
  tmp[tmp>100] <- NA
  tmp[tmp==0] <- NA
  plot(tmp)
  
  tree.list[[i]] <- tmp
  
  # if(max(getValues(tree.rast), na.rm=T)==-Inf){
    # tree.rast <- tmp
  # } else {
    tree.rast <- mosaic(tree.rast, tmp, fun=mean, na.rm=T, tolerance=0.2)
  # }
}
par(mfrow=c(1,1))
# tree.rast2 <- mosaic(tree.list[[2]], tree.list[[4]], fun=mean, na.rm=T, tolerance=0.2)
# tree.rast3 <- mosaic(tree.list[[3]], tree.list[[1]], fun=mean, na.rm=T, tolerance=0.2)
# tree.rast4 <- mosaic(tree.rast3, tree.list[[2]], fun=mean, na.rm=T, tolerance=0.2)
# tree.rast5 <- mosaic(tree.rast4, tree.list[[4]], fun=mean, na.rm=T, tolerance=0.2)
# plot(tree.rast)


tree.chicago <- crop(tree.rast, c(-88.5,-87, 41, 42.5))
plot(tree.chicago)

tree.chicago2 <- tree.chicago
tree.chicago2[tree.chicago>100] <- NA
plot(tree.chicago2)
tree.chicago2