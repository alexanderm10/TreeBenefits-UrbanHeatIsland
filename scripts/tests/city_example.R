# Making a quick map of tree cover in Chicago in 2013
library(ggplot2)
dat.chicago <- read.csv("../data_processed/cities_full_sdei_v5_old/Chicago_data_full.csv")
summary(dat.chicago)


ggplot(data=dat.chicago) +
  coord_equal() +
  ggtitle("Tree Cover in Chicago (2013)") +
  geom_raster(aes(x=x, y=y, fill=cover.tree)) +
  scale_fill_gradient(name="Tree Cover\n(%)", low="tan", high="darkgreen") +
  theme_bw() +
  theme(legend.position=c(0.75, 0.75),
        axis.title=element_blank())
