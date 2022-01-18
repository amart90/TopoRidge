library(tidyverse)
library(ggthemes)
library(ggridges)

setwd("D:/antho/Google Drive/UI-Drive/WallArt/JoyID/ID")
dem.ras. <- raster("DEM_ID_300.tif")
#dem.ras. <- raster("DEM_ID_3000.tif")
dem.ras <- dem.ras.
dem.ras[] <- NA
dem.ras. <- focal(dem.ras., w = matrix(1, 81, 31), fun = mean, na.rm = T)
dem.ras[seq(1, nrow(dem.ras), by = 40), ] <- dem.ras.[seq(1, nrow(dem.ras), by = 40), ]
dem.grp <- clump(dem.ras)

dem <- as.data.frame(rasterToPoints(dem.ras, spatial = F))
colnames(dem) <- c("lng","lat",  "elev")
grp <- as.data.frame(rasterToPoints(dem.grp, spatial = F))
colnames(grp) <- c("lng","lat",  "group")
dem <- merge.data.frame(dem, grp, by = c("lat", "lng"))
dem <- complete(dem, lat, lng)

normalize <-  function(x, y, ...){y * (x - min(x, ...)) / (max(x, ...) - min(x, ...))}
dem.norm <- normalize(dem[,1:2], 100, na.rm = T)
dem.norm$group <- dem$group
dem.norm$elev <- normalize(dem$elev, 2, na.rm = T)^2

ggplot(dem.norm, aes(x = lng, y = lat)) +
  geom_ridgeline(size=0.8, alpha=1, color='white', fill = "black", aes(group=group, height = elev), na.rm=TRUE)+
  ggthemes::theme_map() +
  theme(panel.background = element_rect(fill = "black"))+
  coord_equal()

ggsave("ID.png", width = 12, height = 12, dpi = 600, bg = "black")

