rm(list=ls())
setwd("~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags")

library(lubridate)
library(raster)
library(rgdal)
library(gganimate)
library(broom)
library(stats)
library(gifski)
library(tidyverse)

# Load data 
datafile1 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302202.csv"
datafile2 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302204.csv"
datafile3 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302983.csv"
datafile4 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302989.csv"
datafile5 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302991.csv"
datafile6 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302992.csv"
datafile7 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302996.csv"

rec.202 <- read.csv(datafile1, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.204 <- read.csv(datafile2, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.983 <- read.csv(datafile3, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.989 <- read.csv(datafile4, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.991 <- read.csv(datafile5, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.992 <- read.csv(datafile6, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.996 <- read.csv(datafile7, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)

pings <- bind_rows(rec.202, rec.204, rec.983, rec.989, rec.991, rec.992, rec.996) %>% mutate(as_datetime(DateandTime.UTC)) %>% select(-DateandTime.UTC.) %>% mutate(date.time.PST = with_tz(date.time, tzone = "America/Los_Angeles")) %>% filter(date.time.PST > "2021-07-27 11:00:00") %>% filter(date.time.PST < "2021-11-01 00:00:00") 
# used that cutoff b/c daylight savings messes up the conversion from UTC, can finesse if we want to keep more data

rm(list=ls()[! ls() %in% c("pings")])

pings_grouped <- pings %>% group_by(date.time.PST, Transmitter) %>% mutate(detection.num = cur_group_id()) %>% group_by(detection.num) %>% filter(n()>2) 

### 53710 
pings_53710 <- pings_grouped %>% filter(Transmitter == "A180-1702-53710") 
write.csv(pings_53710, "A180-1702-53710.csv") 
pings_grouped <- pings_53710 %>% select(c(date.time.PST, detection.num)) %>% distinct()

datafile8 <- "~/Academic Projects/Acoustic Tagging Project/July 2021 Trials - Barkley Sound/A180-1702-53710_centroids.csv"
centroids <- read.csv(datafile8, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE) %>% select(c(detection.num, cent.lat, cent.long))

#centroids <- pings_grouped %>% left_join(centroids, by = 'detection.num') %>% write.csv("modify_date.csv")
datafile9 <- "~/Academic Projects/Acoustic Tagging Project/July 2021 Trials - Barkley Sound/modify_date.csv"
centroids <- read.csv(datafile9, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE) %>% mutate(prev.detection.time = ymd_hm(prev.detection.time, tz = "America/Los_Angeles")) %>% mutate(detection.time = ymd_hm(detection.time, tz = "America/Los_Angeles")) %>% mutate(speed = prev.detection.time - detection.time)
centroids$speed <- as.numeric(centroids$speed, units="mins")

### 53711
pings_53711 <- pings_grouped %>% filter(Transmitter == "A180-1702-53711") 
#write.csv(pings_53711, "A180-1702-53711.csv") 
pings_grouped <- pings_53711 %>% select(c(date.time.PST, detection.num)) %>% distinct()

datafile9 <- "~/Academic Projects/Acoustic Tagging Project/July 2021 Trials - Barkley Sound/A180-1702-53711_centroids.csv"
centroids <- read.csv(datafile9, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE) %>% select(c(detection.num, cent.lat, cent.long))

#centroids <- pings_grouped %>% left_join(centroids, by = 'detection.num') %>% write.csv("modify_date.csv")
datafile9 <- "~/Academic Projects/Acoustic Tagging Project/July 2021 Trials - Barkley Sound/modify_date.csv"
centroids <- read.csv(datafile9, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE) %>% mutate(prev.detection.time = ymd_hm(prev.detection.time, tz = "America/Los_Angeles")) %>% mutate(detection.time = ymd_hm(detection.time, tz = "America/Los_Angeles")) %>% mutate(speed =  detection.time - prev.detection.time)
centroids$speed <- as.numeric(centroids$speed, units="mins")


#### MAP ####
BC.hidef <- readOGR("~/Career Documents/Mapping Files/Border Layers/DFO_BC_COASTLIN_AND_BC_BDY_MERGE", "DFO_BC_COASTLIN_AND_BC_BDY_MERGE")
BC.hidef <- spTransform(BC.hidef, CRS("+proj=longlat +datum=WGS84"))

#wcvi.crop <- extent(-130, -122, 48, 56)
#mayne.crop <- extent(-125.298, -125.285, 48.973, 48.98)
#wcvi.outline <- crop(BC.hidef, mayne.crop)
#writeOGR(wcvi.outline, "~/Career Documents/Mapping Files/Border Layers/DFO_BC_COASTLIN_AND_BC_BDY_MERGE", "maynebay_polygon2", driver="ESRI Shapefile")
wcvi.outline <- readOGR("~/Career Documents/Mapping Files/Border Layers/DFO_BC_COASTLIN_AND_BC_BDY_MERGE", "maynebay_polygon2")
wcvi.df <- tidy(wcvi.outline)

### ANIMATION 1 ####

ditch_the_axes <- theme(
  text = element_text(size=12),
  panel.border = element_rect(colour = "black", fill=NA, size=0.5),
  panel.grid = element_blank(),
  axis.title = element_blank()) 

p = ggplot()+
  
  # basemap
  geom_polygon(data = wcvi.df, aes(x=long, y = lat, group = group), color = "black", fill = "grey80", size = 0.20) + 
  coord_fixed(xlim = c(-125.2915, -125.2875),  ylim = c(48.974, 48.976), ratio = 1.3) + 
  
  # lines and points
  #geom_path(data = centroids, 
  #          aes(x=cent.long,y=cent.lat,color='lightgrey'), 
            #alpha = 0.2)+
  geom_point(data = centroids, 
             aes(x=cent.long, y=cent.lat,fill=speed),
             alpha = 0.9, shape=21, size = 3)+
  
  # formatting
  scale_fill_viridis_c(option = "inferno")+
  #scale_color_viridis_c(option = "inferno")+
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL) + 
  theme_bw() +
  theme(legend.position="none") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  ditch_the_axes +
  annotate("text", label = "#53710: male (62 mm)", x = -125.2883, y = 48.97596, size = 5, color = "black")
p

anim = p + 
  transition_reveal(along = detection.time)+
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 365, fps = 10, renderer = gifski_renderer())
anim_save("53710_MayneBay2021.gif")


### ANIMATION 2 ####

ditch_the_axes <- theme(
  text = element_text(size=12),
  panel.border = element_rect(colour = "black", fill=NA, size=0.5),
  panel.grid = element_blank(),
  axis.title = element_blank()) 

p = ggplot()+
  
  # basemap
  geom_polygon(data = wcvi.df, aes(x=long, y = lat, group = group), color = "black", fill = "grey80", size = 0.20) + 
  coord_fixed(xlim = c(-125.2915, -125.2875),  ylim = c(48.974, 48.976), ratio = 1.3) + 
  
  # lines and points
  geom_path(data = centroids, 
            aes(x=cent.long,y=cent.lat,color='lightgrey'), 
  alpha = 0.2)+
  geom_point(data = centroids, 
             aes(x=cent.long, y=cent.lat,fill=speed),
             alpha = 0.9, shape=21, size = 4)+
  
  # formatting
  #scale_fill_viridis_c(option = "inferno")+
  #scale_color_viridis_c(option = "inferno")+
  #scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL) + 
  theme_bw() +
  theme(legend.position="none") +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 12)) +
  ditch_the_axes +
  annotate("text", label = "#53711: male (61 mm)", x = -125.2883, y = 48.97596, size = 5, color = "black")
p

anim = p + 
  transition_reveal(along = detection.time)+
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

animate(anim, nframes = 400, fps = 10, renderer = gifski_renderer())
anim_save("53711_MayneBay2021.gif")


