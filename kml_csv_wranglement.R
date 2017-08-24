
setwd("/home/bigdata09/projs/mob/tt/csvs/")
library(stringr)

# read in files
df0 <- read.csv("kcp1.csv", sep = ',', header=FALSE)
df1 <- read.csv("kcp2.csv", sep = ',', header=FALSE)
# name columns
colnames(df0) <- c("A","B","C","D","E","F","G","H")
colnames(df1) <- c("A","B","C","D","E","F","G","H")


# only keep relevan columns
df00 <- df0[,-c(2,3,7,8)]
df11 <- df1[,-c(2,3,7,8)]
# make one dataframe
df2 <- rbind(df00, df11)
# rename columns
colnames(df2)[1] <- "city"
colnames(df2)[2] <- "contains_hits"
colnames(df2)[3] <- "level"
colnames(df2)[4] <- "lonlats"

# removing the ',' at the start
lonlats <- as.data.frame(df2$lonlats)
colnames(lonlats) <- "ll"

library(tidyverse)
# make smaller dataframes to enable pausing 
# 1
lonlats_s1 <- as.data.frame(lonlats[1:200000,])
colnames(lonlats_s1) <- "ll"
s1 <- read.csv(text = sub("^,", "", lonlats_s1$ll), header = FALSE)
lonlats_s2 <- as.data.frame(lonlats[200001:400000,])
colnames(lonlats_s2) <- "ll"
# 2
s2 <- read.csv(text = sub("^,", "", lonlats_s2$ll), header = FALSE)
lonlats_s3 <- as.data.frame(lonlats[400001:600000,])
colnames(lonlats_s3) <- "ll"
# 3
s3 <- read.csv(text = sub("^,", "", lonlats_s3$ll), header = FALSE)
lonlats_s4 <- as.data.frame(lonlats[600001:872640,])
colnames(lonlats_s4) <- "ll"
#4
s4 <- read.csv(text = sub("^,", "", lonlats_s4$ll), header = FALSE)
# merge into one, rename cols
lls <- do.call("rbind", list(s1, s2, s3, s4))
colnames(lls) <- c("lon_start","lat_start","lon_end","lat_end")

# remove old lonlat col, and have one new, correct dataframe
df2 <- df2[,-c(3)]
df3 <- data.frame(df2, lls)

df3_s <- df3[1:100,]
h <- df3_s$contains_hits
df3_s$hits <- gsub(".*hits:|v.*", "", h)
 

# separate the cogn and acc measurements
library(dplyr)
# trying different subsetting/filtering techni
df_acc <- df3 %>% filter(str_detect(city, "accDelay"))
df_con <- df3 %>% filter(str_detect(city, ".congLevel"))

df_acc2 <- df3[grep("accDelay", df3$city), ]
df_con2 <- df3[grep("congLevel", df3$city), ]

# df_acc$levelcolour <- as.character("a")
# r <- which[df_acc$level == "#ttStyle0"] 
# 
# df_acc[df_acc$levelcolour == "#ttStyle0", ][, ] <- "A"

df_con0 <- df_con[grep("#ttStyle0", df_con$level), ]
df_con1 <- df_con[grep("#ttStyle1", df_con$level), ]
df_con2 <- df_con[grep("#ttStyle2", df_con$level), ]
df_con3 <- df_con[grep("#ttStyle3", df_con$level), ]
df_con4 <- df_con[grep("#ttStyle4", df_con$level), ]
df_con5 <- df_con[grep("#ttStyle5", df_con$level), ]
df_con6 <- df_con[grep("#ttStyle6", df_con$level), ]

df_con0$levelcolour <- as.character("FFFF0000")
df_con1$levelcolour <- as.character("FFFF7F7F")
df_con2$levelcolour <- as.character("FFFFFFFF")
df_con3$levelcolour <- as.character("FF7FFFFF")
df_con4$levelcolour <- as.character("FF00FFFF")
df_con5$levelcolour <- as.character("FF007FFF")
df_con6$levelcolour <- as.character("FF0000FF")

df_acc0 <- df_acc[grep("#ttStyle0", df_acc$level), ]
df_acc1 <- df_acc[grep("#ttStyle1", df_acc$level), ]
df_acc2 <- df_acc[grep("#ttStyle2", df_acc$level), ]
df_acc3 <- df_acc[grep("#ttStyle3", df_acc$level), ]
df_acc4 <- df_acc[grep("#ttStyle4", df_acc$level), ]
df_acc5 <- df_acc[grep("#ttStyle5", df_acc$level), ]
df_acc6 <- df_acc[grep("#ttStyle6", df_acc$level), ]

df_acc0$levelcolour <- as.character("FFFF0000")
df_acc1$levelcolour <- as.character("FFFF7F7F")
df_acc2$levelcolour <- as.character("FFFFFFFF")
df_acc3$levelcolour <- as.character("FF7FFFFF")
df_acc4$levelcolour <- as.character("FF00FFFF")
df_acc5$levelcolour <- as.character("FF007FFF")
df_acc6$levelcolour <- as.character("FF0000FF")

df_accD <- do.call("rbind", list(df_acc0, df_acc1, df_acc2, df_acc3, df_acc4, df_acc5, df_acc6))
df_congL <- do.call("rbind", list(df_con0, df_con1, df_con2, df_con3, df_con4, df_con5, df_con6))

# new dataframes, only containing lon-lats segmets and according colours

df_accD <- df_accD[,-c(1,2)]
df_congL <- df_congL[,-c(1,2)]

# 
# write.csv(df_accD, "data_accDelay.csv")
# write.csv(df_congL, "data_congLevel.csv")

setwd("/home/bigdata09/projs/mob/tt/kmls/")

library(sp)
library(maptools)
library(magrittr) # for pipe function
library(maps)
library(rgeos)

# r1 <- as.data.frame((maptools::getKMLcoordinates('ALM_congLevel.kml'),TRUE)[[1]])
r1 <- as.data.frame(maptools::getKMLcoordinates('ALM_congLevel.kml'))
r1 <- do.call(rbind, lapply("ALM_congLevel.kml", function(x) as.data.frame(maptools::getKMLcoordinates(paste0('/home/bigdata09/projs/mob/tt/kmls/',x),TRUE)[[1]])))

names(r1) <- c('long','lat')
defProj <- sp::CRS('+init=epsg:4326') # default datum


# the lambert conformal conic, set based on sites recomendations
myProj <- sp::CRS('+proj=lcc +lat_1=32 +lat_2=44 +lat_0=38 +lon_0=-100 +x_0=False +y_0=False')

driveLine <- sp::Line(r1) %>% list() %>% sp::Lines(ID='drive-line') %>%
  list() %>% sp::SpatialLines(proj4string = defProj) %>% 
  sp::spTransform(myProj) %>%
  rgeos::gSimplify(tol = 500) # arbitrarily chosen tolerance


ss <- maps::map('state', plot = F, fill = T)
idS <- sapply(strsplit(ss$names, ':'), function(x) x[1])
ssSt <- maptools::map2SpatialPolygons(ss, IDs=idS, proj4string=defProj) %>%
  sp::spTransform(myProj)


library(maps)
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom") 
europeMap <- maps::map('world',europeanUnion,plot = T, fill = T, col = 'black') 


mx <- maps::map('world',c('mexico','guatemala','nicaragua','el salvador', 'honduras', 'belize',
                          'costa rica','panama'),plot = F, fill = T, col = 'black')
idMx <- sapply(strsplit(mx$names, ":"), function(x) x[1])
ssMx <- maptools::map2SpatialPolygons(mx, IDs=idMx, proj4string=defProj) %>%
  sp::spTransform(myProj)

bgMap <- rbind(ssSt, ssMx)

svg(filename = 'driveRoute.svg',width=8, height=8)
par(mar = rep(0,4)) # remove margins
plot(bgMap, col = 'grey15', border = 'grey50')
plot(driveLine, col = 'steelblue3', add = T, lwd = 2.75) 
dev.off()


 


gps <- move(x=temp$GPS_x, y=temp$GPS_y,time=as.POSIXct(temp$GPS_timeDate,        
                                                       format="%d/%m/%Y %H:%M:%S", tz="UTC"), 
            proj=CRS("+proj=longlat +ellps=WGS84"), animal='unknown', sensor='NoName')

gps_df <- as(gps, "data.frame")

m <- get_map(bbox(extent(gps)*1.1), source="osm", zoom=14)

ggmap(m)
+geom_path(data=gps_df[1:nrow(soc_long),], 
           aes(x=temp$GPS_x[1:nrow(soc_long)],   
               y=temp$GPS_y[1:nrow(soc_long)],
               colour=soc_long$dsoc),size=1.5,lineend="round"))



m <- get_map(location=c(lon=median(df_congL$longitude), lat=median(test$latitude)), zoom=8)
ggmap(m) + geom_point(aes(x=longitude, y=latitude, color=mode), data=test) + 
  geom_line(aes(x=longitude, y=latitude, color=mode), data=test)


m <- ggmap(get_map(unlist(geocode("Netherlands")),zoom=11))+
  geom_path(data=df_con,size=1,
            aes(x=long,y=lat,group=group,color=factor(values)))+
  labs(x="",y="")+
  theme(axis.text=element_blank(),axis.ticks=element_blank())

