
# set wd, load libs, read initial csv into dataframe
setwd("/home/bigdata09/projs/mob/")
#install.packages(“ggplot2″)
#install.packages(“jsonlite”)
#install.packages(“plyr”)
library(RJSONIO)
library(ggmap)
library(geosphere)
library(data.table)
library(gdata)
library(varhandle)
library(mapmate)
library(dplyr)

df00 <- read.csv("Bevolkingsontwikkeli_040617192441.csv",header = TRUE)
#dim(df00)
# [1] 388  17

# get only the location-name and population variables
df01 <- df00[1:388,c(1,17)]

# get lon & lat values from google maps api from each municipality based on location names
#geocodes <- geocode(as.character(df01$Regio.s))
#write.csv(geocodes,"geocodes.csv")
geo <- read.csv("geocodes.csv",header=TRUE)
df01 <- data.frame(df01[,1:2],geo)
df01 <- df01[,-c(3)]

# rename variable-names for clarity
colnames(df01)[1] <- "municipality"
colnames(df01)[2] <- "population_01042017"
colnames(df01)[3] <- "longitude"
colnames(df01)[4] <- "latitude"

#head(df01)
#     municipality population_01042017 longitude latitude
# 1   Aa en Hunze               25294  6.749528 53.01048
# 2       Aalburg               13061  5.057085 51.75129
# 3      Aalsmeer               31393  4.750244 52.26064
# 4        Aalten               27134  6.580678 51.92667

#dim(df01)
# [1] 388   4

# create a distance matrix from two lists (-1 on both first and last city-row)
l1 <- data.frame(longitude = df01[1:387,3],
                 latitude = df01[1:387,4])
l2 <- data.frame(longitude = df01[2:388,3],
                 latitude = df01[2:388,4])
mtx <- distm(l1[,c('longitude','latitude')], l2[,c('longitude','latitude')], fun=distVincentyEllipsoid)

#head(mtx)
#           [,1]      [,2]     [,3]      [,4]      [,5]      [,6]
# [1,] 181410.97 158998.75 121147.8  45689.08 189958.68 204965.68
# [2,]      0.00  60463.11 106807.8 178628.27  29788.73  46693.60
# [3,]  60463.11      0.00 130832.1 141961.74  43773.50  51019.39
# [4,] 106807.76 130832.08      0.0 145751.95 131649.72 150232.45

#write.csv(mtx,"matrix_0.csv")

#dim(mtx)
# [1] 387 387
# elements in mtx = 149769
# checked: 387*387 = 149769

# remove duplicates and 'eigen'-distances
mtx[lower.tri(mtx)] <- NA
#head(mtx)
#         [,1]      [,2]     [,3]      [,4]      [,5]      [,6]
# [1,] 181411 158998.75 121147.8  45689.08 189958.68 204965.68
# [2,]     NA  60463.11 106807.8 178628.27  29788.73  46693.60
# [3,]     NA        NA 130832.1 141961.74  43773.50  51019.39
# [4,]     NA        NA       NA 145751.95 131649.72 150232.45

#dim(mtx)
# [1] 387 387

# transpose as quick-fix for correct iteration direction in as.list
mx <- t(mtx)
dists <- as.list(mx)
dists2 <- dists[!is.na(dists)]
dists3 <- data.frame(dists2)
dists4 <- t(dists3)
distances <- data.frame(dists4)

#length(distances)
# [1] 75078
# checked: 149769 + 387 / 2 = 75078

# to get all 'pairwise' combinations first combine all variables, then hen split for further comparison
# dataframe should (row-wise) increase from 388 to 75078 (see above)
# get X-combinations per Y w/ 'combn' 
# split into seperate variables

# municipalities
compared_municipalities <- data.frame(combn(as.character(df01$municipality), 2, FUN = paste, collapse="_"))

# populations
compared_populations <- data.frame(combn(as.character(df01$population_01042017), 2, FUN = paste, collapse="_"))

# locations 
combined_lonlats <- paste(df01$longitude, df01$latitude, sep = ",")
compared_locations <- data.frame(combn(as.character(combined_lonlats), 2, FUN = paste, collapse="_"))

# ------------ new dataframe ------------

# into a new dataframe, should be 75078x4
df011 <- data.frame(compared_municipalities, distances)
df012 <- data.frame(df011, compared_populations)
df02 <- data.frame(df012, compared_locations)

# rename column names
colnames(df02)[1] <- "compared_municipalities"
colnames(df02)[2] <- "distances"
colnames(df02)[3] <- "compared_populations"
colnames(df02)[4] <- "compared_locations"

# make seperate lists with split-up's of compared variables
 
lcm <- data.frame(strsplit(as.character(df02$compared_municipalities), '_'))
lcm2 <- data.frame(t(lcm))

lcp <- data.frame(strsplit(as.character(df02$compared_populations), '_'))
lcp2 <- data.frame(t(lcp))

lcl <- data.frame(strsplit(as.character(df02$compared_locations), '_'))
lcl2 <- data.frame(t(lcl))

df020 <- data.frame(df02, lcm2)
df021 <- data.frame(df020, lcp2)
df022 <- data.frame(df021, lcl2)

colnames(df022)[1] <- "compared_municipalities"
colnames(df022)[2] <- "distances"
colnames(df022)[3] <- "compared_populations"
colnames(df022)[4] <- "compared_locations"
colnames(df022)[5] <- "muni_i"
colnames(df022)[6] <- "muni_j"
colnames(df022)[7] <- "pop_i"
colnames(df022)[8] <- "pop_j"
colnames(df022)[9] <- "lonlat_i"
colnames(df022)[10] <- "lonlat_j"

lcl_lons <- data.frame(strsplit(as.character(df022$lonlat_i), ','))
lcl_lons2 <- data.frame(t(lcl_lons), stringsAsFactors = TRUE)
lcl_lats <- data.frame(strsplit(as.character(df022$lonlat_j), ','))
lcl_lats2 <- data.frame(t(lcl_lats), stringsAsFactors = TRUE)

df023 <- data.frame(lcl_lons2, lcl_lats2)
df03 <- data.frame(df022, df023)
  
colnames(df03)[11] <- "lon_i"
colnames(df03)[12] <- "lat_i"
colnames(df03)[13] <- "lon_j"
colnames(df03)[14] <- "lat_j"
 
# something about the r inferno.
df03$muni_i <- as.character(df03$muni_i)
df03$muni_j <- as.character(df03$muni_j)

df03$pop_i <- as.character(df03$pop_i)
df03$pop_j <- as.character(df03$pop_j)
df03$pop_i <- as.numeric(df03$pop_i)
df03$pop_j <- as.numeric(df03$pop_j)
df03$pop_i <- as.integer(df03$pop_i)
df03$pop_j <- as.integer(df03$pop_j)

df03$lon_i <- unfactor(df03$lon_i)
df03$lat_i <- unfactor(df03$lat_i)
df03$lon_j <- unfactor(df03$lon_j)
df03$lat_j <- unfactor(df03$lat_j)

write.csv(df03, 'df03.csv')

##
df03_possible_duplicates <- duplicated(df03$distances)
df03_uniqdists <- unique(df03$distances)

df03$possible_duplicate <- 0

# remove duplicate connections
for (i in 1:10){
  for (j in 1:30){
    ifelse(
      (df03$distances[i] == df03$distances[j]),
        df03$possible_duplicate[i] <- 1,
        df03$possible_duplicate[i] <- 0
    )
  }
}

##

df03_possible_duplicates <- duplicated(df03[,5:6])
nrow(df03$possible_duplicate)

# make population weights; normalized population sizes
dfo3$pop_i_norm <- (max(df03$pop_i)-min(df03$pop_i))
df03$pop_j_norm <- (max(df03$pop_j)-min(df03$pop_j))



# for drawing directed edges later on
# decide DIRECTION based on the larger population out of any 2-combination
df03$weight_muni <- 0

for (i in 1:nrow(df03)){
  ifelse(
    (df03$pop_i[i] > df03$pop_j[i])
    , df03$weight_muni[i] <- df03$muni_i[i]
      , ifelse(
        (df03$pop_i[i] < df03$pop_j[i])
        , df03$weight_muni[i] <- df03$muni_j[i]
        , df03$weight_muni[i] <- 0)
  )
}

# which population 
df03$weight_lon <- 0
for (i in 1:nrow(df03)){
  ifelse(
    (df03$pop_i[i] > df03$pop_i[i])
    , df03$weight_lon[i] <- df03$lon_i[i]
      , ifelse(
        (df03$pop_i[i] < df03$pop_j[i])
        , df03$weight_lon[i] <- df03$lon_j[i]
        , df03$weight_lon[i] <- 0)
  )
}
 
df03$weight_lat <- 0
for (i in 1:nrow(df03)){
  ifelse(
    (df03$pop_i[i] > df03$pop_i[i])
    , df03$weight_lat[i] <- df03$lat_i[i]
      , ifelse(
        (df03$pop_i[i] < df03$pop_j[i])
        , df03$weight_lat[i] <- df03$lat_j[i]
        , df03$weight_lat[i] <- 0)
  )
}

# ROUND_DISTANCES will be handy for further processing
# it introcuses a (tolerable?) error of <50 cm. (w/in one distance-measurement!)
round_distances <- as.integer(round(df03$distances))
df03$round_distances <- round_distances

# the gravity is based on the 'grivity model' ((pop_i * pop_j) / distances)
df03$gravity2 <- 0

for (i in 1:nrow(df03)){
    df03$gravity2[i] <- round(((df03$pop_i[i] * df03$pop_j[i]) / df03$round_distances[i]))
}

# chance of actual movement, as being not FAR_APART
# note: 2 more gradients should be added for more realistic choices
# also, actual trip-duraion should be taken into acount, regardles of (spherical) lon-lat distances
df03$far_apart <- 0
  for (i in 1:nrow(df03)){
    ifelse(
      (df03$round_distances[i] < 100000),
      df03$far_apart <- 0,
      df03$far_apart <- 1
    )
  }

# draw EDGE if chance present
df03$edge <- 0
  for (i in 1:nrow(df03)){
    ifelse(
      (df$far_apart[i] == 0),
      df03$edge <- 1,
      df03$edge <- 0
      )
}

# animate from-to flow
# add paths as 


>           lon0       lat0       lon1      lat1 Pop_wts0 Pop_wts1     Dist

endpoints <- df03[,c("lon_i","lat_i","lon_j","lat_j")] 










inverse_distance_weighting <- function(x) 1 - x/max(x) 
endpoints <- gc_endpoints(df03, "lon_i", "lat_i") ##########

#

set.seed(192)
data(network)

distFun <- function(x) 1 - x/max(x)  # simple inverse distance weighting
endpoints <- gc_endpoints(network, "lon", "lat")

# take a weighted sample, e.g., favoring larger averaged populations and shorter distances
endpoints <- mutate(endpoints, Dist_wts = distFun(Dist))
endpoints <- sample_n(endpoints, 500, replace = TRUE, weight = (Pop_wts0 + Pop_wts1)/2 + Dist_wts)

# expand data frame from endpoints to arcs, each composed of a sequence of points
arcs_flat <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1", breakAtDateLine = TRUE)
arcs_globe <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1")

n <- max(paths_flat$id)
png.args <- list(width = 600, height = 300, bg = "black")
clrs <- c("#1E90FF50", "#FFFFFF50", "#FFFFFF", "#1E90FF75")
ylm <- range(paths_flat$lat)  # trimming empty southern map region

save_seq(paths_flat, id = "id", n.frames = n, ortho = FALSE, type = "network",
         file = "network2D", png.args = png.args)


gglist <- save_seq(paths_globe, id = "id", n.frames = n, col = clrs, type = "network",
                   
                   
                   
                   
                   
                   
# rank by distane,
> df03$round_distance[order(population$age),c(1,2)]

df031 <- order(df03$round_distances))
df032 <- df031[,c(X,Y,Z)]


#


