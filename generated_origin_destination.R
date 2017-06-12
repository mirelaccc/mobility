
# set wd, load libs, read initial csv into dataframe
setwd("/x/y/z/")


# set wd, load libs, read initial csv into dataframe
setwd("/home/bigdata09/projs/mob/")
#install.packages(“ggplot2″)
#install.packages(“jsonlite”)
#install.packages(“plyr”)
#install_github("leonawicz/mapmate")
#install_github("wjrl/RBioFabric")

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

# > min(df01[,2])
# [1] 946
# > min(df01[,2])
# [1] 946

# normal_pop <- (df01$population_01042017-min(df01$population_01042017))/(max(df01$population_01042017)-min(df01$population_01042017))
# hst <- hist(df01$population_01042017, breaks = 120, xaxt="n")
# axis(side=1, at=axTicks(1), labels=formatC(axTicks(1), format="d", digits = 11, big.mark=','))

# rename variable-names for clarity
colnames(df01)[1] <- "municipality"
colnames(df01)[2] <- "population_01042017"
colnames(df01)[3] <- "longitude"
colnames(df01)[4] <- "latitude"


df01$population_01042017 <- as.integer(df01$population_01042017)

df01$pop_10e5_20e5 <- 10
for (i in 1:nrow(df01)){
  ifelse(
    ((df01$population_01042017[i] >= as.integer(100000)) && (df01$population_01042017 < as.integer(200000))) 
    , df01$pop_10e5_20e5[i] <- 1  
    , ifelse(
      ((df01$population_01042017[i] >= as.integer(200000)) || (df01$population_01042017 < as.integer(100000)))
      , df01$pop_10e5_20e5[i] <- 0
      , df01$pop_10e5_20e5[i] <- 1)
  )
}

df01$pop_over_20e5 <- 10
for (i in 1:nrow(df01)){
  ifelse(
    (df01$population_01042017[i] >= as.integer(200000))
    , df01$pop_over_20e5[i] <- 1
    , ifelse(
      (df01$population_01042017[i] < as.integer(200000))
      , df01$pop_over_20e5[i] <- 0
      , df01$pop_over_20e5[i] <- 1)
  )
} 
 
#dim(df01)
# [1] 388   6

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


# big cities
comb_pop_10e5_20e5 <- data.frame(combn(as.character(df01$op_10e5_20e5), 2, FUN = paste, collapse="_"))
comb_pop_over_20e5 <- data.frame(combn(as.character(df01$comb_pop_over_20e5), 2, FUN = paste, collapse="_"))
 

# ------------ new dataframe ------------

# into a new dataframe, should be 75078x4
df011 <- data.frame(compared_municipalities, distances)
df012 <- data.frame(df011, compared_populations)
df013 <- data.frame(df012, compared_locations)
df014 <- data.frame(df013, comb_pop_10e5_20e5)
df02 <- data.frame(df014, comb_pop_over_20e5)
 

# rename column names
colnames(df02)[1] <- "compared_municipalities"
colnames(df02)[2] <- "distances"
colnames(df02)[3] <- "compared_populations"
colnames(df02)[4] <- "compared_locations"
colnames(df02)[5] <- "comb_pop_10e5_20e5"
colnames(df02)[6] <- "comb_pop_over_20e5"


# make seperate lists with split-up's of compared variables
 
lcm <- data.frame(strsplit(as.character(df02$compared_municipalities), '_'))
lcm2 <- data.frame(t(lcm))

lcp <- data.frame(strsplit(as.character(df02$compared_populations), '_'))
lcp2 <- data.frame(t(lcp))

lcl <- data.frame(strsplit(as.character(df02$compared_locations), '_'))
lcl2 <- data.frame(t(lcl))

lcbc1020 <- data.frame(strsplit(as.character(df02$comb_pop_10e5_20e5), '_'))
lcbc10202 <- data.frame(t(lcbc1020))
  
lcbc20up <- data.frame(strsplit(as.character(df02$comb_pop_over_20e5), '_'))
lcbc20up2 <- data.frame(strsplit(t(lcbc20up))
  
  
df020 <- data.frame(df02, lcm2)
df021 <- data.frame(df020, lcp2)
df022 <- data.frame(df021, lcl2)
df023 <- data.frame(df022, lcbc10202)
df024 <- data.frame(df023, lcbc20up2)

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
colnames(df022)[11] <- "comb_pop_10e5_20e5"
colnames(df022)[12] <- "comb_pop_over_20e5"

df023 <- data.frame(lcl_lons2, lcl_lats2)
df03 <- data.frame(df024, df023)
  
colnames(df03)[13] <- "lon_i"
colnames(df03)[14] <- "lat_i"
colnames(df03)[15] <- "lon_j"
colnames(df03)[16] <- "lat_j"
 
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

write.csv(df03, 'df03_20170612.csv')

# ##
# df03_possible_duplicates <- duplicated(df03$distances)
# df03_uniqdists <- unique(df03$distances)
# 
# df03$possible_duplicate <- 2
# 
# # remove duplicate connections
# for (i in 1:nrow(df03)){
#   for (j in 1:800){
#     ifelse(
#       (df03$distances[i] == df03$distances[j]),
#         df03$possible_duplicate[i] <- 1,
#         df03$possible_duplicate[i] <- 0
#     )
#   }
# }
# 
# ##
# 
# df03_possible_duplicates <- duplicated(df03[,5:6])
# nrow(df03$possible_duplicate)
# 
# ##
# 
# 
# # make population weights; normalized population sizes
# dfo3$pop_i_norm <- (max(df03$pop_i)-min(df03$pop_i))
# df03$pop_j_norm <- (max(df03$pop_j)-min(df03$pop_j))

# for drawing directed edges later on
# decide DIRECTION based on the larger population out of any 2-combination

# which population 
df03$from_lon <- 10
for (i in 1:nrow(df03)){
  ifelse(
    (df03$pop_i[i] < df03$pop_j[i])
    , df03$from_lon[i] <- df03$lon_i[i]
      , ifelse(
        (df03$pop_i[i] > df03$pop_j[i])
        , df03$from_lon[i] <- df03$lon_j[i]
        , df03$from_lon[i] <- 0)
  )
}

df03$from_lat <- 10
for (i in 1:nrow(df03)){
  ifelse(
    (df03$pop_i[i] > df03$pop_j[i])
    , df03$from_lat[i] <- df03 $lat_i[i]
      , ifelse(
        (df03$pop_i[i] < df03$pop_j[i])
        , df03$from_lat[i] <- df03$lat_j[i]
        , df03$from_lat[i] <- 0)
  )
}

df03$to_lon <- 10
for (i in 1:nrow(df03)){
  ifelse(
    (df03$pop_i[i] < df03$pop_j[i])
    , df03$to_lon[i] <- df03$lon_i[i]
    , ifelse(
      (df03$pop_i[i] > df03$pop_j[i])
      , df03$to_lon[i] <- df03$lon_j[i]
      , df03$to_lon[i] <- 0)
  )
}

df03$to_lat <- 10
for (i in 1:nrow(df03)){
  ifelse(
    (df03$pop_i[i] < df03$pop_j[i])
    , df03$to_lat[i] <- df03 $lat_i[i]
    , ifelse(
      (df03$pop_i[i] > df03$pop_j[i])
      , df03$to_lat[i] <- df03$lat_j[i]
      , df03$to_lat[i] <- 0)
  )
}

# ROUND_DISTANCES will be handy for further processing
# it introcuses a (tolerable?) error of <50 cm. (w/in one distance-measurement!)
round_distances <- as.integer(round(df03$distances))
df03$rounded_distances <- round_distances

# the gravity is based on the 'grivity model' ((pop_i * pop_j) / distances)
df03$gravity <- 0
for (i in 1:nrow(df03)){
    df03$gravity[i] <- round(((as.numeric(df03$pop_i[i]) * as.numeric(df03$pop_j[i])) / as.numeric(df03$rounded_distances[i])))
}

# chance of actual movement, as being not too far
# draw edge at each distance over threshold; thresholds for now are 50000 and 100000 (m)
# also, actual trip-duraion should be taken into acount, regardles of (spherical) lon-lat distances
# note: 2 more gradients should be added for more realistic choices

df03$edge1 <- 2
for (i in 1:nrow(df03))
{
  ifelse(
    (as.integer(df03$rounded_distances[i]) >= as.integer(100000))
    , df03$edge1[i] <- 0
    , ifelse(
      (as.integer(df03$rounded_distances[i]) < as.integer(100000))
      , df03$edge1[i] <- 1
      , df03$edge1[i] <- 0)
  )
}

df03$edge2 <- 2
for (i in 1:nrow(df03))
{
  ifelse(
    (as.integer(df03$rounded_distances[i]) >= as.integer(50000))
    , df03$edge2[i] <- 0
    , ifelse(
      (as.integer(df03$rounded_distances[i]) < as.integer(50000))
      , df03$edge2[i] <- 1
      , df03$edge2[i] <- 0)
  )
}

df03$below_10e5 <- 2
for (i in 1:nrow(df03))
{
  ifelse(
    ((as.integer(df03$comb_pop_10e5_20e5[i]) == as.integer(0)) && ((as.integer(df03$comb_pop_over_20e5[i]) == as.integer(0)))
    , df03$edge2[i] <- 1
    , ifelse(
      (as.integer(df03$comb_pop_10e5_20e5[i]) == as.integer(1)) || ((as.integer(df03$comb_pop_over_20e5[i]) == as.integer(1)))
      , df03$edge2[i] <- 0
      , df03$edge2[i] <- 1)
  )
}

head(df03)
dim(df03)
# [1] 75078    22

# df_check1 <- subset(df03, edge1 == 2)
# df_check2 <- subset(df03, edge2 == 2)
# dim(df_check1)
# dim(df_check2)
#[1]  0 22

# subsetting only necessary values new dataframes for network & map plots 
# filter only actual movements

write.csv(df03, "df03_3.csv")


############################################################################
install.packages('igraph')
install.packages('network') 
install.packages('sna')
install.packages('ndtv')
install.packages('visNetwork')


library(RJSONIO)
library(ggmap)
library(geosphere)
library(data.table)
library(gdata)
library(varhandle)
library(mapmate)
library(dplyr)
library(igraph)
library(network)
library(sna)
library(ndtv)
library(visNetwork)
library(maps)
library(ggmap)
library(mapdata)
library(mapproj)
library(maptools)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(scales)
library(raster)
library(RBioFabric)
library(HiveR)

# setwd("/home/bigdata09/projs/mob/")
# df04 <- read.csv("df03_2.csv",header = TRUE)

df_map_plot <- data.frame(df04[,c(6,7,16,17,18,19,21,22,23,24,25)])
df_map_plot <- subset(df_map_plot, edge1 == 1)
#df_map_plot <- subset(df_map_plot[, c(1,2,3,4)])

df_map$gravity <- as.integer(df_map$gravity)
df_map$normalized_gravity <- (df_map$gravity-min(df_map$gravity))/(max(df_map$gravity)-min(df_map$gravity))
df_map_s2 <- subset(df_map, normalized_gravity > 0.01)
df_map <- df_map_plot 
df_map$gravity <- as.integer(df_map$gravity)
df_map$normalized_gravity <- (df_map$gravity-min(df_map$gravity))/(max(df_map$gravity)-min(df_map$gravity))
df_map_s2 <- subset(df_map, normalized_gravity > 0.02)

# df_map_s2 <- subset(df_map, normalized_gravity > 0.01)
# > dim(df_map_s2)
# [1] 1959    9

df_network_plot <- data.frame(df04[,c(6,7,21,23)])
df_network_plot <- subset(df_network_plot, edge2 == 1)
df_network_plot$gravity <- as.integer(df_network_plot$gravity)
df_network_plot$normalized_gravity <- (df_network_plot$gravity-min(df_network_plot$gravity))/(max(df_network_plot$gravity)-min(df_network_plot$gravity))
df_network_plot2<- subset(df_network_plot, normalized_gravity > 0.1)
df_network_plot3 <- df_network_plot2[,c(1,2,5)]

# h<-hist(df_map$normalized_gravity, breaks=200, col="red", xlab="norm_grav", 
#         main="hist_test") 
# d <- density(df_map$normalized_gravity)
# plot(d, main="Kernel Density of normalized_gravity)")
# polygon(d, col="red", border="blue")

pdf(file="ArialBlack.pdf")
plot(x=10:1, y=10:1, pch=19, cex=6, main="This is a plot", col="orange", family="Arial Black" )
dev.off()

colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode="all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net) 



 
nl <- getData('GADM', country='NLD', level=1)
df<-data.frame("from" = df_map_s2$muni_i, 
               "to"= df_map_s2$muni_j)
meta <- data.frame("name"=df_map_s2$muni_i, 
                   "lon"=df_map_s2$from_lon,  
                   "lat"=df_map_s2$from_lat)
g_m <- graph.data.frame(df, directed=T)
lo <- as.matrix(meta[,2:3])
plot(nl)
plot(g_m, layout=lo, add = TRUE, rescale = FALSE)

library(igraph)
#import the sample_adjmatrix file:
dat=read.csv(XXXX,header=TRUE,row.names=1,check.names=FALSE)
m=as.matrix(dat)
net=graph.adjacency(m,mode="directed",weighted=NULL,diag=FALSE)
# show the names of the vertices you just imported:
V(net)$name

#the result will be:
#[1] "23732" "23778" "23824" "23871" "58009" "58098" "58256"
#import the sample_attributes
a=read.csv(file.choose())
V(net)$Sex=as.character(a$Sex[match(V(net)$name,a$Bird.ID)]) # This code says to create a vertex attribute called "Sex" by extracting the value of the column "Sex" in the attributes file when the Bird ID number matches the vertex name.
V(net)$Sex # This will print the new vertex attribute, "Sex"
# and you will get:
#[1] "F" "M" "M" "F" "F" "M" "M"

V(net)$color=V(net)$Sex #assign the "Sex" attribute as the vertex color
V(net)$color=gsub("F","red",V(net)$color) #Females will be red
V(net)$color=gsub("M","blue",V(net)$color) #Males will be blue
plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold)

V(net)$size=degree(net)*5 #because 1 is a small size for a node, I'm just multiplying it by 5
plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold)

library(igraph)
#import the sample_adjmatrix file:
dat=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE)
m=as.matrix(dat)
net=graph.adjacency(m,mode="undirected",weighted=NULL,diag=FALSE)
#import the sample_attributes
a=read.csv(file.choose())
nodecolor=as.character(a$Sex[match(V(net)$name,a$Bird.ID)])
nodecolor=gsub("F","red",nodecolor)
nodecolor=gsub("M","blue",nodecolor)
nodesize=degree(net)*5
plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.color=nodecolor,vertex.size=nodesize)

png(filename = "net2.png", height = 800, width = 800)
plot(nl)
plot(g_m, layout=lo, add = TRUE, rescale = FALSE)
dev.off()

install.packages("maps")
install.packages("geosphere")
library(maps)
library(geosphere)
par(mfrow = c(2,2), mar=c(0,0,0,0))
# Plot a map of the united states:
map('WorldHires','Netherlands', col="grey20", fill=TRUE, bg="black", lwd=0.1)
# Add a point on the map for each airport:
points(x=airports$longitude, y=airports$latitude, pch=19,
       cex=airports$Visits/80, col="orange")
col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)
for(i in 1:nrow(flights)) {
  node1 <- airports[airports$ID == flights[i,]$Source,]
  node2 <- airports[airports$ID == flights[i,]$Target,]
  arc <- gcIntermediate( c(node1[1,]$longitude, node1[1,]$latitude),
                         52
                         c(node2[1,]$longitude, node2[1,]$latitude),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*flights[i,]$Freq / max(flights$Freq))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)

# biofabric plot

g <- graph.data.frame(df_network_plot2, directed=T)
height <- vcount(g)
width <- ecount(g)
aspect <- height / width;
plotWidth <- 600.0
plotHeight <- plotWidth * (aspect * 1.2)
png("biofabric1.png", width=plotWidth, height=plotHeight)
bioFabric(g)
dev.off()


# Hive plot

net1 <- list()
net1$nodes <- df_network_plot3$muni_i
net1$b <- df_network_plot3$muni_j
net1$c <- df_network_plot3$normalized_gravity

hive1 <- edge2HPD(edge_df = df_network_plot3)
hive1 <- edge2HPD(edge_df = dataSet.ext)




# fails! 
apply(df_network_plot, 2, min)
apply(df_network_plot, 2, max)

# > dim(df_network_plot)
# [1] 12521     3
# > dim(df_map_plot)
# [1] 35244     6


# > head(df_map_plot)
# from_lon from_lat   to_lon   to_lat
# X45689.0776298777 6.153565 53.21036 6.749529 53.01048
# X71813.6211306073 6.668492 52.36703 6.749529 53.01048
# X87164.1919990337 6.749529 53.01048 5.658766 53.44056
# X35226.8299801632 6.749529 53.01048 6.854422 53.32068
# X12595.6013129436 6.564228 52.99275 6.749529 53.01048
# X33918.3499777629 6.749529 53.01048 6.599829 53.30168
#
# > head(df_network_plot)
# muni_i        muni_j gravity
# X45689.0776298777 Aa en Hunze Achtkarspelen   15455
# X35226.8299801632 Aa en Hunze    Appingedam    8550
# X12595.6013129436 Aa en Hunze         Assen  135705
# X33918.3499777629 Aa en Hunze         Bedum    7809
# X30549.0101131924 Aa en Hunze  Bellingwedde    7386
# X29781.0735897562 Aa en Hunze      Ten Boer    6176


# plot from-to as directed, weighted NETWORK


colnames(df_network_plot)[1] <- 'from'
colnames(df_network_plot)[2] <- 'to'
colnames(df_network_plot)[4] <- 'weight'

net1 <- graph.data.frame(df_network_plot, directed=TRUE)

plot(net, vertex.shape="none", vertex.label=V(net)$media,
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.7, edge.color="gray85")


plot(g_network_plot, edge.width=E(g_network_plot)$weight)


# ANIMATE from-to flow

df_map_plot$group <-1
df_map_plot$id <- rep(1:35244)

df_map_plot <- df_map_plot[,c("id","from_lon","from_lat","to_lon","to_lat","group")]


endpoints <- df_map_plot[,c("from_lon","from_lat","to_lon","to_lat")] 
paths_flat <- gc_paths(arcs_flat, "group", size = 5)

# expand data frame from endpoints to arcs, each composed of a sequence of points

n <- max(paths_flat$id)
png.args <- list(width = 600, height = 300, bg = "black")
clrs <- c("#1E90FF50", "#FFFFFF50", "#FFFFFF", "#1E90FF75")
#ylm <- range(paths_flat$lat)  # trimming empty southern map region

save_seq(paths_flat, id = "id", n.frames = n, ortho = FALSE, type = "network",
         file = "network2D", png.args = png.args)

gglist <- save_seq(paths_globe, id = "id", n.frames = n, col = clrs, type = "network",
                   
                    
# rank by distane,
# dfX <- order(df03$rounded_distances))

# script adapted from https://github.com/asheshwor/mapping-location-history/blob/master/mapping_google_history.r


#Setting up directory and file list
dirName <- "//ASH-desktop/Users/asheshwor/Documents/R_git/history_KML/"
fileList <- c(dir(dirName))
numFiles <- length(fileList)
#Reading coordinates from KML file downloaded
#  from google location history
lat <- numeric(0)
lon <- numeric(0)
tStart <- Sys.time() #track time
# WARNING!! takes a while to loop through
#took ~10 min for 191,355 sets in 12 months of history (4 gb ram win 7 64 bit)
#took 1.4 hrs for the same on my old toshiba (3 gb ram win 7 64 bit)
for (i in 1:numFiles) {
dirTemp <- paste(dirName, fileList[i], sep="")
hist <- getKMLcoordinates(dirTemp) #read KML file
maxl <- length(hist)
for (j in 1:maxl) {
  hist.0 <- hist[[j]]
  lat <- c(lat, hist.0[2])
  lon <- c(lon, hist.0[1])
}
}
tFinish <- Sys.time()
difftime(tFinish, tStart) #calculating time it took to read
#convert to a dataframe
hist.df <- data.frame(lon, lat)
#Write extracted coordinates to a csv file
write.csv(hist.df, file="//ASH-desktop/Users/asheshwor/Documents/R_git/LocationHistory.csv")
#Reading coordinates
hist.df2 <- read.csv(file="//ASH-desktop/Users/asheshwor/Documents/R_git/LocationHistory.csv", header=TRUE)
hist.df2 <- hist.df2[,-1] #dropping first column

#rounding off data for density plots
hist.df3 <- hist.df2
hist.df3$lon <- round(hist.df3$lon, 4)
hist.df3$lat <- round(hist.df3$lat, 4)

#plotting the points
nl01 <- "nl_01" #create empty map
nl01_map <- qmap(nl01, zoom=14, maptype="roadmap", legend="topleft") #google roadmap
png("nl01_map.png",720,720)
nl01_map + geom_point(aes(x = lon, y = lat), 
                    data = hist.df2, 
                    color = couleur[8], alpha = 0.5)
dev.off()


# dfxx[grep("word", dfxx$yy, ignore.case=T),]
x.sub1 <- subset(x.df, y > 2 & V1 > 0.6)



# > head(df03)
#                   dist_names distances compared_populations pop_i pop_j
# 1       Aa en Hunze_Aalburg    181411          25294,13061 25294 13061
# 2      Aa en Hunze_Aalsmeer  158998.7          25294,31393 25294 31393
# 3        Aa en Hunze_Aalten  121147.8          25294,27134 25294 27134

x <- diff(df03$pop_i, df03$pop_j)
df04 <- data.frame(df03,data.frame)
df03 <- data.frame(pop1=unlist(s), AB=rep(v$AB, sapply(s, FUN=length)))
df02 <- cbind(dist_names,distances,compared_populations)
compared_populations <- combn(as.character(df01$population_01042017), 2, FUN = paste, collapse=",")
max_pop <- as.numeric(max(df01$population_01042017))
df02 <- cbind(dist_names,distances,compared_populations)



# na_municipalities <- df01[is.na(df01$longitude),]
#           municipality population_01042017 longitude latitude
# 113 Goeree-Overflakkee               48788        NA       NA
# 156     Hof van Twente               34972        NA       NA
# 199              Lopik               14290        NA       NA
# 369          De Wolden               23789        NA       NA

# dfx <- df01[is.na(df01$longitude),]







