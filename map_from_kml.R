
# code largely from http://ralanbutler.com/blog/2016/01/25/nicaragua-making-map

library(sp)
library(maptools)
library(magrittr) # for pipe function
library(maps)
library(rgeos) # for gSimplify

Export the route we took from an existing Google Map to KML files.
Read the KML files into R, and convert them to SpatialLines.
Create the background map.
Transform the map and route to an appropriate projection.
Style the map.
Save as an SVG.

# kml files to read in, in the order they should be read in
  kmlFiles <- paste0(c('ALM','DrivingRoute2', 'Ferry', 
                       paste0('DrivingRoute',3:6)),'.kml')
  
  r1 <- do.call(rbind, lapply(kmlFiles, function(x) 
    as.data.frame(maptools::getKMLcoordinates(paste0('data/',x),TRUE)[[1]])))
  
  names(r1) <- c('long','lat')
  
  defProj <- sp::CRS('+init=epsg:4326') # default datum
  
  # the lambert conformal conic, set based on sites recomendations
  myProj <- sp::CRS('+proj=lcc +lat_1=32 +lat_2=44 +lat_0=38 +lon_0=-100 +x_0=False +y_0=False')

# create spatial lines from the kml data
  driveLine <- sp::Line(r1) %>% list() %>% sp::Lines(ID='drive-line') %>%
    list() %>% sp::SpatialLines(proj4string = defProj) %>% 
    sp::spTransform(myProj) %>%
    rgeos::gSimplify(tol = 500) # arbitrarily chosen tolerance

 # now add background map of states and countries we drove through
  ss <- maps::map('state', plot = F, fill = T)
  idS <- sapply(strsplit(ss$names, ':'), function(x) x[1])
  ssSt <- maptools::map2SpatialPolygons(ss, IDs=idS, proj4string=defProj) %>%
    sp::spTransform(myProj)
  
  mx <- maps::map('world',c('mexico','guatemala','nicaragua','el salvador', 'honduras', 'belize',
                      'costa rica','panama'),plot = F, fill = T, col = 'black')
  idMx <- sapply(strsplit(mx$names, ":"), function(x) x[1])
  ssMx <- maptools::map2SpatialPolygons(mx, IDs=idMx, proj4string=defProj) %>%
    sp::spTransform(myProj)
  
  bgMap <- rbind(ssSt, ssMx) # rbind combines polygons for spatialPolygons
  
  svg(filename = 'driveRoute.svg',width=8, height=8)
  par(mar = rep(0,4)) # remove margins
  plot(bgMap, col = 'grey15', border = 'grey50')
  plot(driveLine, col = 'steelblue3', add = T, lwd = 2.75) 
  dev.off()

  
  
  
  
  
  
