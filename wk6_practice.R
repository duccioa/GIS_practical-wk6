library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(spatstat)
library(GISTools)


add.alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
              rgb(x[1], x[2], x[3], alpha=alpha))  
}

blueplaques <- readOGR("./Blue_plaques", "london_blueplaques")
blueplaques <- spTransform(blueplaques, CRS("+init=epsg:27700"))
boroughs <- readOGR("./Boroughs", "england_lad_2011Polygon")
boroughs <- spTransform(boroughs, CRS("+init=epsg:27700"))

plot(boroughs)
points(blueplaques, pch = 20, lwd = 3, col = add.alpha("blue", 0.2))

blueplaques <- remove.duplicates(blueplaques)
#now just select the points inside London
bp_sub <- blueplaques[boroughs,]

plot(boroughs)
points(bp_sub, pch = 20, lwd = 3, col = add.alpha("blue", 0.2))
