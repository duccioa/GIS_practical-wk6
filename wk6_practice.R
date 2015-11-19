library(maptools)
library(rgdal)
library(rgeos)
library(ggmap)
library(sp)
library(spatstat)#spatial analysis tools
library(GISTools)

#add alpha to colours
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

Harrow <- boroughs[boroughs@data$name == "Harrow",]
bp_harrow <- bp_sub[Harrow,]
plot(Harrow)
points(bp_harrow, pch = 20, lwd = 3, col = add.alpha("blue", 0.5))
#Spatial analysis with spatstat
#Needs its own objects called ppp
window <- as.owin(Harrow)
bp.ppp <- ppp(x = bp_harrow@coords[,1], y = bp_harrow@coords[,2], window = window)
plot(bp.ppp, pch = 20, cex = 2, col = add.alpha("blue", 0.4), main="Blue Plaques Harrow")

#plot Kernel Density Estimation
plot(density(bp.ppp, sigma = 500), main = "Kernel Density Estimation")
#QUADRAT ANALYSIS
plot(bp.ppp, pch = 20, cex = 1,  main="Blue Plaques Harrow")
plot(quadratcount(bp.ppp, nx =6, ny = 6), col = "red", add = T)
