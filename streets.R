require(rgdal)
library(rgeos)

map <- rgdal::readOGR("../STREETS.shp", layer="STREETS")

pc <- map[map$NAME == "PARK CENTRAL", ]
ch <- map[map$NAME == "CHURCHILL", ]
inter <- gIntersection(pc, ch)

plot(pc)
lines(ch, col=3)
points(inter, col=2)
