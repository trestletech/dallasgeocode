library(readr)
data <- read_csv("/vagrant/download/2016/02-19/out-1455840262.csv")

library(dallasgeolocate)
map <- dallasgeolocate:::map

add <- render_locations(data$Block, data$Street)
locs <- find_location(add)



