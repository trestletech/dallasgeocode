library(readr)
dir <- "/vagrant/download/2016/02-19/"
files <- list.files(dir)
data <- read_csv(paste0(dir, files[round(runif(1, min=1, max=length(files)))]))

library(dallasgeolocate)

add <- render_locations(data$Block, data$Street)
locs <- find_location(add)
hist(sapply(locs, "[[", "qual"), xlim=c(0,100))


