library(readr)
dir <- "/vagrant/download/2016/02-19/"
files <- list.files(dir)

bigData <- NULL
for (i in 1:10){
data <- read_csv(paste0(dir, files[round(runif(1, min=1, max=length(files)))]))
bigData <- rbind(bigData, data)
}

library(dallasgeolocate)

add <- render_locations(bigData$Block, bigData$Street)
add <- unique(add)
locs <- find_location(add)
hist(sapply(locs, "[[", "qual"), xlim=c(0,100))


