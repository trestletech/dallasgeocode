#' Find an Address
#' @export
#' @examples 
#' find_address("5921", "FOREST")
find_address <- function(num, street){ #TODO: zip
  # Address
  ch <- map[map$NAME == street, ]
  # fromAddresses
  addr <- cbind(ch$SEGMENT_ID, ch$L_F_ADD, ch$R_F_ADD, ch$L_T_ADD, ch$R_T_ADD)
  colnames(addr) <- c("ID", "LeftFrom", "RightFrom", "LeftTo", "RightTo")
  # Replace 0 with NA
  addr[addr==0] <- NA
  suppressWarnings(mins <- apply(addr[,-1], 1, min, na.rm=TRUE))
  suppressWarnings(maxs <- apply(addr[,-1], 1, max, na.rm=TRUE))
  addrRanges <- cbind(addr[,1], mins, maxs)
  colnames(addrRanges)[1] <- "ID"
  addrRanges <- addrRanges[!apply(addrRanges, 1, function(row) {any(is.infinite(row))}),]
  
  seg <- addrRanges[
    apply(addrRanges, 1, function(row){ row["mins"] <= num && row["maxs"] >= num }),
    ]
  
  segment <- map[map$SEGMENT_ID == seg["ID"],]
  # Map points into familiar lat/lon
  segment <- spTransform(segment, CRS("+proj=longlat +datum=NAD27"))
  coords <- segment@lines[[1]]@Lines[[1]]@coords
  
  # How many numbers are covered in this range.
  range <- abs(as.numeric(seg["mins"] - seg["maxs"])) - 1
  
  # Where does our given number fit into the range? Spread over number of coords rows.
  # TODO: rounding should encompass -.5 and nrow()+.5 to be unbiased...
  ind <- as.integer(round((num - seg["mins"]) / range * nrow(coords))) + 1 
  
  # Closest coordinate is...
  coords[ind,]
}

#' Find an Intersection
#' @export
#' @examples
#' find_intersection("PARK CENTRAL", "CHURCHILL")
find_intersection <- function(street1, street2){
  # Intersection
  s1 <- map[map$NAME == street1, ]
  s2 <- map[map$NAME == street2, ]
  inter <- rgeos::gIntersection(s1, s2)
  
  #plot(s1)
  #lines(s2, col=3)
  #points(inter, col=2)
  
  # Unique row names
  rownames(inter@coords) <- 1:nrow(inter@coords)
  
  latlon <- spTransform(inter, CRS("+proj=longlat +datum=NAD27"))
  
  latlon@coords
}
