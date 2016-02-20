#' Find an Address
#' @export
#' @examples 
#' find_address(5921, "FOREST")
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

prefixes <- as.character(unique(map@data$PREFIX))
suffixes <- as.character(unique(map@data$SUFFIX))
types <- as.character(unique(map@data$TYPE))


parse_address <- function(add){
  add <- toupper(add)
  allParts <- strsplit(add, "\\s+")
  
  lapply(allParts, function(parts){
    address <- list()
    if (grepl("^\\d+$", parts[1], perl=TRUE)){
      address$num <- as.numeric(parts[1])
      parts <- parts[-1]
    }
    
    if (parts[1] %in% prefixes){
      address$prefix <- parts[1]
      parts <- parts[-1]
    }
    
    # <SUFFIX> <TYPE>
    if(parts[length(parts)] %in% types){
      address$type <- parts[length(parts)]
      parts <- parts[-length(parts)]
      
      if(parts[length(parts)] %in% suffixes){
        address$suffix <- parts[length(parts)]
        parts <- parts[-length(parts)]
      }
    }
    
    # <TYPE> <SUFFIX>
    if(parts[length(parts)] %in% suffixes){
      address$suffix <- parts[length(parts)]
      parts <- parts[-length(parts)]
      
      if(parts[length(parts)] %in% types){
        address$type <- parts[length(parts)]
        parts <- parts[-length(parts)]
      }
    }
    
    address$street <- paste(parts, collapse=" ")
    
    address
  })
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
  
  quality <- 100
  
  if (nrow(inter@coords) == 0){
    # No results
    stop("No intersections found")
  }
  
  if (nrow(inter@coords) > 1){
    # Multiple intersections found.
    
    # How big is this bounding box?
    spread <- sqrt(sum(abs(apply(inter@bbox, 1, diff))^2))
    
    if (spread < 1000){
      centroid <- apply(inter@coords, 2, mean)
      inter <- SpatialPoints(t(as.matrix(centroid)), inter@proj4string)
      
      if (spread < 250){
        # Close enough that we can just return the centroid
        quality <- 95
      } else {
        # Within a thousand feet. We can return the centroid to get the general area,
        # but it's not too accurate.
        quality <- 80
      }
    } else {
      # Spread is too great. We're not sure which point to choose.
      stop("Multiple intersections found, but far away from each other")
    }
  }
  
  #plot(s1)
  #lines(s2, col=3)
  #points(inter, col=2)
  
  # Unique row names
  rownames(inter@coords) <- 1:nrow(inter@coords)
  latlon <- spTransform(inter, CRS("+proj=longlat +datum=NAD27"))
  
  toReturn <- latlon@coords
  names(toReturn) <- colnames(latlon@coords)
  toReturn["qual"] <- quality
  toReturn
}
