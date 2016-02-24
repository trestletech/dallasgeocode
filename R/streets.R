#' Find an Address
#' @export
#' @examples 
#' find_address("5921 FOREST LN")
#' find_address("5921 FOREST") #Warns, multiple 
find_address <- function(streets){
  streets <- parse_address(streets)
  
  lapply(streets, function(street){
    # Address
    ch <- filter_map(street)
    # fromAddresses
    
    addr <- cbind(ch$SEGMENT_ID, ch$L_F_ADD, ch$R_F_ADD, ch$L_T_ADD, ch$R_T_ADD)
    colnames(addr) <- c("ID", "LeftFrom", "RightFrom", "LeftTo", "RightTo")
    # Replace 0 with NA
    addr[addr==0] <- NA
    if (nrow(addr) > 1){
      suppressWarnings(mins <- apply(addr[,-1], 1, min, na.rm=TRUE))
      suppressWarnings(maxs <- apply(addr[,-1], 1, max, na.rm=TRUE))
    } else if (nrow(addr) == 0){
      warning("Could not find street: ", street)
      return(c(x=NA, y=NA, qual=0))
    } else {
      # Only one segment
      mins <- min(addr[1, -1])
      maxs <- max(addr[1, -1])
    }
    addrRanges <- cbind(addr[,1], mins, maxs)
    colnames(addrRanges)[1] <- "ID"
    addrRanges <- addrRanges[!apply(addrRanges, 1, function(row) {any(is.infinite(row))}),]
    
    if (is.matrix(addrRanges)){
      seg <- addrRanges[
        apply(addrRanges, 1, function(row){is.null(street$num) || row["mins"] <= street$num && row["maxs"] >= street$num }),
      ]
    } else {
      # Only a single segment
      seg <- NULL
      if (!is.null(street$num)){
        if (addrRanges["mins"] <= street$num && addrRanges["maxs"] >= street$num){
          seg <- addrRanges
        }
      } else {
        # Include it
        seg <- addrRanges
      }
    }
    
    segment <- NULL
    if (is.matrix(seg)){
      # Multiple results
      segment <- map[map$SEGMENT_ID %in% seg[,"ID"],]
    } else {
      # One result
      segment <- map[map$SEGMENT_ID == seg["ID"],]
    }
    
    # Force nrow()
    if (!is.matrix(seg)){
      seg <- t(as.matrix(seg))
    }
    
    latlons <- NULL
    if (nrow(segment) == 0){
      warning("Invalid address -- no such number available: ", street)
      return(c(x=NA, y=NA, qual=0))
    }
    
    for (i in 1:nrow(segment)){
      se <- segment[i,]
      lo <- interpolate_address(se, seg[i,], street$num)
      latlons <- rbind(latlons, lo)
    }
    
    # lat/lon to feet
    # TODO: use sp's classes/logic to convert
    ft <- latlons * 10000/90 * 3280.4
    mins <- apply(ft, 2, min)
    maxs <- apply(ft, 2, max)
    spread <- sqrt((maxs[1]-mins[1])^2 + (maxs[2] - mins[2])^2)
    
    qual <- compute_quality(spread)
    if (qual == 0){
      # Refuse to even return a result
      warning("Quality too low for spread of ", spread, " on ", street)
      c(x=NA, y=NA, qual=qual)
    } else {
      res <- apply(latlons, 2, median)
      c(x=res[1], y=res[2], qual=qual)
    }
  })
}

#' Segment Map
#' @param segment The map segments (filter_map)
#' @param seg A table of segment IDs with the associated min and max address range.
interpolate_address <- function(segment, seg, streetNum){
  # Map points into familiar lat/lon
  segment <- spTransform(segment, CRS("+proj=longlat +datum=NAD27"))
  coords <- segment@lines[[1]]@Lines[[1]]@coords
  
  # How many numbers are covered in this range.
  range <- abs(as.numeric(seg["mins"] - seg["maxs"])) - 1
  
  if (is.null(streetNum)){
    # Pick the average address if no number is specified.
    streetNum <- round(seg["mins"] + range/2)
  }
 
  # Where does our given number fit into the range? Spread over number of coords rows.
  # TODO: rounding should encompass -.5 and nrow()+.5 to be unbiased...
  ind <- as.integer(round((streetNum - seg["mins"]) / range * (nrow(coords)-1))) + 1
  
  # Closest coordinate is...
  coords[ind,]
}

parse_address <- function(add){
  # TODO: singleton
  prefixes <- as.character(unique(map@data$PREFIX))
  suffixes <- as.character(unique(map@data$SUFFIX))
  directions <- c("NB", "SB", "EB", "WB") # highway directions
  types <- as.character(unique(map@data$TYPE))
  
  add <- trimws(toupper(add))
  allParts <- strsplit(add, "\\s+")
  
  lapply(allParts, function(parts){
    address <- list()
    if (grepl("^\\d+$", parts[1], perl=TRUE)){
      address$num <- as.numeric(parts[1])
      parts <- parts[-1]
    }
    
    if (length(parts) >= 1 && parts[1] %in% prefixes){
      address$prefix <- parts[1]
      parts <- parts[-1]
    }
    
    if (length(parts) >= 1 && parts[length(parts)] %in% directions){
      address$direction = parts[length(parts)]
      parts <- parts[-length(parts)]
    }
    
    # <SUFFIX> <TYPE>
    if(length(parts) >= 1 && parts[length(parts)] %in% types){
      address$type <- parts[length(parts)]
      parts <- parts[-length(parts)]
      
      if(length(parts) >= 1 && parts[length(parts)] %in% suffixes){
        address$suffix <- parts[length(parts)]
        parts <- parts[-length(parts)]
      }
    }
    
    # <TYPE> <SUFFIX>
    if(length(parts) >= 1 && parts[length(parts)] %in% suffixes){
      address$suffix <- parts[length(parts)]
      parts <- parts[-length(parts)]
      
      if(length(parts) >= 1 && parts[length(parts)] %in% types){
        address$type <- parts[length(parts)]
        parts <- parts[-length(parts)]
      }
    }
    
    if (length(parts) >= 1 && parts[length(parts)] == "SERV"){
      address$service <- TRUE
      parts <- parts[-length(parts)]
    }
    
    address$street <- paste(parts, collapse=" ")
    
    address
  })
}

parse_and_find_intersection <- function(inter){
  streets <- strsplit(inter, "/", fixed=TRUE)
  streets <- lapply(streets, function(sts){
    find_intersection(sts[1], sts[2])
  })
  streets
}

#' Render locations
#' @export
render_locations <- function(blocks, streets){
  st <- streets
  st[!is.na(blocks)] <- paste(blocks[!is.na(blocks)], st[!is.na(blocks)])
  st
}

#' Find a location
#' @export
find_location <- function(loc){
  inters <- grepl(" / ", loc, fixed=TRUE)
  loc[inters] <- parse_and_find_intersection(loc[inters])
  loc[!inters] <- find_address(loc[!inters])
  loc
}

filter_map <- function(street){
  library(sp)
  s <- map[map$NAME == street$street, ] #TODO: Fuzzy
  if (!is.null(street$prefix)){
    s <- s[s$PREFIX == street$prefix, ]
  }
  if (!is.null(street$suffix)){
    s <- s[s$SUFFIX == street$suffix, ]
  }
  if (!is.null(street$type)){
    s <- s[s$TYPE == street$type, ]
  }
  s
}


#' Find an Intersection
#' @export
#' @examples
#' find_intersection("PARK CENTRAL", "CHURCHILL")
find_intersection <- function(street1, street2, debug=FALSE){
  
  street1 <- parse_address(street1)[[1]]
  street2 <- parse_address(street2)[[1]]

  s1 <- filter_map(street1)
  s2 <- filter_map(street2)
  
  if (debug){
    plot(s1)
    lines(s2, col=3)
  }
  
  # Intersection
  inter <- rgeos::gIntersection(s1, s2)
  
  quality <- 100
  if (is.null(inter) || nrow(inter@coords) == 0){
    # No results
    warning("No intersections found: ", street1, street2)
    return(c(x=NA, y=NA, qual=0))
  }
  if (debug){
    points(s2, inter)
  }
  
  if (nrow(inter@coords) > 1){
    # Multiple intersections found.
    
    # How big is this bounding box?
    spread <- sqrt(sum(abs(apply(inter@bbox, 1, diff))^2))
    
    quality <- compute_quality(spread)
    
    if (spread < 1500){
      centroid <- apply(inter@coords, 2, mean)
      inter <- SpatialPoints(t(as.matrix(centroid)), inter@proj4string)
    } else {
      # Spread is too great. We're not sure which point to choose.
      warning("Multiple intersections found, but far away from each other", street1, street2)
      return(c(x=NA, y=NA, qual=0))
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


#' Compute quality
#' @param spread The spread of the hypot. of the bounding box in feet.
compute_quality <- function(spread){
  if (spread < 20){
    return(100)
  } else if (spread < 250){
    # Close enough that we can just return the centroid
    return(98)
  } else if (spread < 750){
    # We can return the centroid to get the general area,
    # but it's not too accurate.
    return(90)
  } else if (spread < 2000){
    return(60)
  } else{
    return(0)
  }
}