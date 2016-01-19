#' Extract Coordinates
#'
#' Exports Point and Polygon coordinates from the geo fields of the tweet
#' The function currently supports coordinates of the type point and bounding box.
#' The coordinates can be shown on a map using \link{drawMap}
#'
#' @param connection The name of a MongoDB connection
#' @param searchterm A term all tweets returned contain
#' @param limit Maximum number of tweets to retrive from db
#'
#' @return dataframe with coordinates (lat,long)
#'
#' @examples
#' #extractCoordinates(connection = "filterstream", searchterm = c("Trump"))
#'
#' @export
extractCoordinates <-
  function(connection, searchterm, limit = 15000) {
    mconnection <- connect2MongoDb(connection)
    points <-
      mconnection$find(buildRegexQuery(searchterm), getMongoFields(
        c(
          "place.bounding_box.coordinates","geo","text", "user.location"
        )
      ),limit = limit)

    for (i in 1:length(points$text)) {
      line <- NULL
      #Check if any points geo included
      if (!is.atomic(points$geo)) {
        line <- (unlist(points$geo$coordinates[i]))
      }

      if (is.null(line)) {
        ul <- unlist(points$place$bounding_box$coordinates[i])
        line <- (ul[5] + ul[6] + ul[7] + ul[8]) / 4
        line <- append(line,(ul[1] + ul[2] + ul[3] + ul[4]) / 4)
      }
      points$lon[i] <- line[2]
      points$lat[i] <- line[1]
    }
    return(points)
  }

#' Geocorde DataFrame
#'
#' Converts a information from the user.location field into lat/lon coordinates
#' using the google maps API
#'
#' @param places a dataframe with at least one column containing the userLocation and google API
#' @param replace bolean value to define if existing lon/lat values should be replaced
#'
#' @return dataframe with coordinates (lat,long)
#'
#' @examples
#' #geocodeDataFrame(df)
#'
#' @import ggmap
#' @import maps
#'
#' @export
geocodeDataFrame <- function(places, replace = FALSE) {
  #Create columns if not existing
  if ((!"lon" %in% colnames(places)) &&
      (!"lon" %in% colnames(places))) {
    places$lon <- NA
    places$lat <- NA
  }

  if (file.exists("locationscache.Rda")) {
    load("locationscache.Rda")
  }
  else {
    location <- ggmap::geocode("London", source = "google")
    locdb <-
      as.data.frame(matrix(
        c("London",location[1],location[2]),nrow = 1,ncol = 3
      ))
    colnames(locdb) <- c("place","lon","lat")
  }
  savecount <- 0

  for (i in 1:nrow(places)) {
    #Check if empty
    if (is.na(places$user$location[i]) |
        (places$user$location[i] == "")) {
      #do nothing
    }
    else {
      #Check if value is already set
      if (is.na(places$lon[i]) && replace == FALSE) {
        #Check if already in cache
        values <-
          locdb[locdb$place == places$user$location[i],c("lon","lat")]
        if (length(values$lon) == 1) {
          places$lon[i] <- unlist(values$lon)
          places$lat[i] <- unlist(values$lat)
          print("Cached DS")
        }
        else {
          location <-
            geocode(toString(places$user$location[i]), source = "google")
          places$lon[i] <- location[1]
          places$lat[i] <- location[2]
          locdb <-
            rbind(locdb,data.frame(
              place = toString(places$user$location[i]),lon = location[1],lat = location[2]

            ))
          if (savecount == 100) {
            save(locdb,file = "locationscache.Rda")
            print("Updated cache")
            savecount = 0
          }
          savecount = savecount+1
        }
      }
    }
  }
  save(locdb,file = "locationscache.Rda")
  return(places)
}


#' Draw a Density Map
#'
#' Draws a Density Map of the coordinates
#'
#' @param data dataframe containing lat/lon coordinates
#' @param location Area to show on the map
#' @param zoom Zoom factor for the map
#' @param fill boolean value to adjust if a filling is used
#'
#' @return map
#'
#' @examples
#' #drawMap(df, "Europe", 4)
#'
#' @import ggplot2
#' @export

drawMap <- function(data, location, zoom, fill = FALSE) {
  map <- get_map(location = location, zoom = zoom)
  if (fill) {
    ggmap(map, extent = "device") + geom_density2d(data = data, aes(x = lon, y = lat), size = 0.3)  +
      stat_density_2d(
        data = data, aes(
          x = lon, y = lat, fill = ..level.., alpha = ..level..
        ), size = 0.01, geom = "polygon"
      )  +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = FALSE)
  }

  else {
    ggmap(map, extent = "device") + geom_density2d(data = data, aes(x = lon, y = lat), size = 0.3)

  }
}
