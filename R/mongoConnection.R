#' Set MongoDB Credentials
#'
#' Function to set MongoDB credentials. Type can be hometimeline or filterstream.
#' Creates either hometimeline or filterstream as a connection element.
#'
#' @param type Type to set the MongoDB credentials for: hometimeline or filterstream
#' @param collection The collection to store the tweets in
#' @param db The database to store the tweets in
#' @param url The path to the MongoDB
#'
#' @return None
#'
#' @examples
#' setMongoDbCredentials (type = "hometimeline", collection = "hometimeline",
#' db = "twitter", url = "mongodb://localhost")
#'
#' @import mongolite
#' @export

setMongoDbCredentials <- function(type, collection, db, url) {
  if ((!class(type) == "character") ||
      (!class(collection) == "character") ||
      (!class(db) == "character") || (!class(url) == "character")) {
    stop("All parameters must be characters")
  }
  if (type == "hometimeline") {
    Sys.setenv(
      collection_home = "hometimeline", db_home = "twitter",
      url_home = "mongodb://localhost"
    )
  }
  else if (type == "filterstream") {
    Sys.setenv(
      collection_filter = "filterstream", db_filter = "twitter",
      url_filter = "mongodb://localhost"
    )
  }
  else {
    message("Type must be filterstream or hometimeline")
  }
}

#' Establish a connection to mongoDB
#'
#' Connect to MongoDB using the credentials stored with \code{\link{setMongoDbCredentials}}
#' Creates either hometimeline or filterstream as a connection element.
#'
#' @param type Type to set the MongoDB credentials for: hometimeline or filterstream
#'
#' @return Connection Object
#'
#' @examples
#' setMongoDbCredentials (type = "hometimeline", collection = "hometimeline",
#' db = "twitter", url = "mongodb://localhost")
#'
#' @export
#'
connect2MongoDb <- function(type) {
  if (!class(type) == "character") {
    stop("Type parameter must be a character")
  }
  if (type == "hometimeline") {
    tryCatch({
      return(
        mongo(
          collection = Sys.getenv("collection_home"),
          db = Sys.getenv("db_home"),
          url = Sys.getenv("url_home"),
          verbose = TRUE
        )
      )
    }, error = function(e)
    {
      stop(
        "Can not connect to MongoDB, wrong credentials or database not available.
        Please use setMongoDbCredentials."
      )
    })
    }
  else if (type == "filterstream") {
    tryCatch({
      return(
        mongo(
          collection = Sys.getenv("collection_filter"),
          db = Sys.getenv("db_filter"),
          url = Sys.getenv("url_filter"),
          verbose = TRUE
        )
      )
    }, error = function(e)
    {
      stop(
        "Can not connect to MongoDB, wrong credentials or database not available.
        Please use setMongoDbCredentials."
      )
    })
    }
  else {
    stop("Type must be filterstream or hometimeline")
  }
  }
