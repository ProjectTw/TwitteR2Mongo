#' Update Home Timeline
#'
#' Function to retrive and update the home timeline, the home timeline consist of up to 800 tweets
#' posted by the accounts the user follows. Set Sys.setlocale("LC_TIME", "English") in order
#' to be able to correctly process the twitter timestamp. See ?as.POSIXct for help.
#'
#' @param connection a mongoconnection to use (default: hometimeline)
#' @param oAuth an oAuth token to use, by default a token is retrived by using \link{makeOAuth}
#'
#' @return None
#'
#' @examples
#' #updateHometimeline()
#'
#' @export

updateHometimeline <-
  function(connection = "hometimeline", oAuth = makeOAuth()) {
    tryCatch({
      mconnection <- connect2MongoDb(connection)
    }, error = function(e)
    {
      stop(
        "Can not connect to MongoDB. The supplied connection does not work or the default Hometimeline does not exist.
        use setMongoDbCredentials()"
      )
    })
    connection = "hometimeline"
    mconnection <- connect2MongoDb(connection)
    oAuth = makeOAuth()
    fileid <- 0
    since_id <-
      mconnection$find('{}','{"id":1,"_id":0, "created_at":1}','{"id":-1}',limit =
                         1)
    if (length(since_id) == 0) {
      since <- 1
    }
    else {
      since <- since_id[1,2]
    }
    for (i in 1:5) {
      filename <- paste0("home", i, ".json")
      #filename <- getFilename("home",)
      if (i == 1) {
        requestURL <-
          paste0(
            "https://api.twitter.com/1.1/statuses/home_timeline.json?count=200&since_id=",since
          )
      }
      else {
        requestURL <-
          paste0(
            "https://api.twitter.com/1.1/statuses/home_timeline.json?count=200&max_id=",max_id, "&since_id=",since
          )
      }
      httr::GET(
        requestURL, httr::config(token = oAuth), httr::write_disk(filename, overwrite = TRUE)
      )
      json_data <- jsonlite::fromJSON(filename)


      Sys.setlocale("LC_TIME", "English")
      if (is.data.frame(json_data)) {
        max_id <- (min(json_data$id) - 65)
        json_data$timestamp_ms <-
          as.character(as.numeric(
            as.POSIXct(json_data$created_at, format = "%a %b %d %H:%M:%S %z %Y")
          ) * 1000)
        mconnection$insert(json_data)
      }
      else {

        message("Timeline is up to date")
        unlink(filename)
        break
      }
      unlink(filename)
    }
    }
