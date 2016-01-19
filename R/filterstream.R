#' Importing a filterstream file to mongoDB.
#'
#' Used to import fiterstream files to mongo
#' in a seperate R process. Can be used to import tweets
#' manually for times of MongoDB downtime. The file to import
#' must be located in the directory set in sys.setenv("wd" =)
#'
#' @param fileid Nnumber of the file to import
#' @param connection A mongo db connection
#' @param createLog If set to FALSE output is send to the
#' console instead of the logfile (logfile_import.txt)
#'
#' @return none
#'
#' @examples
#' #import2mongo(2)
#'
#' @export
import2mongo <-
  function(fileid, connection = "filterstream", createLog = TRUE) {
    if (createLog == TRUE) {
      logfile <- file("logfile_import.txt","at")
      sink(
        logfile, append = TRUE, type = c("message"), split = FALSE
      )
      message(toString(Sys.time()), " Starting import of file no. ", fileid)
    }
    mconnection <- connect2MongoDb(connection)
    setwd(Sys.getenv("wd"))
    filename <- paste0(connection, fileid, ".json")
    con_imp <- file(filename,"r")
    amount <- try(mconnection$import(con_imp, bson = FALSE))
    close(con_imp)
    unlink(filename)
    message(toString(Sys.time()), " Import completed! Processed ", amount, " tweets.")
    mconnection$remove('{"id_str":null}',multiple = TRUE)
  }


#' Start streaming
#'
#' Start the streaming process for the filterstream
#' in a seperate R process. The timeout also defines the maximum waiting time
#' between executing \link{stopStreaming} and the end of the streaming process
#' If no query_url is supplied then the topHashtags from the users hometimeline
#' are used.
#'
#' @param timeout period of time after which the stream ist stopped
#' and restarted with a updated list of search parameters
#' @param query_url set a specific URL to query the API
#'
#' @return list of hashtags
#'
#' @examples
#' #startStreaming(320)
#' #startStreaming(60,"https://stream.twitter.com/1.1/statuses/filter.json?track=Trump")
#'
#' @export
#'
startStreaming <- function(timeout = 60, query_url) {
  Sys.setenv(wd = getwd())
  clearLogfiles()
  # Check database and credentials to make sure streamingLoop has all required information.
  mconection <- connect2MongoDb("filterstream")
  mconection <- connect2MongoDb("hometimeline")
  tryCatch({
    makeOAuth()
  }, error = function(e) {
    stop("Failed to create oAuth token. Check API credentials")
  })

  if (missing(query_url)) {
    print("Now starting stream based on the hashtags of the supplied users network")
    command <-
      paste0(
        "Rscript -e library(TwitteR2Mongo);streamingLoop(",timeout,",", as.character(1),")"
      )
  }

  else {
    Sys.setenv(query_url = query_url)
    print("Now starting stream based on supplied query_url")

    command <-
      paste0(
        "Rscript -e library(TwitteR2Mongo);streamingLoop(",timeout,",", as.character(2),")"
      )
  }
  system(command = command, wait = FALSE)
}


#' Stop streaming
#'
#' Stop the streaming process. Streaming is stoped within the timeframe given
#' as timeout in \link{startStreaming}
#' Stop stream will not terminate the streaming process imediately but stop instead
#  of reloading if the timeout occurs.
#'
#' @return list of hashtags
#'
#' @examples
#' #stopStreaming()
#'
#' @export

stopStreaming <- function() {
  status <- file("stop_request.txt","w+b")
  writeLines("TRUE", status)
  close(status)
}

#' StreamingLoop
#'
#' StreamingLoop is usually started by \link{startStreaming} in a seperate instance of R.
#' If you start StreamingLoop manually, you will not be able to stop it using R. StreamingLoop
#' can be triggered to stop after timeout using \link{stopStreaming}.
#'
#' @param timeout Time until the streaming is stoped and restarted with updated parameters
#' @param type The type of stream which is started. (1=Based on hometimeline) (2=Based on query_url)
#' @param createLog If set to FALSE output is send to the console instead of the logfile
#' (logfile_filterstream.txt)
#'
#' @examples
#' #do not run
#' #streamingLoop()
#'
#' @export
streamingLoop <- function(timeout = 30, type, createLog = TRUE) {
  if (createLog == TRUE) {
    logfile <- file("logfile_filterstream.txt","at")
    sink(
      logfile, append = TRUE, type = c("message"), split = FALSE
    )
    message(toString(Sys.time()), " Streaming process started")
  }
  hometimeline <- connect2MongoDb("hometimeline")
  filterstream <- connect2MongoDb("filterstream")
  setwd(Sys.getenv("wd"))

  base_url <<-
    "https://stream.twitter.com/1.1/statuses/filter.json?"
  stop_filterstream <- FALSE
  #Write Status to file
  status <- file("stop_request.txt","w+b")
  writeLines("FALSE", status)
  close(status)
  fileid <<- 0
  while (stop_filterstream == FALSE) {
    kw_filename <- paste0("filterstream", fileid, ".json")
    if (type == 1) {
      testlist <- getTracklist(hometimeline, fileid)
      try(httr::GET(
        base_url, query = testlist,httr::config(token = makeOAuth(), timeout = timeout),
        httr::write_disk(kw_filename, overwrite = TRUE)
      ), silent = TRUE)
    }
    if (type == 2) {
      base_url <- Sys.getenv("query_url")
      message(paste(toString(Sys.time()), " STREAMLOOP Type 2: in progress"))
      try(httr::GET(
        base_url, httr::config(token = makeOAuth(), timeout = timeout),
        httr::write_disk(kw_filename, overwrite = TRUE)
      ), silent = TRUE)
    }
    command <-
      paste0("Rscript -e library(TwitteR2Mongo);import2mongo(",fileid,")")
    system(command = command, wait = FALSE)

    # Check if stop is requested
    status <- file("stop_request.txt","rb")
    stop_filterstream <- readLines(status, n = 1)
    close(status)
    fileid <- fileid + 1
  }
  message(toString(Sys.time()), " Streaming process will now terminate.")
  unlink("stop_request.txt")
}


# Create the tracklist
# Will NOT be exported. Used by: StreamingLoop

getTracklist <- function(connection, fileid) {
  updateHometimeline()
  merge <-
    mergeHashtagFrames(
      topHashtagsInInterval(connection, interval = getInterval(60 * 24), 10),
      topHashtagsInInterval(connection, interval = getInterval(), 10)
    )
  merge <- na.omit(merge)
  tracklist <- list()
  message(
    toString(Sys.time()), " STREAM: Tracklist generated writing to file no.", fileid, ", searching for: ", toString(merge[,1])
  )
  for (i in 1:nrow(merge)) {
    tracklist <- addToList(toString(merge[i,1]),tracklist)
  }
  #Collapse list to string
  testlist <- list(track = paste(tracklist, collapse = "%2C"))
  return(testlist)
}


# Add values to list in utf8-url encoding
# Will NOT be exported. Used by: StreamingLoop
addToList <- function(value, list) {
  list <- c(list, URLencode(value, reserved = TRUE))
  return (list)
}


# Helper function deletes logfiles in working directory
# Will NOT be exported. Used by: startStreaming
clearLogfiles <- function() {
  unlink("logfile_dates.txt")
  unlink("logfile_filterstream.txt")
  unlink("logfile_import.txt")
}
