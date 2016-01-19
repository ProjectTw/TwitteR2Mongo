#' createTwitterTimeline
#'
#' Creates a time series of hashtags found in the given time interval.
#' Is being used by createTwitterTimeSeries(). Parameter interval may consist
#' of an interval vector created by createIntervalVector(). See examples.
#'
#' @param mongo a mongoDB connection to use
#' @param interval which will be used to aggregate the hashtags in the mongodb.
#'
#' @return dataframe containing all hashtags with count of the respective time interval
#'
#' @examples
#' #createTwitterTimeline(hometimeline,
#' #                        createIntervalVector(c("2015-12-15 00:00:01","2015-12-17 14:59:59"),3))
#' @import stringr
#' @export
createTwitterTimeline <- function(mongo, interval) {
  # basequery
  interval <- as.character(interval)
  basequery <- '[{"$match": {"timestamp_ms": {"$gt": "intervalstart", "$lte": "intervalend"}}},
  {"$unwind": "$entities.hashtags"},
  {"$project": {"entities.hashtags": {"$toLower": "$entities.hashtags.text"}}},
  {"$group": {"_id":"$entities.hashtags","count":{"$sum":1}}},
  {"$sort": {"count":-1}}]'

  # insert respective interval into basequery
  for (i in 1:(length(interval) - 1)) {
    query <- stringr::str_replace_all(stringr::str_c(basequery),
                             c("intervalstart" = interval[i],
                               "intervalend" = interval[i + 1]))

    if (i == 1) {
      # dataframe of the first interval, to which everything gets appended to
      toAppendTo <- mongo$aggregate(query)
      if ((length(toAppendTo) == 0) | (nrow(toAppendTo) < 2)) {
        stop(
          "First interval does not contain enough or any
          documents in the mongoDB. Decrease interval steps
          or increase interval length"
        )
      }
    }
    else {
      # all further dataframes with more recent interval times.
      # they will be appended to the toAppendTo dataframe
      toAppendFrom <- mongo$aggregate(query)
      if (nrow(toAppendFrom) > 2) {
        toAppendTo <- appendDeltas(toAppendTo, toAppendFrom)
      }
      else {
        stop(
          paste0(
            "Not enough tweets in interval Nr.", i," . Increase
            decrease steps or increase interval size"
          )
        )
      }
    }
  }
  interval <- timestampToUserdate(as.numeric(interval))
  names(toAppendTo)[2:length(toAppendTo)] <-
    paste0(interval[1:(length(interval) - 1)]," to ",interval[2:length(interval)])
  return(toAppendTo)
}

#' createIntervalVector
#'
#' Creates a time interval vector. Interval parameters passed may be an unixtimestamp
#' (either in seconds or miliseconds), an interval of the twitter created_at format
#' or an interval of the form YYYY-MM-DD HH:MM:SS.
#'
#' @param interval which will be divided into equally sized subintervals.
#'        Interval must be of the form \%d.\%m.\%Y \%HH:\%MM:\%SS, for example: 11.09.2015 00:00:01
#' @param intervalsteps specifiy the amount of subintervals
#'
#' @return interval vector of unixtimestamps in ms
#'
#' @examples
#' #createIntervalVector(c("2015-12-15 00:00:01","2015-12-17 14:59:59"),3)
#' @export
createIntervalVector <- function(interval, intervalsteps) {
  if (is.numeric(interval) == TRUE) {
    fullinterval <-
      seq(
        from = interval[1],to = interval[2], by = diff(interval) / intervalsteps
      )

    if (all(nchar(interval, type = "chars") == 10)) {
      fullinterval <- fullinterval * 1000

    }
  }

  else if (is.character(interval) == TRUE) {
    if (all(grepl(pattern = "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}" ,x = interval)) ==
        TRUE) {
      interval <- as.numeric(as.POSIXct(interval, format = "%Y-%m-%d %T"))
    }

    else if (all(
      grepl(pattern = "^\\w{3} \\w{3} \\d{2} \\d{2}:\\d{2}:\\d{2} \\+\\d{4} \\d{4}", x = interval)
    ) == TRUE) {
      interval <-
        as.numeric(as.POSIXct(interval, format = "%a %b %d %H:%M:%S %z %Y"))
    }

    fullinterval <-
      seq(
        from = interval[1],to = interval[2], by = diff(as.numeric(interval)) / intervalsteps
      )
    fullinterval <- fullinterval * 1000
  }

  return(fullinterval)
}


#' cleanTwitterTimeline
#'
#' Cleans the dataframe returned by createTwitterTimeline. Inserts 0 for NaN and
#' removes hashtags which lie unter a certain threshold with their total amount
#' over all considered intervals.Threshold may be userspecified, if not will
#' default to an implemented threshold. See defaultThreshold().
#'
#' @param data to clean created by createTwitterTimeline()
#' @param threshold (optional) under which values will be removed from the dataframe
#'
#' @return dataframe
#'
#' @examples
#' #cleanTwitterTimeline(data, threshold = NULL)
#' #cleanTwitterTimeline(data, threshold = 10)
#' @export
cleanTwitterTimeline <- function(data, threshold = NULL) {

  data[is.na(data)] <- 0

  if (is.null(threshold)) {
    threshold <- defaultThreshold(data)
  }
  sums <- c()
  for (i in 1:nrow(data)) {
    sums <- c(sums, sum(data[i,2:length(data)]))
  }

  index <- which(sums < threshold)
  data <- data[-c(index) ,]
  names(data)[1] <- "Hashtag"
  rownames(data) <- 1:nrow(data)
  return(data)
}



#' defaultThreshold
#'
#' Calculates a threshlod value to remove hashtags from a dataframe
#' which have in sum a smaller count than the defined threshold.
#' Threshold is the next greatet number of the mean. Used by
#' cleanTwitterTimeline().
#'
#' @param data to calculate the threshold for.
#'
#' @return threshold integer
#'
#' @examples
#' #defaultThreshold(data)
#' @export
defaultThreshold <- function(data) {
  # calculate default threshold for the dataframe. i.e the value,
  # at which hashtags with an accumulated count lower than the threshold
  # are discarded

  # replace NA's and NaN in data frame
  data[is.na(data)] <- 0
  # calculat accummulated values
  sums <- c()
  for (i in 1:nrow(data)) {
    sums <- c(sums, sum(data[i,2:length(data)]))
  }
  # return threshold round up the mean(sum) to the next bigger number
  return(ceiling(mean(sums)))
}


#' absoluteHashtagCount
#'
#' Sums up the time series dataframe to get absolute counts
#' of a given time interval.
#'
#' @param dataframe to transform
#' @param interval for the specified timeframe
#'
#' @return interval which has been used to run createTwitterTimeline()
#'
#' @examples
#' #absoluteHashtagCount(dataframe, interval)
#' @export
absoluteHashtagCount <- function(dataframe, interval) {
  dataframe[is.na(dataframe)] <- 0

  for (i in 1:nrow(dataframe)) {
    for (j in 1:(length(dataframe) - 2))
      dataframe[i,j + 2] <- sum(dataframe[i,(j + 1):(j + 2)])
  }

  return(dataframe)
}



#' rankTweets
#'
#' Ranks the hashtags according to their respective counts. Highest
#' count in a given interval is being assigned rank 1. Second highest
#' counts rank 2 and so on.
#'
#' @param data dataframe time series to rank.
#'
#' @return dataframe consisting of ranked hashtags
#'
#' @examples
#' #rankTweets(dataframe)
#' @export
rankTweets <- function(data) {
  for (column in 2:length(data)) {
    data <- data[order(data[column],decreasing = TRUE),]

    uniques <- unlist(unique(data[column]))

    rank <- 1
    for (row in 1:nrow(data)) {
      if (data[row,column] == uniques[rank]) {
        data[row,column] <- rank
      }
      else {
        rank <- rank + 1
        data[row,column] <- rank
      }
    }
  }
  return(data)
}


# Helperfunction
appendDeltas <- function(toAppendTo, toAppendFrom) {

  indexmatch <- match(toAppendFrom[,1], toAppendTo[,1])


  elementpositionToExtract <- which(!is.na(indexmatch))


  toAppendTo[,paste0("new",(length(toAppendTo) - 1))] <- NaN


  for (i in 1:sum(!is.na(indexmatch))) {
    toAppendTo[indexmatch[elementpositionToExtract[i]],length(toAppendTo)] <-
      toAppendFrom[elementpositionToExtract[i],2]

  }

  columnsToAdd <- length(toAppendTo) - length(toAppendFrom)
  toAppendFrom[,c(paste0("new",seq(columnsToAdd)))] <- NaN
  names(toAppendFrom) <- names(toAppendTo)
  toAppendFrom[2:length(toAppendFrom)] <-
    toAppendFrom[(length(toAppendFrom):2)]
  toAppendTo <-
    rbind.data.frame(toAppendTo, toAppendFrom[which(is.na(indexmatch)),])
  return(toAppendTo)
}


#' createTwitterTimeSeries
#'
#'  Creates a time series of hashtags found in the given time interval.
#'  The time series gets cleaned by cleanTwitterTimeline() and ordered
#'  in decreasing fashion with respect to the last column.
#'
#' @param mongo mongo instance to use
#' @param interval timeinterval
#' @param intervalsteps number of parts to divide the interval into
#'
#' @return dataframe
#'
#' @examples
#' #timeseries <-
#' # createTwitterTimeSeries(hometimeline,c("2015-12-15 00:00:01","2015-12-17 14:59:59"),3)
#' @export
createTwitterTimeSeries <- function(mongo, interval, intervalsteps) {

  timeseries <- createTwitterTimeline(mongo, createIntervalVector(interval, intervalsteps))
  timeseries <- cleanTwitterTimeline(timeseries)
  timeseries <- timeseries[order(timeseries[ncol(timeseries)],decreasing = TRUE),]
  return(timeseries)
}

#' plotTweets
#'
#' Plots a dataframe in order to visualize the sum of hashtags in a
#' given time interval.
#'
#' @param data dataframe to plot created by createTwitterTimeSeries()
#' @param tophashtags number of hashtags to plot.
#' @param interval specified time interval. Relevant for the x-axis of the plot.
#'
#' @return plot of the given dataframe
#'
#' @examples
#' #plots the top 10 hashtags
#' #plotTweets(data,10)
#' #interval <- createIntervalVector(c("2016-01-15 20:00:00", "2016-01-16 00:10:00"),9)
#' #interval
#' #plotTweets(data,10, interval)
#' @export

plotTweets <- function(data, tophashtags, interval) {
  values <- hashtag <- date <- NULL

  data <- head(data, tophashtags)
  rownames(data) <- data$Hashtag
  if(missing(interval)==TRUE) {
    dates <- colnames(data[,2:ncol(data)])
  }
  else {
    dates <- as.POSIXct(interval[1:(length(interval)-1)]/1000, origin = "1970-01-01 00:00:00")
  }
  data <- as.data.frame(t(data[,2:ncol(data)]))
  data <- stack(data)
  data <- cbind.data.frame(data, rep(dates,tophashtags))
  colnames(data) <- c("values","hashtag","date")
  pd <- position_dodge(0.001)
  ggplot(data, mapping = aes(x=date, y=values, color=hashtag, ymax = max(data[1]))) + geom_line(position=pd,aes(group=hashtag))
}


