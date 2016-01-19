#' buildRegexQuery
#'
#' Builds a regular expression query for mongoDB in json format,
#' for the fields "entitites.hashtags.text" and "text". Can be use
#' in conjunction with a time interval.
#'
#' @param regex the regular expression
#' @param timestamp to retrieve hits only within a given interval
#' @param fields to be searched by the regex. Default "text" and "entitites.hashtags.text".
#' @param regexoption options used for the regex. Default is "i", case insensitivity.
#' @param conjunction used to link the regex. Can only be "$and" or "$or". Default is "$or".
#'
#' @return query in jsonformat for mongolite
#'
#' @examples
#' #buildRegexQuery(c("koln","koeln","berlin"), timestamp = getInterval(
#' #inputinterval = c("2016-01-04 18:37:43 CET","2016-01-05 18:37:43 CET")))
#'
#' #hometimeline$find(buildRegexQuery(c("koln","koeln"),timestamp = getInterval(60*24)),
#' #getMongoFields(c("text","entitites.hashtags.text")))
#' @export

buildRegexQuery <- function(regex, timestamp, fields = c("entitites.hashtags.text","text"), regexoption = "i",  conjunction = "$or") {

  regexvector <- seq(1:(length(regex)*2))
  replace_by_option <- which(regexvector%%2 == 0)
  replace_by_regex <- which(regexvector%%2 == 1)

  regexvector[replace_by_option] <- regexoption
  regexvector[replace_by_regex] <- regex

  regexquery <- operatorValuePair(c("$regex","$options"), regexvector)
  regexquery <- fieldValuePair(fields, regexquery, conjunction)

  if(!missing(timestamp)) {
    timestamp <- as.character(timestamp)
    timestampquery <- operatorValuePair(c("$gte","$lt"), timestamp)
    timestampquery <- fieldValuePair(c("timestamp_ms"), timestampquery)
    query <- useConjunction(timestampquery, regexquery, "$and")
  }

  else {
    query <- regexquery
  }
  return(query)
}



#' fieldValuePair
#'
#' Creates field value pair, called by buildRegexQuery
#'
#' @param key, fields to search. "text" or "entitites.hashtags.text"
#' @param value, regex and options as value. use operatorValuePair()
#' @param conjunction used for the regular expressions. Can only by "$and" or "$or"
#'
#' @return field value pair used by buildRegexQuery()
#'
#' @examples
#' #fieldValuePair(key = c("entitites.hashtags.text","text"),value =
#' #operatorValuePair(c("$regex","$options"),c("syrien","i")),"$or")
#'
#' @export

fieldValuePair <- function(key, value, conjunction) {


  querystring <- NULL
  pastestring <- NULL

  if(length(key)==1) {
    for(i in 1:length(value)) {

      querystring[i] <- jsonlite::toJSON(setNames(as.list(value[i]), key))
    }
  }

  else {
    for(i in 1:length(value)) {

      pastestring[i] <- jsonlite::toJSON(setNames(as.list(value[i]), key[2]))
      querystring[i] <- jsonlite::toJSON(setNames(as.list(value[i]), key[1]))
    }
    pastestring <- gsub(pattern = "(\\[\")|\\\\|(\"\\])",replacement = "", x = pastestring)
  }

  querystring <- gsub(pattern = "(\\[\")|\\\\|(\"\\])",replacement = "", x = querystring)

  if(length(key)>1) {
    querystring  <- paste(querystring, pastestring , collapse = ",", sep = ",")

  }
  else {
    querystring  <- paste(querystring, collapse = ",", sep = ",")
  }
  if(!missing(conjunction)) {
    querystring  <- jsonlite::toJSON(setNames(as.list( querystring), conjunction))
    querystring <- gsub(pattern = "\\\\", replacement = "", x =  querystring)
    querystring <- gsub(pattern = "(\\:\\[\")(\\{.*\\})(\"\\])", replacement = ":[\\2]", x =  querystring)
  }

  return(querystring)
}

#' operatorValuePair
#'
#' Creates operator value pair, called by buildRegexQuery.
#'
#' @param key of the key/value pair. Is always a mongo operator like $regex
#' @param value of the key/value pair. Can be the regex and/or the corresponding option.
#'
#' @return operator value pair
#'
#' @examples
#' #operatorValuePair(c("$regex","$options"),c("koln","i"))
#'
#' @export

operatorValuePair <- function(key,value) {

  querystring <- NULL
  if ((length(value)==1)&&(length(key)==1)) {

    querystring <- jsonlite::toJSON(setNames(as.list(value), key))
  }
  else if (((length(key)==1)&&(length(value)>1))) {

    for(i in 1:(length(value)/length(key))) {
      querystring[i] <- jsonlite::toJSON(setNames(as.list(value[i]), key))
    }
  }
  else {
    for(i in 1:(length(value)/length(key))) {

      querystring[i] <- jsonlite::toJSON(setNames(as.list(value[(i*2-1):(i*2)]), key))
    }
  }
  querystring <- gsub(pattern = "\\[|\\]", replacement = "", x = querystring)
  return(querystring)
}

#' useConjunction
#'
#' Connects query parts with either $and or $or.
#'
#' @param firstpart first part of the query
#' @param secondpart second part of the query
#' @param conjunction which can only be "$and" or "$or"
#'
#' @return linked mongo query parts.
#'
#' @examples
#' useConjunction(c("$regex","$options"),c("syrien","i"),"$and")
#' @export

useConjunction <- function(firstpart, secondpart, conjunction) {
  querystring <- paste(firstpart, secondpart, sep =",")
  querystring <- jsonlite::toJSON(setNames(as.list(querystring), conjunction))
  querystring <- gsub(pattern = "\\\\", replacement = "", x = querystring)
  querystring <- gsub(pattern = "(\\:\\[\")(\\{.*\\})(\"\\])", replacement = ":[\\2]", x = querystring)

  return(querystring)
}

#' getMongoFields
#'
#' Creates JSON formated string of fields to be returned by a mongo find query,
#' so one does not have to write a JSON string "by hand".
#'
#' @param fields character vector of fields to be returned by a mongoDB query
#'
#' @return JSON format list of field to be returned
#'
#' @examples
#' #getMongoFields(c("text","created_at"))
#'
#' #getInterval(60*24)), getMongoFields(c("text","entitites.hashtags.text","created_at")))
#' @export

getMongoFields <- function(fields) {
  fields[length(fields) + 1] <- "_id"

  values <- c(rep(1,length(fields) - 1),0)

  jsonstring <- jsonlite::toJSON(setNames(as.list(values), fields))

  jsonstring <-
    gsub(pattern = "\\[|\\]",replacement = "",x = jsonstring)

  return(jsonstring)
}


