

#' Store Twitter-API Credentials
#'
#' This function stores you Twitter-API key and secret to make it available for other functions and
#' for other instances of R.
#'
#' @param key The key for the Twitter-API: Consumer Key (API Key)
#' @param secret The scret for the Twitter-API: Consumer Secret (API Secret)
#'
#' @return None
#'
#' @examples
#' setAPICredentials(key = "Consumer Key", secret = "Consumer Secret")
#'
#' @export

setAPICredentials <- function(key, secret) {
  if ((!class(key) == "character") ||
      (!class(secret) == "character")) {
    stop("Key and sectet must be character")
  }
  Sys.setenv(twitter_key = key)
  Sys.setenv(twitter_secret = secret)
  message("Credentials stored, make sure to set callback url to http://127.0.0.1:1410")
}

#' Make OAuth
#'
#' Creates oAuth token for Twitter-API Access and returns token
#'
#' @param key The key for the Twitter-API (character)
#' @param secret The scret for the Twitter-API (character)
#'
#' @return OAuth token
#'
#' @examples
#' #makeOAuth()
#'
#' @export
#'

makeOAuth <-
  function(key = Sys.getenv("twitter_key"), secret = Sys.getenv("twitter_secret")) {
    # Generate or load authetification token, please store the credentials

    if ((key == "") || (secret == "")) {
      stop("Twitter key or secret are not set properly, use setAPICredentials to fix this.")
    }
    myapp <- httr::oauth_app("twitter", key = key, secret = secret)
    return (httr::oauth1.0_token(httr::oauth_endpoints("twitter"), myapp))
  }
