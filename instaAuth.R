# @param app_id numeric, Client ID of application to be used to create OAUth token. Available
# at \url{http://instagram.com/developer}
# 
# @param app_secret string, Client Secret of application to be used to create OAUth token.
# Available at \url{http://instagram.com/developer}.
#
# @param scope string, specifies scope of access to the authenticated user data. See
# \url{http://instagram.com/developer/authentication/#scope} for available options.

instaOAuth <- function(app_id, app_secret, scope="basic"){
  
  ## getting callback URL
  full_url <- httr::oauth_callback()
  full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
  ## fixing most common error
  if (full_url=="http://localhost:1410") full_url <- 'http://localhost:1410/'
  
  message <- paste("Copy and paste into 'OAuth redirect_uri' on Instagram App Settings:", 
                   full_url, "\nWhen done, press any key to continue...")
  ## prompting user to introduce callback URL in app page
  invisible(readline(message))
  ## using httr package functions
  instagram <- httr::oauth_endpoint(NULL, "authorize", "access_token",
                                    base_url = "https://api.instagram.com/oauth")
  myapp <- httr::oauth_app("instagram", app_id, app_secret)
  
  ## before httr 0.3
  if (packageVersion('httr') <= "0.2"){
    insta_token <- httr::oauth2.0_token(instagram, myapp, scope=scope)
    token <- httr::sign_oauth2.0(insta_token$access_token)
    if (httr::GET("https://api.instagram.com/v1/users/self/feed?count=1", 
                  config=token)$status==200){
      message("Authentication successful.")
    }
  }
  
  ## httr 0.3 to 0.6.1
  if (packageVersion('httr') > "0.2" & packageVersion('httr') <= "0.6.1"){
    token <- httr::oauth2.0_token(instagram, myapp, cache=FALSE, scope=scope)
    if (httr::GET(paste0("https://api.instagram.com/v1/users/self/feed?count=1", 
                         "&access_token=", 
                         token$credentials$access_token))$status==200){
      message("Authentication successful.")
    }   
  }
  
  ## current httr version
  if (packageVersion('httr') > "0.6.1"){
    Sys.setenv("HTTR_SERVER_PORT" = "1410/")
    token <- httr::oauth2.0_token(instagram, myapp, cache=FALSE, scope=scope)
    if (httr::GET(paste0("https://api.instagram.com/v1/users/self/feed?count=1", 
                         "&access_token=", 
                         token$credentials$access_token))$status==200){
      message("Authentication successful.")
    }  
  }
  
  return(token)
}
