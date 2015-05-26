
current_token <- NULL

get_request_token <- function(endpoint, app, scope = NULL, type = NULL,
                          use_oob = T, redirect_uri = 'urn:ietf:wg:oauth:2.0:oob',is_interactive = interactive()) {

  stopifnot(interactive())

  state <- httr:::nonce()

  if (isTRUE(use_oob)) {
    state <- NULL
  }

  scope_arg <- paste(scope, collapse = ' ')

  authorize_url <- httr::modify_url(endpoint$authorize, query = httr:::compact(list(
    client_id = app$key,
    scope = scope_arg,
    redirect_uri = redirect_uri,
    response_type = "token",
    state = state)))

  result <- NULL
  if (isTRUE(use_oob)) {
    result <- list(access_token = httr:::oauth_exchanger(authorize_url)$code)
  } else {
    result <- httr:::oauth_listener(authorize_url, is_interactive)
  }
  return(result)
}

# @importFrom httr oauth_endpoint
# @importFrom httr oauth_app
# @importFrom httr oauth2.0_token
doSignin <- function(client_id=getOption("OneDriveClientId"),client_secret=getOption("GoogleClientSecret")) {
  if (!is.null(current_token)) {
    return(current_token)
  }
  if (! is.null(getOption("current_token"))) {
    return(list(access_token=getOption("current_token")))
  }

	login.live <- httr::oauth_endpoint(authorize="https://login.live.com/oauth20_authorize.srf",access="https://login.live.com/oauth20_token.srf")

  # https://login.live.com/oauth20_desktop.srf

	onenote.app <- httr::oauth_app("onenote",client_id,secret=client_secret)
  scopes = c('office.onenote_create','office.onenote_update')

  if (! is.null(client_secret)) {
    token <- httr::oauth2.0_token(login.live,onenote.app,scope=c(scopes,'wl.offline_access'),use_oob=F)
  } else {
    token <- get_request_token(login.live,onenote.app,scope=scopes,use_oob = F, redirect_uri="https://knoter-auth.s3.amazonaws.com/index.html")
  }
  current_token <<- token
	return (token)
}