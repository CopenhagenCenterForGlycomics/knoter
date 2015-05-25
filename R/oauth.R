
current_token <- NULL

ask_request_token <- function(endpoint, app, scope = NULL, type = NULL,
                          use_oob = T) {
  if (isTRUE(use_oob)) {
    redirect_uri <- "https://login.live.com/oauth20_desktop.srf"
    state <- NULL
  }
  scope_arg <- paste(scope, collapse = ' ')

  authorize_url <- httr::modify_url(endpoint$authorize, query = httr:::compact(list(
    client_id = app$key,
    scope = scope_arg,
    redirect_uri = redirect_uri,
    response_type = "token",
    state = state)))
  if (isTRUE(use_oob)) {
    code <- httr:::oauth_exchanger(authorize_url)$code
  }
  list(access_token=code)
}

# @importFrom httr oauth_endpoint
# @importFrom httr oauth_app
# @importFrom httr oauth2.0_token
doSignin <- function() {
  if (!is.null(current_token)) {
    return((list(access_token=current_token)))
  }
  if (! is.null(getOption("current_token"))) {
    return(list(access_token=getOption("current_token")))
  }

	login.live <- httr::oauth_endpoint(authorize="https://login.live.com/oauth20_authorize.srf",access="https://login.live.com/oauth20_token.srf")
	onenote.app <- httr::oauth_app("onenote",getOption("OneDriveClientId"))
	token <- ask_request_token(login.live,onenote.app,scope="office.onenote_create",use_oob = T )
  current_token <- token$access_token
	return (token)
}