
cacheEnv <- new.env()

assign('current_token',NULL,envir=cacheEnv)

init_oauth2.0_request_token <- function(endpoint, app, scope = NULL, type = NULL,
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

RequestToken <- R6::R6Class("RequestToken", inherit = httr::Token2.0, list(
  init_credentials = function() {
    message("Initing credentials from scratch")
    self$credentials <- init_oauth2.0_request_token(self$endpoint, self$app,
      scope = self$params$scope, type = self$params$type,
      use_oob = self$params$use_oob, redirect_uri = self$params$redirect_uri)
  },
  can_refresh = function() {
    TRUE
  },
  refresh = function() {
    message("Refreshing credentials")
    self$credentials <- init_oauth2.0_request_token(self$endpoint, self$app,
      scope = self$params$scope, type = self$params$type,
      use_oob = self$params$use_oob, redirect_uri = self$params$redirect_uri)
    self$cache()
  }
))

# @importFrom httr oauth_endpoint
# @importFrom httr oauth_app
# @importFrom httr oauth2.0_token
doSignin <- function(client_id=getOption("OneDriveClientId"),client_secret=getOption("GoogleClientSecret")) {
  if (!is.null(get('current_token',envir=cacheEnv))) {
    return(get('current_token',envir=cacheEnv))
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
    token <- RequestToken$new(onenote.app,login.live,params = list(use_oob = F, scope = scopes, type = NULL, redirect_uri="https://knoter-auth.s3.amazonaws.com/index.html",as_header=T) )
#    get_request_token(login.live,onenote.app,scope=scopes,use_oob = F, redirect_uri="https://knoter-auth.s3.amazonaws.com/index.html")
  }
  assign('current_token', token, envir=cacheEnv)
	return (token)
}