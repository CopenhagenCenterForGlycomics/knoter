
cacheEnv <- new.env()

assign('knoter_current_token',NULL,envir=cacheEnv)

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

AccessToken <- R6::R6Class("AccessToken", inherit= httr::Token2.0, list(
  params = list(as_header = T),
  initialize = function(token) {
    self$credentials = list(access_token=token)
  },
  can_refresh = function() {
    FALSE
  }
))

DEFAULT_CLIENT_ID <- 'ea21f153-ff9a-45f9-9044-74c2e142a4af'
# Using the "CCG Knoter" registered app on Azure Portal

# Expired token renew by:
# vault_sudo write oauth2/azure_ad/creds/knoter state=foo scopes="Notes.Create,Notes.ReadWrite.All,Sites.Read.All,offline_access"
#
# Get the code from the browser and then put it in
# vault_sudo write oauth2/azure_ad/creds/knoter code=$code



# @importFrom httr oauth_endpoint
# @importFrom httr oauth_app
# @importFrom httr oauth2.0_token
doSignin <- function(client_id,client_secret=NULL) {
  if (missing(client_id)) {
    env_name <- "ONENOTE_CLIENT_ID"
    env_client_id <- Sys.getenv(env_name)
    if (env_client_id != "") {
      client_id <- env_client_id
      message("Using Client ID stored in environment variable ", substr(env_client_id,1,4),"...")
    } else {
      client_id <- DEFAULT_CLIENT_ID
    }
  }
  if (missing(client_secret)) {
    env_name <- "ONENOTE_CONSUMER_SECRET"
    client_secret <- Sys.getenv(env_name)
    if (client_secret != "") {
      message("Using Client Secret stored in environment variable ", substr(client_secret,1,4),"...")
    } else {
      client_secret <- NULL
    }
  }

  if (('vaultr' %in% rownames(installed.packages())) && (Sys.getenv('VAULT_TOKEN') != "") ) {
    vault <- vaultr::vault_client(login="token",quiet=T)
    wanted_token=tryCatch(vault$read('oauth2/azure_ad/creds/knoter'),error=function (e) { errmessage = e[['message']]; stop(paste("Vault backend error",errmessage)); });
    if ( class(wanted_token) == 'list' ) {
      return (AccessToken$new( wanted_token[[1]] ));
    }
  }

  if (!is.null(get('knoter_current_token',envir=cacheEnv))) {
    return(get('knoter_current_token',envir=cacheEnv))
  }
  if (! is.null(getOption("knoter_current_token"))) {
    return(list(access_token=getOption("knoter_current_token")))
  }

  login.microsoft <- httr::oauth_endpoint(authorize="https://login.microsoftonline.com/common/oauth2/v2.0/authorize",access="https://login.microsoftonline.com/common/oauth2/v2.0/token")

  # https://login.live.com/oauth20_desktop.srf
  onenote.app <- httr::oauth_app("onenote",key=client_id,secret=client_secret)
  scopes = c('Notes.Create','Notes.ReadWrite.All','Sites.Read.All')

  if (! is.null(client_secret)) {
    token <- httr::oauth2.0_token(login.microsoft,onenote.app,scope=c(scopes,'offline_access'),use_oob=TRUE,oob_value="https://login.microsoftonline.com/common/oauth2/nativeclient",query_authorize_extra=list(response_mode="query"))
  } else {
    token <- RequestToken$new(onenote.app,login.microsoft,params = list(use_oob = F, scope = scopes, type = NULL, redirect_uri="https://knoter-auth.s3.amazonaws.com/index.html",as_header=T) )
  }
  assign('current_token', token, envir=cacheEnv)
	return (token)
}