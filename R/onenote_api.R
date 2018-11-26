
globals = new.env()

assign('api_url_base','https://graph.microsoft.com/v1.0/me/onenote/',globals)

#' List of all available OneNote notebooks
#'
#' @examples
#' # Get all the notebooks on the server
#' \dontrun{
#' knoter::list_notebooks()
#' }
#' @export
list_notebooks <- function() {
	data <- do_api_call(url='notebooks', query=list(select= 'name'))
	if (! is.null(data) ) {
		return(as.character(unlist(data$value)))
	}
}

list_sections <- function(notebook) {
	notebook_id <- get_target_id(notebook)
	if (!is.null(notebook_id)) {
		datas = do_api_call(url=paste('notebooks/',notebook_id,'/sections',sep=''), query=list(select ='name' ))$value
		if (! is.null(datas)) {
			return(as.character(unlist(datas)))
		}
	}
}

get_sharepoint_id <- function(sharepoint_url) {
	url_parts = httr::parse_url(sharepoint_url)
	res = do_api_call(url=paste(url_parts$hostname,':/',url_parts$path,sep=''),method="get",base='https://graph.microsoft.com/v1.0/sites/')
	if ( ! is.null(res) ) {
	  message(paste('Enabling SharePoint',res$name,res$description))
	  return(res$id)
	}
	stop('Could not get SharePoint site id')
}

#' Enable reading/writing of Notebooks from a SharePoint site
#'
#' @examples
#' # Enable the SharePoint site at the given URL
#' \dontrun{
#' knoter::enable_sharepoint('https://xxxx.sharepoint.com/sites/MySite')
#' }
#' @export
enable_sharepoint <- function(sharepoint_url) {
  assign('api_url_base',paste('https://graph.microsoft.com/v1.0/sites/',get_sharepoint_id(sharepoint_url),'/onenote/',sep=''),envir=globals)
}

get_target_id <- function(notebook,section=NULL,page=NULL) {
	notebook_id = NULL
	section_id = NULL
	datas = do_api_call(url='notebooks', method="get", query=list(filter= paste("tolower(name) eq \'",tolower(notebook),"\'",sep='') ,select='id'))$value
	if ( ! is.null(datas) ) {
		notebook_id = datas[[1]]$id
	}
	if (is.null(section) && is.null(page)) {
		return(notebook_id)
	}
	if (!is.null(section) && !is.null(notebook_id)) {
		datas = do_api_call(url=paste('notebooks/',notebook_id,'/sections',sep=''), method="get",query=list(filter=paste( "tolower(name) eq \'",tolower(section),"\'",sep='' ) , select ='id,pages' ))$value
		if (! is.null(datas)) {
			section_id = datas[[1]]$id
		}
	}
	if (is.null(page)) {
		return(section_id)
	}

	if (!is.null(section_id)) {
		datas = do_api_call(url=paste('sections/',section_id,'/pages',sep=''), method="get",query=list( select ='id,title' ))$value
		if (! is.null(datas)) {
			for (page_data in datas) {
				if (tolower(page_data['title']) == tolower(page)) {
					return (page_data$id)
				}
			}
		}

	}

}

upload_files <- function(notebook_name,section_name,files=list()) {
	section_id = get_target_id(notebook_name,section_name)
	if ( ! is.null(section_id) && length(files) > 0 ) {
		do_api_call(paste( 'sections/',section_id,'/pages',sep=''), method='post', body=files)
	}

}

patch_page <- function(notebook_name,section_name,page_name,files=list()) {
	page_id = get_target_id(notebook_name,section_name,page_name)
	if ( ! is.null(page_id) && length(files) > 0) {
		names(files)[1] <- 'Commands'
		files[[1]] <- string_to_file_upload( 'Commands', paste("[{'target':'body','action':'append','content':'", gsub('"','\\\\"', gsub("'","\\\\'", readChar(files[[1]]$path,file.info(files[[1]]$path)$size) )) ,"'}]"), 'application/json' )
		results = do_api_call(paste('pages/',page_id,'/content',sep=''),method='patch',body=files)
	} else {
		message("Cannot get Page ID")
	}
}

handle_http_errors <- function(response) {
	status_code <- httr::status_code(response)
	if (status_code >= 200 && status_code < 300) {
		return ()
	}
	if (status_code >= 400 && status_code < 500) {
		stop("Not authorized to do this on the server")
	}
	if (status_code >= 500) {
		stop("There was a server error, try again later")
	}
}

do_api_call <- function(url,method='get',base=get('api_url_base',globals),...) {
	access_info <- doSignin()
	if (method == 'get') {
		resp = httr::GET(paste(base,url,sep=''),...,httr::config(token=access_info))
		handle_http_errors(resp)
	}
	if (method == 'post') {
		resp = httr::POST(paste(base,url,sep=''),encode="multipart",httr::config(token=access_info),...)
		handle_http_errors(resp)
	}
	if (method == 'patch') {
		resp = httr::PATCH(paste(base,url,sep=''),encode="multipart",httr::config(token=access_info),...)
		handle_http_errors(resp)
	}
	return (httr::content(resp))
}