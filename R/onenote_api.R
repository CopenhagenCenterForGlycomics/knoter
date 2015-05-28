
api_url_base <- 'https://www.onenote.com/api/v1.0/'


list_notebooks <- function() {
	data <- do_api_call(url='notebooks', query=list(select= 'name'))
	if (data) {
		return(data$value)
	}
}


get_target_id <- function(notebook,section=NULL,page=NULL) {
	notebook_id = NULL
	section_id = NULL
	datas = do_api_call(url='notebooks', method="get", query=list(filter= paste("name eq \'",notebook,"\'",sep='') ,select='id'))$value
	if ( ! is.null(datas) ) {
		notebook_id = datas[[1]]$id
	}
	if (is.null(section) && is.null(page)) {
		return(notebook_id)
	}
	if (!is.null(section) && !is.null(notebook_id)) {
		datas = do_api_call(url=paste('notebooks/',notebook_id,'/sections',sep=''), method="get",query=list(filter=paste( "name eq \'",section,"\'",sep='' ) , select ='id,pages' ))$value
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
				if (page_data['title'] == page) {
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
		files[[1]]$filename <- 'Commands'
		files[[1]]$contentType <- 'application/json'
		files[[1]]$contents <- paste("[{'target':'body','action':'append','content':'", gsub('"','\\\\"', gsub("'","\\\\'",files[[1]]$contents)) ,"'}]")
		results = do_api_call(paste('pages/',page_id,'/content',sep=''),method='patch',body=files)
	} else {
		message("Cannot get Page ID")
	}
}

do_api_call <- function(url,method='get',...) {
	access_info <- doSignin()
	config=httr::add_headers(Authorization = paste('Bearer', access_info$access_token))
	if (method == 'get') {
		return ( httr::content(httr::GET(paste(api_url_base,url,sep=''),...,config=config)) )
	}
	if (method == 'post') {
		return ( httr::content(httr::POST(paste(api_url_base,url,sep=''),encode="multipart",config=config,...)) )
	}
	if (method == 'patch') {
		return ( httr::content(httr::PATCH(paste(api_url_base,url,sep=''),encode="multipart",config=config,...)) )
	}
}