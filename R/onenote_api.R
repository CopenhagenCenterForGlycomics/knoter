
api_url_base <- 'https://www.onenote.com/api/v1.0/'


list_notebooks <- function() {
	data <- do_api_call(url='notebooks', query=list(select= 'name'))
	if (data) {
		return(data$value)
	}
}

upload_files <- function(notebook_name,section_name,files=list()) {
    datas = do_api_call(url='notebooks', method="get", query=list(filter= paste("name eq \'",notebook_name,"\'",sep='') ,select="id"))$value
    notebook_id = NULL
    section_id = NULL
    if (! is.null(datas)) {
        notebook_id = datas[[1]]$id
        datas = do_api_call(url=paste('notebooks/',notebook_id,'/sections',sep=''), method="get",query=list(filter=paste( "name eq \'",section_name,"\'",sep='' ) , select ='id' ))$value
    }
    if (! is.null(datas)) {
        section_id = datas[[1]]$id
    }
    message(notebook_id)
    message(section_id)
    if ( ! is.null(notebook_id) && ! is.null(section_id) && length(files) > 0 ) {
    	do_api_call(paste( 'sections/',section_id,'/pages',sep=''), method='post', body=files)
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
}


#httr::POST("https://httpbin.org/post",encode="multipart",body=blah,config=httr::add_headers(Authorization = paste('Bearer', access_info$access_token)))
#httr::verbose(data_out=TRUE))