id_generator <- function(size=6) {
    return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),size,replace=TRUE),collapse=''))
}

string_to_file_upload <- function(filename,contents,mime) {
    result <- structure(list(filename = as.character(filename), contents = contents, 
        contentType = as.character(mime)))
    attr(result,'class') <- "FileUploadInfo"
    result
}

read_html <- function(filename) {
    root <- XML::htmlParse(filename,asText=F)
    elements_to_read <- c( XML::getNodeSet(root, '//img[not(starts-with(@src, "http"))]') , XML::getNodeSet(root,'//object[starts-with(@data, "file://")]') )
    to_attach <- list()
    for (external in elements_to_read) {
        part_id = id_generator()
        tag_name = XML::xmlName(external)
        if (tag_name == 'img') {
            filename = as.character(XML::xmlAttrs(external)['src'])
            XML::xmlAttrs(external,append=T) <- c('src'=paste('name:',part_id,sep=''))
        }
        if (tag_name == 'object') {
            filename = as.character(XML::xmlAttrs(external)['data'])
            XML::xmlAttrs(external,append=T) <- c('data'=paste('name:',part_id,sep=''))
        }
        to_attach[[length(to_attach)+1]] <- list(part_id=part_id, filename=  gsub("file://",'', filename,fixed=T) )
    }
    files = list()
    files[['Presentation']] <- string_to_file_upload( 'Presentation', paste('<?xml version="1.0" encoding="utf-8" ?>\n', XML::saveXML(root,doctype=NULL),sep=''), 'application/xhtml+xml' )
    for (attachment in to_attach) {
        mime = mime::guess_type(attachment$filename)
        files[[attachment$part_id]] <- httr::upload_file(attachment$filename,mime)
    }
    files
}