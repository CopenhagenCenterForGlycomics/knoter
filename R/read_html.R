id_generator <- function(size=6) {
    return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),size,replace=TRUE),collapse=''))
}

string_to_file_upload <- function(filename,contents,mime) {
    result <- structure(list(filename = as.character(filename), contents = contents, 
        contentType = as.character(mime)))
    attr(result,'class') <- "FileUploadInfo"
    result
}

css_def_to_inline <- function(css) {
    attrs = names(css)
    paste(sapply(attrs, function(attr) {
        paste(attr,":",css[[attr]],";",sep='')
    }),collapse=" ")
}

inline_css <- function(root) {
    style_nodes = XML::getNodeSet(root,'/html/head/style')
    if (is.null(style_nodes) || length(style_nodes) < 1 ) {
        return()
    }
    css_defs = sapply( style_nodes, function(style) {
        css_lines=strsplit( XML::getChildrenStrings(style),'\n',fixed=T)[[1]]
        if (any(grepl("[A-Za-z]",css_lines))) {
            knitr:::css.parser(lines=css_lines)
        }
    })
    css_defs = css_defs[[ ! is.null(css_defs) ]]
    names(css_defs) <- gsub(' +',' ', gsub('.',' ',names(css_defs),fixed=T))
    spans = XML::getNodeSet(root,'//span[@class]')
    sapply(spans, function(span_node) {
        css_def = css_def_to_inline( css_defs[[ XML::xmlAttrs(span_node,'class') ]] )
        XML::xmlAttrs(span_node) <- c(style=css_def,class=NULL)
    })
}

style_pre_tags <- function(root) {
    pres = XML::getNodeSet(root,'//pre[@class="knitr r"]')
    sapply(pres,function(pre_node) {
        XML::xmlAttrs(pre_node) <- c(style="font-family: Courier;")
        XML::xmlName(pre_node) <- 'div'
    })
}

style_source_tags <- function(root) {
    divs = XML::getNodeSet(root,'//div[@class="source"]')
    sapply(divs,function(div_node) {
        XML::xmlAttrs(div_node) <- c(style="background-color: #f5f5f5;")
    })
}

read_html <- function(html,asText,fragment.only=F) {
    root <- XML::htmlParse(html,asText=asText)

    inline_css(root)
    style_pre_tags(root)
    style_source_tags(root)

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
    element_to_save = NULL
    if (! fragment.only) {
        element_to_save = paste('<?xml version="1.0" encoding="utf-8" ?>\n', XML::saveXML(root,doctype=NULL),sep='')
    } else {
        target_node = XML::getNodeSet(root,'//body')[[1]]
        XML::xmlName(target_node) <- 'div'
        element_to_save = XML::saveXML(target_node)
    }

    files = list()
    files[['Presentation']] <- string_to_file_upload( 'Presentation', element_to_save, 'application/xhtml+xml' )
    for (attachment in to_attach) {
        mime = mime::guess_type(attachment$filename)
        files[[attachment$part_id]] <- httr::upload_file(attachment$filename,mime)
    }
    files
}