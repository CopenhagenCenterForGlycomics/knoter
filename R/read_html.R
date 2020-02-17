id_generator <- function(size=6) {
    return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),size,replace=TRUE),collapse=''))
}

string_to_file_upload <- function(filename,contents,mime) {
    if ( compareVersion(as.character(packageVersion("httr")),"0.7") < 1 ) {
        result <- structure(list(filename = as.character(filename), contents = contents,
            contentType = as.character(mime)))
        attr(result,'class') <- "FileUploadInfo"
    } else {
        toupload <- tempfile()
        writeLines(contents, toupload)
        result <- httr::upload_file(toupload, type = as.character(mime))
    }
    result
}

css_def_to_inline <- function(css) {
    attrs = names(css)
    paste(sapply(attrs, function(attr) {
        cleaned_attr = gsub('&nbsp;',intToUtf8(0x00a0L),css[[attr]])
        cleaned_name = gsub('&nbsp;',intToUtf8(0x00a0L),attr)
        paste(cleaned_name,":", cleaned_attr,";",sep='')
    }),collapse=" ")
}

inline_css <- function(root) {
    style_nodes = XML::getNodeSet(root,'/html//style')
    if (is.null(style_nodes) || length(style_nodes) < 1 ) {
        return()
    }
    css_defs = unlist( sapply( style_nodes, function(style) {
        css_lines=strsplit( XML::getChildrenStrings(style),'\n',fixed=T)[[1]]
        if (any(grepl("[A-Za-z]",css_lines))) {
          return (knitr:::css.parser(lines=css_lines))
        }
    },simplify=F),recursive=F)
    if (is.null(css_defs)) {
        return()
    }
    css_defs = css_defs[ ! is.null(css_defs) ]
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
        XML::xmlAttrs(pre_node) <- c(style="font-family: Courier; font-size: 4px;")
        XML::xmlName(pre_node) <- 'div'
    })
}

style_output_tags <- function(root) {
    divs = XML::getNodeSet(root,'//div[@class="source"]')
    sapply(divs,function(div_node) {
        table_el <- XML::newXMLNode("table")
        row_el <- XML::newXMLNode('tr')
        td_el <- XML::newXMLNode('td')
        XML::addChildren(table_el, row_el)
        XML::addChildren(row_el,td_el)
        XML::replaceNodes(div_node,table_el)
        XML::xmlAttrs(row_el) <- c(style="background-color: #ffffff;")
        XML::addChildren(td_el,div_node)
        XML::xmlAttrs(div_node) <- c(style="background-color: #ffffff;")
    })
}

style_source_tags <- function(root) {
    divs = XML::getNodeSet(root,'//div[@class="source"]')
    sapply(divs,function(div_node) {
        table_el <- XML::newXMLNode("table")
        row_el <- XML::newXMLNode('tr')
        td_el <- XML::newXMLNode('td')
        XML::addChildren(table_el, row_el)
        XML::addChildren(row_el,td_el)
        XML::replaceNodes(div_node,table_el)
        XML::xmlAttrs(row_el) <- c(style="background-color: #f5f5f5;")
        XML::addChildren(td_el,div_node)
        XML::xmlAttrs(div_node) <- c(style="background-color: #f5f5f5;")
    })
}

read_html <- function(html,asText,fragment.only=F,batch.chunks=10) {
    root <- XML::htmlParse(html,asText=asText)

    inline_css(root)
    style_pre_tags(root)
    style_source_tags(root)

    head_text = XML::saveXML(XML::getNodeSet(root,'//head')[[1]],doctype=NULL)

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
    additional_elements = NULL

    chunks = XML::getNodeSet(root,'//body/*')
    chunk_length = ifelse(batch.chunks > 0,batch.chunks,length(chunks))
    chunk_groups = suppressWarnings(Filter(function(x) { length(x) > 0 }, split(chunks,cut(1:length(chunks), max(floor(length(chunks) / chunk_length)+1,2),labels=F)) ))
#XML::saveXML(root,doctype=NULL)
    if (! fragment.only) {
        element_to_save = paste(c('<?xml version="1.0" encoding="utf-8" ?>\n','<html>',head_text, '<body>', sapply(chunk_groups[[1]],function(chunk) { XML::saveXML(chunk,doctype=NULL) }) , '</body>','</html>'), sep='',collapse='')
        additional_elements = sapply( chunk_groups[2:length(chunk_groups)], function(chunkset) {  paste(c('<div>',sapply( chunkset, function(chunk) { XML::saveXML(chunk,doctype=NULL) } ),'</div>'),sep='',collapse='') } )
    } else {
        target_node = XML::getNodeSet(root,'//body')[[1]]
        XML::xmlName(target_node) <- 'div'
        element_to_save = XML::saveXML(target_node)
    }
    html_blocks = sapply(list(c(element_to_save,additional_elements)), function(htmlblock) { gsub(intToUtf8(0x00a0L),'&nbsp;',htmlblock) })
    filesets = lapply(html_blocks, function(html_block) {
        files = list()
        files[['Presentation']] <- string_to_file_upload( 'Presentation', html_block, 'application/xhtml+xml' )
        to_attach = Filter(function(x) !is.null(x),Filter(function(file) {  grepl(file$part_id, html_block) },to_attach))
        for (attachment in to_attach) {
            mime = mime::guess_type(URLdecode(attachment$filename))
            if (file.info(URLdecode(attachment$filename))$size <= 2*1024*1024) {
                files[[attachment$part_id]] <- httr::upload_file(URLdecode(attachment$filename),mime)
            } else {
                message(paste(URLdecode(attachment$filename),'is too large (> 2MB), skipping',sep=' '))
            }
        }
        files
    })
    toattach=filesets[[1]]
    if (length(filesets) > 1) {
        attributes(toattach)$extrablocks = filesets[-1]
    }
    toattach
}