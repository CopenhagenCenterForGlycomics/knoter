
`%n%` = function(x, y) if (is.null(x)) y else x

excel <- function(...,name=NA) {
  obj <- list(...)
  class(obj) <- "excel.workbook"
  if ( ! is.na(name) ) {
    attr(obj,'name') <- name
  }
  varid <- substring(tempfile(pattern="excel",tmpdir=''),2)
  assign(varid,obj,parent.frame())
  return ()
}

table <- function(dataframe) {
  dataframe <- xtable::xtable(dataframe)
  varid <- substring(tempfile(pattern="html.table",tmpdir=''),2)
  assign(varid,dataframe,parent.frame())  
}

extract_source_excel_block <- function(tags) {
  root <- XML::htmlParse(paste(tags,collapse=''),asText=T)
  source_node <- XML::getNodeSet(root, "/html/body/root/div[@class='source']")[[1]]
  results <- list()
  if (!is.null(source_node)) {
    results$source_node <- XML::saveXML(source_node)    
  }
  results$excel_node <- sapply(XML::getNodeSet(root,'//excel/text()'),function(node) { XML::saveXML(node) })
  XML::free(root)
  results
}

get_objects_with_class <- function(class,envir) {
  object.names=Filter(function(var) { class(envir[[var]]) == class},objects(envir))
  objects=sapply(object.names,function(obj) { get(obj,envir) },simplify=F)
  rm(list=object.names,envir=envir)
  objects
}

write_excel_workbooks <- function(books,options) {
  bookids = as.vector(sapply(books, function(book) { attr(book,'name',exact=T) %n% 0L },simplify=T))
  bookids[ bookids == 0 ] <- paste('Workbook' , 1:length(bookids[bookids == 0]),sep='')
  filenames = file.path( options$data.path, paste( options$label,'-',bookids,'.xlsx',sep=''))
  sapply(1:length(filenames), function(idx) { write_workbook(books[[idx]],filenames[idx]) })
  return (filenames)
}

write_workbook <- function(data,filename) {
  if (!file.exists(dirname(filename)))
    dir.create(dirname(filename))
  output <- XLConnect::loadWorkbook(filename,create=T)
  for (sheet in names(data)) {
    XLConnect::createSheet(output,sheet)
    XLConnect::writeWorksheet(output,data[[sheet]],sheet=sheet)    
  }
  XLConnect::saveWorkbook(output)
}

knote.md <- function(file=NULL,...) {
  results = markdown::markdownToHTML(file,options=c('skip_style'),stylesheet='',extensions=c())
  rHtml = gsub("<code>r ([^\\>]*)</code>","<!--rinline \\1 -->",results)
  rHtml = gsub( "</code></p>", "end.rcode-->\n",  gsub("<p><code>\\{r([^\\}]*)\\}","\n<!--begin.rcode \\1",rHtml))
  rHtml = gsub('&#39;',"'",rHtml)
  rHtml = gsub('&quot;','"',rHtml)
  knote(...,text=rHtml)
}

file_is_markdown <- function(file=NULL,...) {
  if (is.character(file) && tools::file_ext( file ) == 'Rmd') {
    return(TRUE)
  }
  return(FALSE)
}

knote <- function(...,append.meta.created=T) {
  if (file_is_markdown(...)) {
    return(knote.md(...,append.meta.created))
  }
  knitr::knit_hooks$set(document=function(x) {
    if ( ! append.meta.created ) {
      return (x);
    }
    root <- XML::htmlParse(paste(x,collapse=''),asText=T)
    nodeset <- XML::getNodeSet(root, "/html/head/meta[@name='created']")
    if (! is.null(nodeset)) {
      XML::removeNodes(nodeset)      
    }
    XML::addChildren(XML::getNodeSet(root, "/html/head")[[1]], XML::newXMLNode("meta", attrs=c(name='created', content=format(Sys.time(), "%FT%H:%M:%S%z" ))))
    text <- XML::saveXML(root)
    XML::free(root)
    text
  },check.excel=function(before,options,envir) {
    if (! before) {
      workbooks = get_objects_with_class('excel.workbook',envir)
      if (length(workbooks) > 0) {
        filenames=write_excel_workbooks(workbooks,options)
        return(paste(sapply(filenames,function(filename) { paste('<excel>',filename,'</excel>',sep='') }),collapse=''))
      }
    }
  },make.tables=function(before,options,envir) {
    if ( ! before) {
      tables = get_objects_with_class('xtable',envir)
      if (length(tables) > 0) {
        table_data <- (sapply(tables,function(tab) { print(tab,type='html') },simplify=T,USE.NAMES=F))
        return (table_data)
      }      
    }
  })
  knitr::render_html()
  knitr::opts_chunk$set(dev=c('png','pdf'),check.excel=T,make.tables=T,data.path='data/')
  knitr::opts_knit$set(eval.after = 'check.excel')
  old_chunk <- knitr::knit_hooks$get('chunk')
  knitr::knit_hooks$set(plot=function(x,options) {
    x = knitr::hook_plot_html(x,options)
    paste(x, '</div><div>',
             '<object type="application/pdf" data="file://',
             knitr::fig_path('.pdf',options),
             '"  data-attachment="',
             options$label,
             '-',
             options$fig.cur %n% 1L,
             '"></object></div><div class="rcode">\n'
          ,sep='')
  },chunk=function(x,options) {
    message("Doing output of chunk",options$label)
    if (grepl("<excel>",paste(x,collapse=''),fixed=T))  {
      components<-extract_source_excel_block(c('<root>',x,'</root>'))
      x<- c( components$source_node, sapply(components$excel_node, function(file) {
        paste( '<object type="application/vnd.ms-excel" data="file://',
               file,
               '"  data-attachment="',
               basename(file),
               '"></object>',sep='')
      },USE.NAMES=F,simplify=F))
      block <- paste(x,sep='',collapse='')
      x <- block
    }
    return (old_chunk(x,options))
  });
  knitr::knit(...)
}