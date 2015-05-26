
`%n%` = function(x, y) if (is.null(x)) y else x

#' Mark a set of dataframes to be turned into an Excel workbook
#'
#' To mark out a set of dataframes to be turned into a Excel workbook,
#' call this function with the dataframes as arguments. Named arguments
#' will result in the names used as the worksheet names.
#'
#' @param ... Data frames to place in workbook
#' @param name Name to use to name the workbook file
#' @examples
#' excel(Cars_sheet=cars,CO2_sheet=CO2)
#'
#' # Use default sheet names
#' excel(cars,CO2)
#'
#' # Write to a specific filename
#' excel(cars,name="Cars")
#' @seealso \code{\link{knit}}
#' @export
excel <- function(...,name=NA) {
  obj <- list(...)
  class(obj) <- "excel.workbook"
  if ( ! is.na(name) ) {
    attr(obj,'name') <- name
  }
  assign(variable_name_for_class('excel.workbook',parent.frame()),obj,parent.frame())
  return ()
}

#' Mark a dataframe to be turned into a HTML table
#'
#' To mark out a dataframe as one that should be
#' turned into a HTML table, call this function
#' on the dataframe within the output chunk.
#'
#' @param dataframe Data frame to turn into a HTML table
#' @seealso \code{\link{knit}}
#' @export
table <- function(dataframe) {
  dataframe <- xtable::xtable(dataframe)
  varid <- substring(tempfile(pattern="html.table",tmpdir=''),2)
  assign( variable_name_for_class('excel.workbook',parent.frame()) ,dataframe,parent.frame())
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

variable_name_for_class <- function(class,envir) {
  existing_objects <- sort( Filter(function(var) { class(envir[[var]]) == class},objects(envir)) )
  if (length(existing_objects) > 0) {
    varid <- paste(existing_objects[[1]],length(existing_objects)+1,sep='_')
  } else {
    varid <- substring(tempfile(pattern=class,tmpdir=''),2)
  }
  varid
}

get_objects_with_class <- function(class,envir,remove=T) {
  object.names=sort( Filter(function(var) { class(envir[[var]]) == class},objects(envir)) )
  objects=sapply(object.names,function(obj) { get(obj,envir) },simplify=F)
  if (remove) {
    rm(list=object.names,envir=envir)
  }
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

knit.md <- function(file=NULL,...) {
  results = markdown::markdownToHTML(file,options=c('skip_style'),stylesheet='',extensions=c())
  rHtml = gsub("<code>r ([^\\>]*)</code>","<!--rinline \\1 -->",results)
  rHtml = gsub( "</code></p>", "end.rcode-->\n",  gsub("<p><code>\\{r([^\\}]*)\\}","\n<!--begin.rcode \\1",rHtml))
  rHtml = gsub('&#39;',"'",rHtml)
  rHtml = gsub('&quot;','"',rHtml)
  knoter::knit(...,text=rHtml)
}

file_is_markdown <- function(file=NULL,...) {
  if (is.character(file) && tools::file_ext( file ) == 'Rmd') {
    return(TRUE)
  }
  return(FALSE)
}

#' Knit a Rhtml or Rmd file to a HTML document
#'
#' Wrapper function for knitr that will take 
#' a source \code{Rhtml} or \code{Rmd} file,
#' and prepare it so that it can be uploaded.
#' This involves producing Excel workbooks 
#' and extra PDF versions of figures.
#'
#' Arguments are the same as found for
#' \code{\link[knitr]{knit}}.
#'
#' @param append.meta.created Append a meta tag with the created date
#' @seealso \code{\link[knitr]{knit}}
#' @export
knit <- function(...,append.meta.created=T) {
  if (file_is_markdown(...)) {
    return(knit.md(...,append.meta.created))
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

  knitr::opts_chunk$set(dev=c('png','pdf'),data.path='data/')
  if (requireNamespace('XLConnect',quietly=T)) {
    knitr::opts_chunk$set(check.excel=T)
    knitr::opts_knit$set(eval.after = 'check.excel')
  } else {
    message("XLConnect is not installed, not writing Excel files")
  }
  knitr::opts_chunk$set(make.tables=T)
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