
`%n%` = function(x, y) if (is.null(x)) y else x

#' Mark a dataframe to be turned into a HTML table
#'
#' To mark out a dataframe as one that should be
#' turned into a HTML table, call this function
#' on the dataframe within the output chunk.
#'
#' @param dataframe Data frame to turn into a HTML table
#' @seealso \code{\link{knit}}
#' @export
prettytable <- function(dataframe,environment=parent.frame()) {
  dataframe
}

#' @export
excel.workbook <- function(...,name=NULL) {
  result <- list(...)
  class(result) <- 'excel.workbook'
  attributes(result)$filename = name
  result
}

#' @export
excel <- excel.workbook

#' Mark a dataframe to be turned into a HTML table or print using
#' the default Rstudio method if running in an interactive environment
#'
#' @param dataframe Data frame to turn into a HTML table
#' @export
print_prettytable <- function(dataframe) {
  dataframe
}

render = function (x, ...) {
  UseMethod("render", x)
}

render.grob = function(grob) {
  grid::grid.draw(grob)
}

render.ggplot = function(ggplot) {
  ggplot2:::print.ggplot(ggplot)
}

#' Mark a list of plots to be turned into a multi-page PDF
#'
#' @param plots List of plots to convert into a single PDF
#' @seealso \code{\link{knit}}
#' @export
multipage <- function(plots) {
  if (! requireNamespace('gridExtra',quietly=T)) {
    message("Missing gridExtra library, not combining PDF")
    return (plots)
  }
  plotlist = do.call(gridExtra::marrangeGrob,c(list(plots),ncol=1,nrow=1,list(top=NULL)))
  class(plotlist) <- "multipage"
  attributes(plotlist)$thumbnail = do.call(gridExtra::marrangeGrob,c(list(plots),nrow=1,ncol=length(plots),list(top=NULL)))
  attributes(plotlist)$count = length(plots)
  return(plotlist)
}

write_workbook <- function(data,filename) {
  if (!file.exists(dirname(filename))) {
    dir.create(dirname(filename),recursive=T)
  } else if (file.exists(filename)) {
    file.remove(filename)
  }
  datalist=c(list(),data)
  writexl::write_xlsx(x=datalist,path=filename,col_names=T,format_headers = T)
}

write_multipage_plot <- function(plotlist,thumbnail=NULL,pages=1,options=list()) {
  multipage_image_id = multipage_counter()
  filename = knitr::fig_path(paste('',multipage_image_id,'multi.pdf',sep='.'),options,number=NULL)
  filename_thumbnail = knitr::fig_path(paste('',multipage_image_id,'multi.png',sep='.'),options,number=NULL)
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  pdf(file=filename,width=options$fig.width %n% 5L,height=options$fig.height %n% 5L)
  lapply(plotlist, function(plot) { grid::grid.newpage(); grid::grid.draw(plot); })
  dev.off()
  if (!is.null(thumbnail)) {
    png(file=filename_thumbnail,width= (pages * options$fig.width) %n% 5L,height=options$fig.height %n% 5L,units="in",res=72)
    grid::grid.draw(thumbnail);
    dev.off()
    return (list(multi=filename,thumbnail=filename_thumbnail))
  }
  return (list(multi=filename))
}

fix_escaping <- function(html) {
  # Markdown gets escaped to HTML - we should unescape anything
  # https://github.com/rstudio/markdown/blob/master/src/houdini_html_e.c
  root <- XML::htmlParse(html,asText=T)
  comments <- XML::getNodeSet(root,'//comment()')

  ESCAPE_LOOKUPS <- list( '&#39;' = "'", '&#47;' = '/', '&quot;' = '"', '&amp;' = '&', '&lt;' = '<', '&gt;' = '>' )

  if (is.null(comments)) {
    XML::free(root)
    return(html)
  }
  to_change <- list()
  if (length(comments) > 0) {
    sapply(1:length(comments),function(idx) {
      comment = XML::xmlValue(comments[[idx]])
      to_change[[comment]] <<- Reduce(function(out,next.val) {
        gsub(next.val,ESCAPE_LOOKUPS[[next.val]],out,fixed=T)
      },names(ESCAPE_LOOKUPS),comment)
    })
  }
  html <- Reduce(function(out,next.val) {
    gsub(next.val,to_change[[next.val]],out,fixed=T)
  },names(to_change),html)

  XML::free(root)
  return (html)

}

set_knit_opts_common = function() {

  knitr::opts_chunk$set(dev=c('png','pdf'),dev.args=list(pdf=list(useDingbats=F,onefile=T)),data.path='knoter_data/')
  
  knitr::opts_chunk$set(reset.counters=T)

}

set_knit_opts_rmarkdown = set_knit_opts_common

set_knit_opts_hooks_common = function() {
  knitr::opts_hooks$set(
    child.md = function(options) {
      options$results = 'asis'
      options$echo = FALSE
      options
    },
    data.path = function(options) {
      options$data.path = calculate_data_path(options)
      options
    },
    label.prefix = function(options) {
      if (is.null(options$label) || any(stringr::str_detect(string=options$label,pattern='unnamed-chunk'))) {
        options$label = paste(options$label.prefix,options$label.count,sep='_')
        message('Renamed label: ',options$label)
        knitr::opts_chunk$set(label.count=options$label.count+1)
      }
      options
    }
  )
}

set_knit_opts_hooks_html = set_knit_opts_hooks_common
set_knit_opts_hooks_rmarkdown = set_knit_opts_hooks_common

set_knit_hooks_common = function() {
  knitr::knit_hooks$set(
    reset.counters=function(before,options,envir) {
      if ( before ) {
        workbook_counter(reset=T)
        multipage_counter(reset=T)
      }
    }
  )  
}

calculate_data_path = function(opts) {
  file.path(dirname(opts$fig.path),opts$data.path)
}

set_knit_hooks_rmarkdown = function() {
  set_knit_hooks_common()
  
  knitr::knit_hooks$set(
    document=function(x) {
      x
    },
    child.md = function(before,options,envir) {
      if ( before ) {
        default_label = knitr::opts_knit$get('unnamed.chunk.label')
        current_counter = knitr:::chunk_counter() - 1
        knitr:::chunk_counter(reset=T)
        knitr::opts_knit$set('unnamed.chunk.label'=gsub('\\..*','',options$child.md))
        res = knitr::knit_child(options$child.md)
        knitr::opts_chunk$set('unnamed.chunk.label'=default_label)
        counter_inc = knitr:::chunk_counter(reset=T)
        while (counter_inc < current_counter) {
          counter_inc = knitr:::chunk_counter()
        }
        return(res)
      }
    }
  )
}

post_html_fixes = function(text) {

  text = gsub('&nbsp;','nbsp',paste(text,collapse=''))
  text = gsub('NEWLINECODE','<br/>',paste(text,collapse=''))
  text = gsub('NEWLINE','\n',paste(text,collapse=''))

  root <- XML::htmlParse(text,asText=T,replaceEntities=T)
  nodeset <- XML::getNodeSet(root, "/html/head/meta[@name='created']")
  if (! is.null(nodeset)) {
    XML::removeNodes(nodeset)
  }
  head_elements = XML::getNodeSet(root, "/html/head")
  if ( ! is.null(head_elements) && length(head_elements) > 0 ) {
    XML::addChildren(head_elements[[1]], XML::newXMLNode("meta", attrs=c(name='created', content=format(Sys.time(), "%FT%H:%M:%S%z" ))))
    XML::addChildren(head_elements[[1]], XML::newXMLNode("meta", attrs=c(charset="utf-8")))
  }

  
  # Fix the line breaks in code blocks

  pre_element_text = c( XML::getNodeSet(root,"//pre/text()"), XML::getNodeSet(root,"//code/text()") ) 
  for ( pre_text in pre_element_text ) {
    old_text = XML::xmlValue(pre_text)
    XML::xmlValue(pre_text) = gsub('\n','#br#\n',old_text)
  }

  text = XML::saveXML(root)
  text = gsub('nbsp','&nbsp;',text)
  text = gsub('__TWOSPACE__ ','&nbsp;&nbsp;',paste(text,collapse=''))
  text = gsub('#br#\n*</pre>','</pre>',text)
  text = gsub('#br#\n*</code>','</code>',text)

  text = gsub('#br#','<br/>',text)

  XML::free(root)

  text
}

object_counter = function(init = 0L) {
  n = init
  function(reset = FALSE) {
    if (reset) return(n <<- init)
    n <<- n + 1L
    n - 1L
  }
}

workbook_counter = object_counter(1L)
multipage_counter = object_counter(1L)


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
#' Input data to be knitted can be either a filename, or
#' text containing the \code{Rmd} or \code{Rhtml} that
#' can be converted. Regardless of the input type, the
#' output of the function will be the knitted html - unless
#' the output parameter is specified, and the filename that
#' the output data is written to is returned.
#'
#' @param ...     Parameters passed to \code{link[knitr]{knit}}.
#' @param append.meta.created Append a meta tag with the created date
#' @seealso \code{\link[knitr]{knit}}
#' @examples
#' \dontrun{
#' # Return the knitted Rhtml document as HTML text
#' html_string <- knoter::knit('example.Rhtml')
#'
#' # Return the knitted Rmd document as HTML text
#' html_string <- knoter::knit('example.Rmd')
#'
#' @export
knit <- function(...) {
  args = list(...)

  args$notebook = NULL

  return (do.call(knote,args))
}