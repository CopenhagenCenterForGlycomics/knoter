
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
prettytable <- function(dataframe,environment=parent.frame()) {
  dataframe <- xtable::xtable(dataframe)
  varid <- substring(tempfile(pattern="xtable",tmpdir=''),2)
  assign( variable_name_for_class('xtable',parent.frame()) ,dataframe,environment)
}

#' Mark a dataframe to be turned into a HTML table or print using
#' the default Rstudio method if running in an interactive environment
#'
#' @param dataframe Data frame to turn into a HTML table
#' @export
print_prettytable <- function(dataframe) {
  if ( require('rstudioapi',quietly=T) ) {
    if (!rstudioapi::isAvailable()) {
      return( knoter::prettytable(dataframe,environment=knitr::knit_global()) )
    }
  }
  return(print(dataframe))
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
  varid <- substring(tempfile(pattern="multipage",tmpdir=''),2)
  assign( variable_name_for_class("multipage",parent.frame()) ,plotlist,parent.frame())
  return (render(plots[[1]]))
}

extract_source_excel_block <- function(tags) {
  root <- XML::htmlParse(paste(tags,collapse=''),asText=T)
  source_node <- XML::getNodeSet(root, "/html/body/root/div[@class='source']")
  results <- list()
  if (!is.null(source_node) && length(source_node) > 0) {
    results$source_node <- XML::saveXML(source_node[[1]])
  } else {
    results$source_node = ''
  }
  results$excel_node <- sapply(XML::getNodeSet(root,'//excel/text()'),function(node) { XML::saveXML(node) })
  XML::free(root)
  results
}

variable_name_for_class <- function(clazz,envir) {
  existing_objects <- sort( Filter(function(var) { clazz %in% class(envir[[var]]) },objects(envir)) )
  if (length(existing_objects) > 0) {
    varid <- paste(existing_objects[[1]],length(existing_objects)+1,sep='_')
  } else {
    varid <- substring(tempfile(pattern=clazz,tmpdir=''),2)
  }
  varid
}

get_objects_with_class <- function(clazz,envir,remove=T) {
  object.names=sort( Filter(function(var) { clazz %in% class(envir[[var]])},objects(envir)) )
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
  if (!file.exists(dirname(filename))) {
    dir.create(dirname(filename),recursive=T)
  } else if (file.exists(filename)) {
    file.remove(filename)
  }
  datalist=c(list(),data)
  writexl::write_xlsx(x=datalist,path=filename,col_names=T,format_headers = T)
}

write_multipage_plots <- function(plotlists,options) {
  sapply(1:length(plotlists),function(idx) {
    filename = knitr::fig_path(paste('',idx,'multi.pdf',sep='.'),options,number=NULL)
    plotlist = plotlists[[idx]]
    pdf(file=filename,width=options$fig.width %n% 5L,height=options$fig.height %n% 5L)
    lapply(plotlist, function(plot) { grid::grid.newpage(); grid::grid.draw(plot); })
    dev.off()
    filename
  })
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

#' @export
knit_child <- function(input,...) {
  results = markdown::markdownToHTML(input,text=text,output=NULL,options=c('skip_style'),stylesheet='',extensions=c('fenced_code'),fragment.only=T)
  rHtml = gsub("<code>r ([^\\>]*)</code>","<!--rinline \\1 -->",results)
  rHtml = gsub( "</code></pre>", "end.rcode-->\n",  gsub("<pre><code class=\"r([^\"]*)\">","\n<!--begin.rcode \\1\n",rHtml))
  in_rcode = FALSE
  lines = sapply(unlist(strsplit(rHtml,'\n')),function(line) {
    if (grepl("begin.rcode",line)) {
      in_rcode <<- TRUE
    }
    if (grepl("end.rcode",line)) {
      if ( ! in_rcode ) {
        return ("</code></pre>")
      }
      in_rcode <<- FALSE
    }
    line
  })
  rHtml = paste(lines,collapse="\n")
  rHtml = fix_escaping(rHtml)
  knitr::knit_child(text=rHtml,quiet=F)
}

knit.md <- function(input,text=NULL,...) {
  results = markdown::markdownToHTML(input,text=text,output=NULL,options=c('skip_style'),stylesheet='',extensions=c('fenced_code'))
  rHtml = gsub("<code>r ([^\\>]*)</code>","<!--rinline \\1 -->",results)
  rHtml = gsub( "</code></pre>", "end.rcode-->\n",  gsub("<pre><code class=\"r([^\"]*)\">","\n<!--begin.rcode \\1\n",rHtml))
  in_rcode = FALSE
  lines = sapply(unlist(strsplit(rHtml,'\n')),function(line) {
    if (grepl("begin.rcode",line)) {
      in_rcode <<- TRUE
    }
    if (grepl("end.rcode",line)) {
      if ( ! in_rcode ) {
        return ("</code></pre>")
      }
      in_rcode <<- FALSE
    }
    line
  })
  rHtml = paste(lines,collapse="\n")
  rHtml = fix_escaping(rHtml)
  args = list(...)
  args['text'] <- rHtml
  do.call( knoter::knit, args )
}

file_is_markdown <- function(input,text=NULL) {
  if (is.character(input) && tools::file_ext( input ) %in% c('Rmd','md')) {
    return(TRUE)
  }
  if (is.null(text)) {
    return(FALSE)
  }
  text = unlist(strsplit(text,"\n"))

  pat = knitr::all_patterns[['md']][['chunk.begin']]

  if (is.character(text) && length(grep(pat, text))) {
    return(TRUE)
  }

  pat = '`r[ #]([^`]+)\\s*`' #knitr::all_patterns[['md']][['inline.code']]

  if (is.character(text) && length(grep(pat, text))) {
    return(TRUE)
  }
  return(FALSE)
}

set_knit_opts_common = function() {

  knitr::opts_chunk$set(dev=c('png','pdf'),dev.args=list(pdf=list(useDingbats=F,onefile=T)),data.path='knoter_data/')
  
  if (requireNamespace('writexl',quietly=T)) {
    knitr::opts_chunk$set(check.excel=T)
    knitr::opts_knit$set(eval.after = 'check.excel')
  } else {
    message("Library 'writexl' is not installed, not writing Excel files")
  }

  knitr::opts_chunk$set(make.tables=T)
  knitr::opts_chunk$set(make.multipage=T)
  knitr::opts_chunk$set(chunk.post=T)

}

set_knit_opts_html = set_knit_opts_common
set_knit_opts_rmarkdown = set_knit_opts_common

set_knit_opts_hooks_common = function() {
  knitr::opts_hooks$set(
    child.md = function(options) {
      options$results = 'asis'
      options$echo = FALSE
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
    check.excel=function(before,options,envir) {
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
          table_data <- (sapply(tables,function(tab) { print(tab,type='html',print.results=F) },simplify=T,USE.NAMES=F))
          return (table_data)
        }
      }
    },make.multipage=function(before,options,envir) {
      if ( ! before) {
        plots = get_objects_with_class('multipage',envir)
        if (length(plots) > 0) {
          filenames=write_multipage_plots(plots,options)
          return(paste(sapply(filenames,function(filename) {
            paste('<object type="application/pdf" data="file://',
               filename,
               '" data-attachment="',
               options$label,
               '.pdf"></object></div><div class="rcode">\n'
            ,sep='')
          }),collapse=''))
        }
      }
    }
  )  
}

set_knit_hooks_rmarkdown = function() {
  set_knit_hooks_common()
  
  knitr::knit_hooks$set(
    document=function(x) {
      x
    }
  )
}

post_html_fixes = function(text) {

  text = gsub('&nbsp;','nbsp',paste(text,collapse=''))
  text = gsub('NEWLINE','\n',paste(text,collapse=''))

  root <- XML::htmlParse(text,asText=T,replaceEntities=T)
  nodeset <- XML::getNodeSet(root, "/html/head/meta[@name='created']")
  if (! is.null(nodeset)) {
    XML::removeNodes(nodeset)
  }
  head_elements = XML::getNodeSet(root, "/html/head")
  if ( ! is.null(head_elements) && length(head_elements) > 0 ) {
    XML::addChildren(head_elements[[1]], XML::newXMLNode("meta", attrs=c(name='created', content=format(Sys.time(), "%FT%H:%M:%S%z" ))))
  }

  
  # Fix the line breaks in code blocks

  pre_element_text = c( XML::getNodeSet(root,"//pre/text()"), XML::getNodeSet(root,"//code/text()") ) 
  for ( pre_text in pre_element_text ) {
    old_text = XML::xmlValue(pre_text)
    XML::xmlValue(pre_text) = gsub('\n','#br#\n',old_text)
  }

  text = XML::saveXML(root)
  text = gsub('nbsp','&nbsp;',text)
  text = gsub('#br#\n*</pre>','</pre>',text)
  text = gsub('#br#\n*</code>','</code>',text)

  text = gsub('#br#','<br/>',text)

  XML::free(root)

  text
}

set_knit_hooks_html = function() {
  set_knit_hooks_common()

  old_chunk <- knitr::knit_hooks$get('chunk')
  old_source <- knitr::knit_hooks$get('source')

  knitr::knit_hooks$set(
    document=function(x) {
      if ( ! append.meta.created ) {
        return (x);
      }
      return(post_html_fixes(x))
    },
    child.md=function(before,options,envir) {
      if ( before ) {
        default_label = knitr::opts_knit$get('unnamed.chunk.label')
        current_counter = knitr:::chunk_counter() - 1
        knitr:::chunk_counter(reset=T)
        knitr::opts_knit$set('unnamed.chunk.label'=gsub('\\..*','',options$child.md))
        res = knit_child(options$child.md)
        knitr::opts_chunk$set('unnamed.chunk.label'=default_label)
        counter_inc = knitr:::chunk_counter(reset=T)
        while (counter_inc < current_counter) {
          counter_inc = knitr:::chunk_counter()
        }
        return(res)
      }
    },
    plot=function(x,options) {
      x = knitr::hook_plot_html(x,options)
      paste(x, '</div><div>',
               '<object type="application/pdf" data="file://',
               knitr::fig_path('.pdf',options),
               '" data-attachment="',
               options$label,
               '-',
               options$fig.cur %n% 1L,
               '.pdf"></object></div><div class="rcode">\n'
            ,sep='')
    },
    chunk=function(x,options) {
      if (grepl("<excel>",paste(x,collapse=''),fixed=T))  {
        components<-extract_source_excel_block(c('<root>',x,'</root>'))
        x<- c( components$source_node, sapply(components$excel_node, function(file) {
          paste( '<object type="application/vnd.ms-excel" data="file://',
                 file,
                 '" data-attachment="',
                 basename(file),
                 '"></object>',sep='')
        },USE.NAMES=F,simplify=F))
        block <- paste(x,sep='',collapse='')
        x <- block
      }
      x <- old_chunk(x,options)
      # Remove trailing spaces
      x = gsub(' +\n','\n',x)
      # Remove trailing newlines
      x = gsub("\n$","",x)
      # Turn multiple newlines into br
      x = gsub("\n+","<br/>",x,fixed=T)
      # Turn double space into non-breaking space
      x = gsub('  ', "&nbsp;&nbsp;",x)
      return( paste(x,"\n",sep=''))
    },source=function(x,options) {
      x <- old_source(x,options)
      x = gsub(' +\n','\n',x)
      paste(gsub('  ', "&nbsp;&nbsp;", gsub("\n+","<br/>",gsub("\n$","",x),fixed=T)),"\n",sep='')
    }
  )

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
#' # Write the output of the knit to a file.
#' out_filename <- knoter::knit('example.Rhtml',output='example.html')
#'
#' # Convert some Rmd to a HTML document
#' rmd_text = "## This is a heading ##\n\nThis is a paragraph with some inline R `r 1 + 1` that will be converted.\n\n"
#' html_string <- knoter::knit(text=rmd_text)
#'
#' # or if we want to write it to a file
#' out_filename <- knoter::knit(text=rmd_text,output='example.html')
#' }
#' @export
knit <- function(...,append.meta.created=T) {
  args = list(...)

  if (file_is_markdown(args[[1]], text=args[['text']])) {
    args['append.meta.created'] <- append.meta.created
    return(do.call( knit.md, args))
  }
  if ( ( is.null(names(args)) || names(args)[[1]] == "" ) && file.exists(args[[1]]) && is.null(args[['text']])) {
    args[['text']] <- readChar(args[[1]], file.info(args[[1]])$size)
    args[[1]] <- NULL
  }

  set_knit_hooks_html()

  knitr::render_html()

  set_knit_opts_html()

  set_knit_opts_hooks_html()

  return (do.call(knitr::knit,args))
}