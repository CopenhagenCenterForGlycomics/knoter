#' @export
note_page <- function(notebook=NULL,section=NULL,sharepoint=NULL,keep_md=FALSE,batch.chunks=10) {

  args <- c("--template", as_tmpfile(template),"--highlight-style","pygments")

  rmarkdown::output_format(
    knitr = note_page_knitr_options(),
    pandoc = rmarkdown::pandoc_options(
      to = "html", from = NULL, args = args
    ),
    keep_md = keep_md,
    clean_supporting = FALSE,
    pre_knit = pre_knit,
    post_knit = post_knit,
    on_exit = on_exit,
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator,
    post_processor = purrr::partial(post_processor,notebook=notebook,section=section,sharepoint=sharepoint,batch.chunks=batch.chunks)
  )
}

template = "
<html>
  <head>
    <title>$title$</title>
    <style>$highlighting-css$</style>
  </head>
  <body>
  $body$
  </body>
</html>
"

tmpfile_pattern <- "rmarkdown-str"

# return a string as a tempfile
as_tmpfile <- function(str) {
  if (length(str) == 0) return()
  f <- tempfile(tmpfile_pattern, fileext = ".html")
  write_utf8(str, f)
  f
}

# temp files created by as_tmpfile() cannot be immediately removed because they
# are needed later by the pandoc conversion; we have to clean up the temp files
# that have the pattern specified in `tmpfile_pattern` when render() exits
clean_tmpfiles <- function() {
  unlink(list.files(
    tempdir(), sprintf("^%s[0-9a-f]+[.]html$", tmpfile_pattern), full.names = TRUE
  ))
}

read_utf8 <- function(file) {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = 'UTF-8'); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

write_utf8 <- function(text, con, ...) {
  opts <- options(encoding = "native.enc"); on.exit(options(opts), add = TRUE)
  writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
}

get_knitr_hook_list <- function(hook_names = NULL) {
  if (is.null(hook_names))
    hook_names <- c("knit_hooks", "opts_chunk", "opts_hooks", "opts_knit")
  knitr_ns <- asNamespace("knitr")
  hook_list <- lapply(hook_names, function(hook_name) {
    hooks <- get(hook_name, envir = knitr_ns, inherits = FALSE)
    hooks$get()
  })
  names(hook_list) <- hook_names
  hook_list
}

set_knitr_hook_list <- function(hook_list) {
  knitr_ns <- asNamespace("knitr")
  enumerate(hook_list, function(hook_name, hook_value) {
    hook <- get(hook_name, envir = knitr_ns, inherits = FALSE)
    hook$set(hook_value)
  })
}

enumerate <- function(data, f, ...) {
  lapply(seq_along(data), function(i) {
    f(names(data)[[i]], data[[i]], ...)
  })
}

note_page_knitr_options <- function() {

  # save original hooks (restore after we've stored requisite
  # hooks in our output format)
  saved_hooks <- get_knitr_hook_list()
  on.exit(set_knitr_hook_list(saved_hooks), add = TRUE)

  # use 'render_markdown()' to get default hooks
  knitr::render_markdown()

  # store original hooks and annotate in format
  orig_knit_hooks <- knitr::knit_hooks$get()

  # generic hooks for knitr output
  hook_names <- c("source", "chunk", "plot", "text", "output",
                 "warning", "error", "message", "error")

  # meta_hooks <- list(
    # source  = html_notebook_text_hook,
    # output  = html_notebook_text_hook,
    # warning = html_notebook_text_hook,
    # message = html_notebook_text_hook,
    # error   = html_notebook_text_hook
  # )

  knit_hooks = list()

  # knit_hooks <- lapply(hook_names, function(hook_name) {
  #   html_notebook_annotated_knitr_hook(hook_name,
  #                                      orig_knit_hooks[[hook_name]],
  #                                      meta_hooks[[hook_name]])
  # })
  # names(knit_hooks) <- hook_names

  # use a custom 'chunk' hook that ensures that html comments
  # do not get indented
  old_chunk <- knitr::knit_hooks$get('chunk')
  knit_hooks$chunk <- function(x, options) {
    x <- old_chunk(x,options)
    # Remove trailing spaces
    x = gsub(' +\n','\n',x)
    # Remove trailing newlines
    x = gsub("\n$","",x)
    # Turn multiple newlines into br
    x = gsub("\n+","<br/>",x,fixed=T)
    # Turn double space into non-breaking space
    x = gsub('  ', " nbspnbsp ",x)
    return( paste(x,"\n",sep=''))
  }

  old_plot <- knitr::knit_hooks$get('plot')
  knit_hooks$plot <- function(x,options) {
      x = old_plot(x,options)
      if (!is.null(options[['skip_pdf']])) {
        return(paste(x,sep=''))
      }
      paste(x, '<object type="application/pdf" data="file://',
               knitr::fig_path('.pdf',options),
               '" data-attachment="',
               options$label,
               '-',
               options$fig.cur %n% 1L,
               '.pdf"></object>\n'
            ,sep='')
  }

  old_output <- knitr::knit_hooks$get('output')
  knit_hooks$output <- function(x,options) {
    x = stringr::str_replace_all(x,'\n','\nNEWLINE\n')
    old_output(x,options)
  }

  old_source <- knitr::knit_hooks$get('source')
  knit_hooks$source <- function(x,options) {
    x = gsub('[\n\r]','\nNEWLINECODE\n',x)
    x <- old_source(unlist(Map(function(x) c(x,'NEWLINECODE') ,as.list(x)))[1:(2*length(x)-1)],options)
    x = gsub(' +\n','\n',x)
    paste(gsub('  ', "__TWOSPACE__ ", x),'\n',sep='')
  }

  opts_chunk <- list(
    render = note_render_hook,
    data.path= 'knoter_data/'
  )

  # return as knitr options
  rmarkdown::knitr_options(knit_hooks = knit_hooks, opts_chunk = opts_chunk)
}

knit_print_excel <- function(dflist,options,filename='Woorkbook1.xlsx') {
  autofilename = file.path(options$data.path, paste( options$label,'-',filename,sep=''))
  outfile = write_workbook(dflist,autofilename)
  res = paste( '<object type="application/vnd.ms-excel" data="file://',
         outfile,
         '" data-attachment="',
         basename(outfile),
         '"></object>',sep='')
  return (res)
}

knit_print.excel.workbook <- function(x,options) {
  if (! is.null(attributes(x)$filename)) {
    res = knit_print_excel(x,options,filename=attributes(x)$filename)
  } else {
    res = knit_print_excel(x,options,filename=paste('Workbook',workbook_counter(),'.xlsx',sep=''))
  }
  knitr::asis_output(res)
}

xtable_print = function(dataframe) {
  print(xtable::xtable(dataframe),type='html',print.results=F)
}

knit_print.multipage <- function(x,options) {
  paths = write_multipage_plot(x,thumbnail=attributes(x)$thumbnail,pages=attributes(x)$count,options=options)
  html_fragment = paste('<object type="application/pdf" data="file://',
     paths$multi,
     '" data-attachment="',
     options$label,
     '.pdf"></object>\n'
  ,sep='')
  options$skip_pdf = TRUE
  knitr::asis_output(c( knitr::knit_hooks$get('plot')(paths$thumbnail,options), html_fragment ))
}

knit_print.data.frame <- function(x,options) {
  if (!is.null(options[['excel']])) {
    workbook=excel.workbook(data=x,name=paste('Workbook',workbook_counter(),'.xlsx',sep=''))
    res = knit_print_excel(workbook,options,filename=attributes(workbook)$filename)
  } else {
    res = paste(c('', '', xtable_print(x)), collapse = '\n')
  }
  knitr::asis_output(res)
}

note_render_hook <- function(x,options,...) {

  can_excel = requireNamespace('writexl',quietly=T)

  if (! can_excel ) {
    options[['excel']] = NULL
  }

  knitr::knit_print(x,options)

}


pre_knit <- function(input, ...) {

  set_knit_hooks_rmarkdown()

  set_knit_opts_rmarkdown()

  set_knit_opts_hooks_rmarkdown()

}

# Set any extra arguments to pandoc here
post_knit <- function(metadata, input_file, runtime, ...) {

}

on_exit <- function() {

}

pre_processor <- function(metadata, input_file, runtime, knit_meta,
                            files_dir, output_dir) {

}

intermediates_generator <- function(original_input, intermediates_dir) {

}

post_processor <- function(metadata, input_file, output_file, clean, verbose, notebook=NULL,section=NULL,sharepoint=NULL,batch.chunks=10 ) {

  write_utf8(post_html_fixes(read_utf8(output_file)),output_file)

  write_utf8(rewrite_as_onenote_html(output_file),output_file)

  perform_html_upload(output_file,notebook,section,sharepoint,batch.chunks)

  output_file
}