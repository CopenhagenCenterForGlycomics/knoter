note_page <- function(notebook=NULL,section=NULL,sharepoint=NULL,batch.chunks=10) {

  args <- c("--template", as_tmpfile(template),"--highlight-style","pygments")

  rmarkdown::output_format(
    knitr = note_page_knitr_options(),
    pandoc = rmarkdown::pandoc_options(
      to = "html", from = NULL, args = args
    ),
    keep_md = FALSE,
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
    <style>$highlighting-css$</style
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
    if (grepl("<excel>",paste(x,collapse=''),fixed=T))  {
      components<-extract_source_excel_block(c('<root>',x,'</root>'))
      excel_elements<- c( components$source_node, sapply(components$excel_node, function(file) {
        paste( '<object type="application/vnd.ms-excel" data="file://',
               file,
               '" data-attachment="',
               basename(file),
               '"></object>',sep='')
      },USE.NAMES=F,simplify=F))
      x = stringr::str_replace_all(x,'<excel>[^<]*</excel>','')
      block <- paste(c(x,unlist(excel_elements)),sep='',collapse='')
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
    x = gsub('  ', " nbspnbsp ",x)
    return( paste(x,"\n",sep=''))
  }

  old_plot <- knitr::knit_hooks$get('plot')
  knit_hooks$plot <- function(x,options) {
      x = old_plot(x,options)
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
    x = stringr::str_replace_all(x,'\n','NEWLINE\n')
    old_output(x,options)
  }


  # return as knitr options
  rmarkdown::knitr_options(knit_hooks = knit_hooks)
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
  browser()
  write_utf8(post_html_fixes(read_utf8(output_file)),file(output_file))

  files = read_html(output_file,asText=F,fragment.only=F,batch.chunks=batch.chunks)


  if (!is.null(notebook)) {
    added = perform_upload(files,notebook,section,sharepoint,auto.archive=FALSE)
    if ('extrablocks' %in% names(attributes(files))) {
      if ( ! is.null(sharepoint) ) {
        enable_sharepoint(sharepoint)
      }

      message('Waiting for Page to appear')
      Sys.sleep(10)

      pb = NULL

      if (requireNamespace('progress',quietly=T)) {
        pb = progress::progress_bar$new(total=length(attributes(files)$extrablocks))
      }

      for (extrablock in attributes(files)$extrablocks) {
        patch_page_by_id(added$id,extrablock)
        if ( ! is.null(pb) ) {
          pb$tick()
        }
      }

      use_default_endpoint()
  }


  }

  files_paths = c( files[[1]]$path, sapply(attr(files,'extrablocks'), function(x) x$Presentation$path ))
  files_contents = paste(unlist(sapply( files_paths, function(x) read_utf8(x), simplify=F )),collapse="\n")
  write_utf8(files_contents,file(output_file))
  output_file
}