note_page <- function() {

  args <- c("--template", as_tmpfile(template),"--highlight-style","pygments")

  rmarkdown::output_format(
    knitr = NULL,
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
    post_processor = post_processor
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

pre_knit <- function(input, ...) {

}

post_knit <- function(metadata, input_file, runtime, ...) {

}

on_exit <- function() {

}

pre_processor <- function(metadata, input_file, runtime, knit_meta,
                            files_dir, output_dir) {

}

intermediates_generator <- function(original_input, intermediates_dir) {

}

post_processor <- function(metadata, input_file, output_file, clean, verbose) {
  batch.chunks = 10
  files = read_html(output_file,asText=F,fragment.only=F,batch.chunks=batch.chunks)
  write_utf8(read_utf8(files[[1]]$path),file(output_file))
  output_file
}