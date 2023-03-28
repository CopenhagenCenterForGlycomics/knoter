perform_html_upload <- function(html_file,notebook=NULL,section=NULL,sharepoint=NULL,batch.chunks=10) {
  files = read_html_for_upload(html_file,asText=F,fragment.only=F,batch.chunks=batch.chunks)

  # rmarkdown::render('foo.Rmd',output_file='foo.html',output_format=note_page(notebook=utils::URLdecode("Hiren%20Jitendra%20@%20K%C3%B8benhavns%20Universitet"),section='Test'))
  # touchid-envchain vault rstudio

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

}