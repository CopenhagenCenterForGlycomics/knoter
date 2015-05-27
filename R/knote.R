#' Knit a Rhtml or Rmd file to a OneNote page
#'
#' Take an Rhtml or Rmd document and turn it
#' into a page in OneNote.
#'
#' Arguments are the same as found for
#' \code{\link[knitr]{knit}}.
#'
#' @param notebook Name of notebook to upload page to
#' @param section Name of section to upload page to
#' @seealso \code{\link[knitr]{knit}}
#' @export
knote <- function(...,notebook,section) {
	arguments = list(...)
	text_input = arguments[['text']]

	knitted = knoter::knit(...)
	files = read_html(knitted,asText=!is.null(text_input))
	if ( ! is.null(notebook) && ! is.null(section)) {
		upload_files(notebook,section,files)		
	}

	NULL
}