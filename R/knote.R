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
	output_file = arguments[['output']]
	
	knitted = knoter::knit(...)
	if (! is.null(output_file) ) {
		knitted = output_file
	}

	files = read_html(knitted,asText=is.null(output_file))

	if ( ! is.null(notebook) && ! is.null(section)) {
		upload_files(notebook,section,files)		
	}

	NULL
}