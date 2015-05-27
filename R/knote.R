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
	files = read_html(knitted,asText=!is.null(text_input),fragment.only=F)
	if ( ! is.null(notebook) && ! is.null(section)) {
		upload_files(notebook,section,files)		
	}

	NULL
}

#' Knit a Rhtml or Rmd fragment, and append to a OneNote page
#'
#' Take an Rhtml or Rmd document, extract out the contents
#' (usually in the body tag), and append a page in OneNote
#' that has the given title.
#'
#' Arguments are the same as found for
#' \code{\link[knitr]{knit}}.
#'
#' @param notebook Name of notebook to find the target page in
#' @param section Name of section to find the target page in
#' @param page Title of page to append the markup to
#' @seealso \code{\link[knitr]{knit}}
#' @export
knote.append <- function(...,notebook,section,page) {
	arguments = list(...)
	text_input = arguments[['text']]

	knitted = knoter::knit(...)
	files = read_html(knitted,asText=!is.null(text_input),fragment.only=T)
	if ( ! is.null(notebook) && ! is.null(section) && ! is.null(page) ) {
		patch_page(notebook,section,page,files)
	}

	NULL
}
