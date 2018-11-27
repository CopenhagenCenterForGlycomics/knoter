#' Knit a Rhtml or Rmd file to a OneNote page
#'
#' Take an Rhtml or Rmd document and turn it
#' into a page in OneNote.
#'
#' If you haven't granted access to this application
#' to write to your notebooks before, it will lead
#' you through the login process. Note that you will
#' need to log back in with this app every hour.
#'
#' For more advanced users, you can set up an offline
#' Oauth2.0 flow.
#'
#' Arguments for knitting are the same as found for
#' \code{\link[knoter]{knit}} as well as \code{\link[knitr]{knit}}
#'
#' @param ... Parameters passed on to \code{\link[knoter]{knit}}
#' @param notebook Name of notebook to upload page to
#' @param section Name of section to upload page to
#' @seealso \code{\link[knitr]{knit}}
#' @examples
#' \dontrun{
#' # Knit and send an R markdown file to OneNote
#' knoter::knote('example.Rmd',notebook='My Notebook',section='My section')
#'
#' # Send to OneNote, saving the output html in a file
#' knoter::knote('example.Rmd',output='output.html',notebook='My Notebook',section='My section')
#'
#' # Send markdown from a text string to OneNote
#' rmd_text = "## This is a heading ##\n\nThis is a paragraph with some inline R `r 1 + 1` that will be converted.\n\n"
#' knoter::knote(text=rmd_text,notebook='My Notebook', section='My section')
#' }
#' @export
knote <- function(...,notebook,section,sharepoint=NULL) {
	arguments = list(...)
	file_output = arguments[['output']]

	knitted = knoter::knit(...)
	files = read_html(knitted,asText=is.null(file_output),fragment.only=F)
	if ( ! is.null(notebook) && ! is.null(section)) {
		upload_files(notebook,section,files,sharepoint)
	}

	NULL
}

#' Knit a Rhtml or Rmd fragment, and append to a OneNote page
#'
#' Take an Rhtml or Rmd document, extract out the contents
#' (usually in the body tag), and append a page in OneNote
#' that has the given title.
#'
#' Arguments for knitting are the same as found for
#' \code{\link[knoter]{knit}} as well as \code{\link[knitr]{knit}}
#'
#' @param ... Parameters passed on to \code{\link[knoter]{knit}}
#' @param notebook Name of notebook to find the target page in
#' @param section Name of section to find the target page in
#' @param page Title of page to append the markup to
#' @seealso \code{\link[knitr]{knit}}
#' @examples
#' # Append a page with the contents of a markdown
#' \dontrun{
#' knoter::knote.append('example.Rmd',notebook='My Notebook',section='My section',page='My first page')
#'
#' # Send markdown from a text string to OneNote
#' rmd_text = "## This is a heading ##\n\nThis is a paragraph with some inline R `r 1 + 1` that will be converted.\n\n"
#' knoter::knote.append(text=rmd_text,notebook='My Notebook', section='My section',page='My first page')
#' }
#' @export
knote.append <- function(...,notebook,section,page,sharepoint=NULL) {
	arguments = list(...)
	file_output = arguments[['output']]

	knitted = knoter::knit(...)
	files = read_html(knitted,asText=is.null(file_output),fragment.only=T)
	if ( ! is.null(notebook) && ! is.null(section) && ! is.null(page) ) {
		patch_page(notebook,section,page,files,sharepoint)
	}

	NULL
}
