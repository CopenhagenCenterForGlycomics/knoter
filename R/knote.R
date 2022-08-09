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
#' @param section Name of section to upload page to, if the section does not exist, it is created.
#' @param sharepoint Optional URL for SharePoint site to upload to
#' @param auto.archive Flag for automatically demoting pages when a new page with the same title is uploaded
#' @param batch.chunks Batch chunks into groups of this size for upload
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
knote <- function(...,notebook,section,sharepoint=NULL,auto.archive=F,batch.chunks=10) {
	arguments = list(...)
	file_output = arguments[['output']]

	knitted = knoter::knit(...)
	files = read_html(knitted,asText=is.null(file_output),fragment.only=F,batch.chunks=batch.chunks)

	added = perform_upload(files,notebook,section,sharepoint,auto.archive)
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
	NULL
}

#' Upload a pre-generated HTML file to OneNote
#'
#' No validation is performed on the input HTML, so the upload
#' may fail due to the formatting of the HTML.
#' @param file Path to HTML file to read in
#' @param notebook Name of notebook to upload page to
#' @param section Name of section to upload page to, if the section does not exist, it is created
#' @param sharepoint Optional URL for SharePoint site to upload to
#' @param auto.archive Flag for automatically demoting pages when a new page with the same title is uploaded
#' @param batch.chunks Batch chunks into groups of this size for upload
#' @export
knote.html <- function(file,notebook,section,sharepoint=NULL,auto.archive=F,batch.chunks=10) {
	files = read_html(file,asText=F,fragment.only=F,batch.chunks=batch.chunks)
	added = perform_upload(files,notebook,section,sharepoint,auto.archive)

	if ('extrablocks' %in% names(attributes(files))) {
		if ( ! is.null(sharepoint) ) {
			enable_sharepoint(sharepoint)
		}

		message('Waiting for Page to appear')
		Sys.sleep(15)

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
	NULL
}

perform_upload <- function(files,notebook,section,sharepoint,auto.archive) {
	if ( ! is.null(sharepoint) ) {
		enable_sharepoint(sharepoint)
	}

	if ( ! is.null(notebook) && ! is.null(section)) {
		new_page = upload_files(notebook,section,files)
		if ( auto.archive && ! is.null(new_page)) {
			sibling_pages = Filter(function(page) {  page$id != new_page$id & page$title == new_page$title },list_page_ids(notebook,section))
			sibling_pages = sapply(sibling_pages, function(page) { page$id })
			set_page_level(sibling_pages,level=1)
		}
	}

	use_default_endpoint()

	new_page
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
#' @param section Name of section to find the target page in. If the section does not exist, it is created
#' @param page Title of page to append the markup to
#' @param sharepoint Optional URL for SharePoint site to upload to
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

	if ( ! is.null(sharepoint) ) {
		enable_sharepoint(sharepoint)
	}

	files = read_html(knitted,asText=is.null(file_output),fragment.only=T)
	if ( ! is.null(notebook) && ! is.null(section) && ! is.null(page) ) {
		patch_page(notebook,section,page,files)
	}

	use_default_endpoint()

	NULL
}
