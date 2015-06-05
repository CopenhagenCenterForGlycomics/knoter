# Knoter

Knoter is an accessory library to the venerable [Knitr](https://github.com/yihui/knitr) that allows you to take your reports generated in R, and send them in to [OneNote](http://www.onenote.com/). Not only can the reports be synced up to OneNote, but any accessory data, such as the original figures in PDF format, as well as data frames wrapped into an excel format, can all be sent along for the ride. This gives all the data necessary for turning an analysis into a set of figures for publication, or data sheets to go with the publication.

### Installation

The development version of the tool can be installed using devtools

    devtools::install_github('hirenj/knoter')
    
### Basic Usage

  - Create your reports as usual using RHTML (`Rhtml`) or R Markdown (`Rmd`).
  - Find the name of the OneNote Notebook and Section you want to create a page in

A single command will upload your report to OneNote

    knoter::knote('example.Rhtml',notebook='My Notebook',section='My Section')

Knoter will log in to OneNote for you, asking for permissions to write to your OneNote notebooks.

Or, if you'd like to append some Markdown to a page

    my_markdown = "## Header\n\n This is some inline R `r 1+1`.\n"
    knoter::knote.append(text=my_markdown,notebook='My Notebook',section='My Section',page='My Page')

This will append a header, and a paragraph with some inline R to the page `My Page`.

### Limitations

OneNote only supports a subset of HTML for uploading into their notebooks. As long as you stick to a relatively simple subset of tags (e.g. `<table>`,`<div>`,`<ul>`,`<img>`), the upload should work. Inline style attributes are likely to be removed, so don't count on the availability of styling.

### Licensing

LGPL