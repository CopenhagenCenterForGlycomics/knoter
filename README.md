# Knoter

Knoter is an accessory library to the venerable [Knitr](https://github.com/yihui/knitr) that allows you to take your reports generated in R, and send them in to [OneNote](http://www.onenote.com/). Not only can the reports be synced up to OneNote, but any accessory data, such as the original figures in PDF format, as well as data frames wrapped into an excel format, can all be sent along for the ride. This gives all the data necessary for turning an analysis into a set of figures for publication, or data sheets to go with the publication.

### Installation

The development version of the tool can be installed using devtools

```R
    devtools::install_github('CopenhagenCenterForGlycomics/knoter')
```

### Basic Usage

  - Create your reports as usual using RHTML (`Rhtml`) or R Markdown (`Rmd`).
  - Find the name of the OneNote Notebook and Section you want to create a page in

A single command will upload your report to OneNote

```R
    knoter::knote('example.Rhtml',notebook='My Notebook',section='My Section')
```

Knoter will log in to OneNote for you, asking for permissions to write to your OneNote notebooks.

Or, if you'd like to append some Markdown to a page

```R
    my_markdown = "## Header\n\n This is some inline R `r 1+1`.\n"
    knoter::knote.append(text=my_markdown,notebook='My Notebook',section='My Section',page='My Page')
```

This will append a header, and a paragraph with some inline R to the page `My Page`.

### Attachments

When sharing reports for datasets there are two important artefacts that often have a life beyond the
original report that they are embedded in. Tabular data and generated figures are often either embedded
within other things (such as manuscripts). To make this easier, there are three functions that can be
used within a code block, `excel`, `table` and `multipage`

#### excel

If you want to attach an Excel workbook with sets of tabular data to your OneNote, you can use this function
to write the Excel workbook.

```R
	excel(Cars_sheet=cars,CO2_sheet=CO2)
```

#### table

If you have tabular data that you would like OneNote to display properly, you can use the table function, which
will add the correct formatting to show a table in OneNote.

```R
	table(cars)
```

#### multipage

If you have a lot of plots that you want to generate as PDF and attach to OneNote, but don't actually want to
have an image inserted for every one of them, you can use this function to tell knoter to write the multipage PDF
and then only insert an image for the first plot

```R
	# Only plot1 will appear in the output
	multipage(list(plot1,plot2,plot3))
```


### SharePoint libraries

If you have your notebooks sitting on a SharePoint library, you can ask knoter to write to that location with the
`sharepoint` parameter.

```R
	knoter::knote('example.Rhtml',notebook='My SharePoint Notebook',section='My Section',sharepoint="https://hostname.sharepoint.com/sites/MySite")
```

### Limitations

OneNote only supports a subset of HTML for uploading into their notebooks. As long as you stick to a relatively simple subset of tags (see: [Tag support](#Tags)), the upload should work. Inline style attributes are likely to be removed, so don't count on the availability of styling.


### <a name="Tags"></a> Tag support

The full list of supported tags can be found at the [OneNote API description](https://msdn.microsoft.com/en-us/library/office/dn575442.aspx)

|  Supported Tag                       |
|:-------------------------------------|
| `<table>` , `<tr>` , `<td>`          |
| `<div>`                              |
| `<ul>` , `<ol>` , `<li>`             |
| `<img>`                              |
| `<a>`                                |
| `<br/>`                              |
| `<h1>` to `<h6>`                     |
| `<b>`, `<em>`, `<strong>`, `<i>`     |
| `<u>`, `<strike>`, `<del>`           |
| `<sup>`, `<sub>`, `<cite>`, `<font>` |


### Licensing

LGPL