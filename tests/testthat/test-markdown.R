context("knitting markdown")

test_that("knoter can detect markdown", {
    test_result = 'markdown-simple_output.html'
    test_expected = readChar(test_result, file.info(test_result)$size)
    expect_equal( knoter::knit( text='# Header \n Inline R `r 1+1`\n',append.meta.created=F ), test_expected )
})

test_that("knoter can convert html", {
    test_result = 'html-simple_output.html'
    test_expected = readChar(test_result, file.info(test_result)$size)
    expect_equal( knoter::knit( 'html-simple.Rhtml' ,append.meta.created=F ), test_expected )    
})