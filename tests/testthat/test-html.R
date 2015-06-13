context("knitting html")

test_that("knoter can convert html", {
    test_result = 'html-simple_output.html'
    test_expected = readChar(test_result, file.info(test_result)$size)
    expect_equal( knoter::knit( 'html-simple.Rhtml' ,append.meta.created=F ), test_expected )    
})

test_that("knoter can knit to an output file", {
    knoter::knit( 'html-simple.Rhtml',output='html-simple.html',append.meta.created=F )
    test_result = 'html-simple_output.html'
    test_expected = readChar(test_result, file.info(test_result)$size)
    test_output = readChar('html-simple.html', file.info(test_result)$size)
    expect_equal( test_output , test_expected )    
})