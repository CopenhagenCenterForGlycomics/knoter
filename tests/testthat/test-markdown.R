context("knitting markdown")

test_that("knoter can detect markdown", {
    test_result = 'markdown-simple_output.html'
    test_expected = readChar(test_result, file.info(test_result)$size)
    expect_equal( knoter::knit( text='# Header \n Inline R `r 1+1`\n',append.meta.created=F ), test_expected )
})

test_that("knoter can knit to an output file", {
    knoter::knit( 'markdown-simple.Rmd',output='markdown-simple.html',append.meta.created=F )
    test_result = 'markdown-simple_output.html'
    test_expected = readChar(test_result, file.info(test_result)$size)
    test_output = readChar('markdown-simple.html', file.info(test_result)$size)
    expect_equal( test_output , test_expected )    
})

test_that("Characters are escaped properly", {
    test_result = 'markdown-escape_output.html'
    test_expected = readChar(test_result, file.info(test_result)$size)
    expect_equal( knoter::knit( 'markdown-escape.Rmd',append.meta.created=F ) , test_expected )        
})