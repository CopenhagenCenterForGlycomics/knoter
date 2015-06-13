context("knitting markdown")

test_that("knoter can detect markdown", {
  test_result = 'markdown-simple.html'
  test_expected = readChar(test_result, file.info(test_result)$size)
  expect_equal( knoter::knit( text='# Header \n Inline R `r 1+1`\n',append.meta.created=F ), test_expected )
})