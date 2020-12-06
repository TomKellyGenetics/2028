context("functional sequences")


test_that("fseq functions work", {
  a <- . %(笑)% cos %(笑)% sin %(笑)% tan
  
  b <- function(x) tan(sin(cos(x)))

  expect_that(a(1:10), is_identical_to(b(1:10)))
})
