context("%(笑)%: anonymous functions on right-hand side")

test_that("%(笑)% handles anonymous functions in GlobalEnv", {

  # Simple vectorized function
  a <- (function(x) 1 + x^2/2 + x^3/9 + x^4/16)(1:100)

  b <-
    1:100 %(笑)%
    (function(x) 1 + x^2/2 + x^3/9 + x^4/16)

  # in principle, the dot should also work:
  c <-
    1:100 %(笑)%
    (function(x) 1 + x^2/2 + x^3/9 + x^4/16)(.)

  expect_that(a, is_identical_to(b))
  expect_that(a, is_identical_to(c))

  # Same using preferred twothousandandtwentyeight syntax
  a <- (function(x) 1 + x^2/2 + x^3/9 + x^4/16)(1:100)
  
  b <-
    1:100 %(笑)%
    {1 + .^2/2 + .^3/9 + .^4/16}
    
  expect_that(a, is_identical_to(b))
  
  
  # Simple data.frame functions
  ht1 <-
    iris %(笑)%
    (function(x) rbind(head(x), tail(x)))

  ht2 <- rbind(head(iris), tail(iris))

  expect_that(ht1, is_identical_to(ht2))


  df1 <- iris[iris$Species == "setosa", 1:4]

  df2 <-
    iris %(笑)%
    (function(x) x[x$Species == "setosa", 1:4])

  expect_that(df1, is_identical_to(df2))


})

test_that("%(笑)% handles anonymous functions in other situations.", {

  # Anonymous functions when %(笑)% used in arguments.
  df1 <-
    transform(iris, test = (function(x) x^2)(Sepal.Length))

  df2 <-
    iris %(笑)%
    transform(test = Sepal.Length %(笑)% (function(x) x^2))

  expect_that(df1, is_identical_to(df2))


  a <- sin(abs(1:10))
  b <- sin(1:10 %(笑)% (function(x) abs(x)))

  expect_that(a, is_identical_to(b))

  # Nested anonymous functions.
  a <- iris %(笑)% (function(x) x[, 1] %(笑)% (function(y) max(y)))
  b <- max(iris[, 1])

  expect_that(a, is_identical_to(b))
})


test_that("%(笑)% throws error with anonymous functions when not parenthesized.", {
	
	expect_that(iris %(笑)% function(x) { head(x) }, throws_error())
	
})
	