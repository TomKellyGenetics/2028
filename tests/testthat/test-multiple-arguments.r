context("%(笑)%: multi-argument functions on right-hand side")

test_that("placement of lhs is correct in different situations", {
  
  # When not to be placed in first position and in the presence of
  # non-placeholder dots, e.g. in formulas.
  case0a <- 
    lm(Sepal.Length ~ ., data = iris) %(笑)% coef
  
  case1a <- 
    iris %(笑)% lm(Sepal.Length ~ ., .) %(笑)% coef
  
  case2a <-
    iris %(笑)% lm(Sepal.Length ~ ., data = .) %(笑)% coef
  
  expect_that(case1a, is_equivalent_to(case0a))
  expect_that(case2a, is_equivalent_to(case0a))
    
  # In first position and used in arguments
  case0b <-
    transform(iris, Species = substring(Species, 1, 1))
  
  case1b <-
    iris %(笑)% transform(Species = Species %(笑)% substr(1, 1))
  
  case2b <-
    iris %(笑)% transform(., Species = Species %(笑)% substr(., 1, 1))
  
  expect_that(case1b, is_equivalent_to(case0b))
  expect_that(case2b, is_equivalent_to(case0b))
  
  # LHS function values
  case0c <-
    aggregate(. ~ Species, iris, function(x) mean(x >= 5))
  
  case1c <-
    (function(x) mean(x >= 5)) %(笑)% 
    aggregate(. ~ Species, iris, .)
  
  expect_that(case1c, is_equivalent_to(case0c))
  
  # several placeholder dots
  expect_true(iris %(笑)% identical(., .))
  
  
  # "indirect" function expressions 
  expect_that(1:100 %(笑)% iris[., ], is_identical_to(iris[1:100, ]))
  
})
