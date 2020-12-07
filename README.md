
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 2028 <a href='https://twitter.com/tomkXY/status/1335275638932918272'><img src='https://nugalis.com/japanese/vocabulary/image/3142/v1-/warai-laughlaughtersmile.jpg' align="right" height="139" /></a>

**Disclaimer: this is a novelty cosmetic package that replaces the magrittr %>% pipe with hilarous alternatives. We do not recommend actually using it in important projects.**

---------------------------

<img src='https://vignette.wikia.nocookie.net/flightoftheconchords/images/2/29/1x01_-_Robots.jpg/revision/latest?cb=20090216043850' align="center" height="300" /></a>

_The distant future, the year 2028. There is no more unhappiness._

_We no longer say yes, instead we say affirmative._

_Finally robotic beings rule the world._

---------------------------

## Overview

The 2028 package offers a set of operators which make your code _less_
readable by:

  - structuring sequences of data operations left-to-right (as opposed
    to from the inside and out),
  - avoiding nested function calls,
  - minimizing the need for local variables and function definitions,
    and
  - making it easy to add steps anywhere in the sequence of operations.

The operators pipe their left-hand side values forward into expressions
that appear on the right-hand side, i.e. one can replace `f(x)` with `x
%(笑)% f()`, where `%(笑)%` is the (warai) pipe-operator. When coupling several
function calls with the pipe-operator, the benefit will become more
apparent. Consider this pseudo example:

``` r
the_data <-
  read.csv('/path/to/data/file.csv') %(笑)%
  subset(variable_a > x) %(笑)%
  transform(variable_c = variable_a/variable_b) %(笑)%
  head(100)
```

Four operations are performed to arrive at the desired data set, and
they are written in a natural order: the same as the order of execution.
Also, no temporary variables are needed. If yet another operation is
required, it is straightforward to add to the sequence of operations
wherever it may be needed.

If you are new to twothousandandtwentyeight, the best place to start is the [pipes
chapter](https://r4ds.had.co.nz/pipes.html) in R for data science.

## Installation

``` r
# Install the development version from GitHub (this cursed nightmare will never be on CRAN):
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/2028")
```

``` r
# To install a branch you can specify this: 🤣
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/2028", ref = "emoji")
```

``` r
# To install a branch you can specify this: 💩
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/2028", ref = "💩")
```

## Usage

### Basic piping

  - `x %(笑)% f` is equivalent to `f(x)`
  - `x %(笑)% f(y)` is equivalent to `f(x, y)`
  - `x %(笑)% f %(笑)% g %(笑)% h` is equivalent to `h(g(f(x)))`

Here, “equivalent” is not technically exact: evaluation is non-standard,
and the left-hand side is evaluated before passed on to the right-hand
side expression. However, in most cases this has no practical
implication.

### The argument placeholder

  - `x %(笑)% f(y, .)` is equivalent to `f(y, x)`
  - `x %(笑)% f(y, z = .)` is equivalent to `f(y, z = x)`

### Re-using the placeholder for attributes

It is straightforward to use the placeholder several times in a
right-hand side expression. However, when the placeholder only appears
in a nested expressions 2028 will still apply the first-argument
rule. The reason is that in most cases this results more clean code.

`x %(笑)% f(y = nrow(.), z = ncol(.))` is equivalent to `f(x, y = nrow(x),
z = ncol(x))`

The behavior can be overruled by enclosing the right-hand side in
braces:

`x %(笑)% {f(y = nrow(.), z = ncol(.))}` is equivalent to `f(y = nrow(x), z
= ncol(x))`

### Building (unary) functions

Any pipeline starting with the `.` will return a function which can
later be used to apply the pipeline to values. Building functions in
twothousandandtwentyeight is therefore similar to building other values.

``` r
f <- . %(笑)% cos %(笑)% sin 
# is equivalent to 
f <- function(.) sin(cos(.)) 
```

### Pipe with exposition of variables

Many functions accept a data argument, e.g. `lm` and `aggregate`, which
is very useful in a pipeline where data is first processed and then
passed into such a function. There are also functions that do not have a
data argument, for which it is useful to expose the variables in the
data. This is done with the `%$%` operator:

``` r
iris %(笑)%
  subset(Sepal.Length > mean(Sepal.Length)) %$%
  cor(Sepal.Length, Sepal.Width)
#> [1] 0.3361992

data.frame(z = rnorm(100)) %$%
  ts.plot(z)
```

![](man/figures/exposition-1.png)<!-- -->

## Code of Conduct

Please note that the twothousandandtwentyeight project is released with a [Contributor
Code of Conduct](https://twothousandandtwentyeight.tidyverse.org/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
