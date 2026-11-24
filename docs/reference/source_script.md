# Source an R script with command-line arguments

Sources an R script while making command-line arguments available to it.
This is convenient for running external R scripts with different
command-line arguments from within an interactive R session.

## Usage

``` r
source_script(file, ..., argv_str = NULL)
```

## Arguments

- file:

  The R script to source.

- ...:

  Passed to [`base::source()`](https://rdrr.io/r/base/source.html).

- argv_str:

  The command-line arguments made available to the script.

## Value

The value returned by
[`base::source()`](https://rdrr.io/r/base/source.html), invisibly.

## Examples

``` r
f = tempfile(fileext = ".R")
writeLines(c(
    'library(argitect)',
    'argitect("count=i", "Number of items.")',
    'cat("count =", count, "\\n")'
), f)
source_script(f, argv_str = "--count 3")
#> count = 3 
```
