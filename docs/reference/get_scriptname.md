# File name of the current script

Returns the file name of the currently running R script.

## Usage

``` r
get_scriptname()
```

## Value

A character string, or `NULL` if the R script is not run from the
command line.

## Examples

``` r
# NULL when the script is not run from the command line
get_scriptname()
#> NULL
```
