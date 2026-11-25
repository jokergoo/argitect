# File or directory name of the current script

Returns the file or directory name of the currently running R script.

## Usage

``` r
get_scriptname()

get_scriptdir()
```

## Value

A character string, or `NULL` if the R script is not run from the
command line.

## Examples

``` r
# NULL when the script is not run from the command line
get_scriptname()
#> NULL
get_scriptdir()
#> NULL

f = tempfile(fileext = ".R")
writeLines(c(
    'library(argitect)',
    'cat(get_scriptname(), "\n")',
    'cat(get_scriptdir(),  "\n")'
), f)
system(paste("Rscript", f), intern = TRUE)
#> [1] "/private/var/folders/kk/3mz0fbm96576_s8wg9xr5fmr0000gn/T/RtmpccZDVK/file174069939221.R "
#> [2] "/private/var/folders/kk/3mz0fbm96576_s8wg9xr5fmr0000gn/T/RtmpccZDVK "                   
```
