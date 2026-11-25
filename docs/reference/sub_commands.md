# Setting sub commands

Dispatches to an external R script based on the first command-line
argument. The remaining command-line arguments are passed to the
selected script.

## Usage

``` r
sub_commands(..., help_head = NULL, help_foot = NULL, argv_str = NULL)
```

## Arguments

- ...:

  Specification of commands.

- help_head:

  Head of the help message.

- help_foot:

  Foot of the help message.

- argv_str:

  A string containing command-line arguments. Only for testing purposes.

## Value

Invisible `NULL`.

## Details

The specification can be one of the following:

1.  A vector with length a multiple of 2:
    `(script path, description, ...)`.

2.  A vector with length a multiple of 3:
    `(command, script path, description, ...)`.

3.  A single string template where commands are marked by the template
    tag (default `<CODE>`).

For more details, please refer to the vignette.

## Examples

``` r
d = tempfile("argitect_sub_")
dir.create(d)
writeLines(c(
    'library(argitect)',
    'argitect("x=i", "X.")',
    'cat("x =", x, "\n")'
), file.path(d, "cmd1.R"))
writeLines(c(
    'library(argitect)',
    'argitect("y=s", "Y.")',
    'cat("y =", y, "\n")'
), file.path(d, "cmd2.R"))
sub_commands(
    file.path(d, "cmd1.R"), "First command",
    file.path(d, "cmd2.R"), "Second command",
    argv_str = "cmd1 --x 5")
#> x = 5 
```
