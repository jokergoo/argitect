# Get or set global options for `argitect()`

Get or set global options used by [`argitect()`](argitect.md). This
function is backed by
[`GlobalOptions::setGlobalOptions()`](https://rdrr.io/pkg/GlobalOptions/man/set_opt.html),
so options can be read and written with `argitect_options("name")`,
`argitect_options(name = value)` or `argitect_options$name`.

## Usage

``` r
argitect_options(
  ...,
  RESET = FALSE,
  READ.ONLY = NULL,
  LOCAL = FALSE,
  ADD = FALSE
)

argitect.options(
  ...,
  RESET = FALSE,
  READ.ONLY = NULL,
  LOCAL = FALSE,
  ADD = FALSE
)
```

## Arguments

- ...:

  Named options to set, or a single option name to get.

- RESET:

  Whether to reset all options to their default values.

- READ.ONLY:

  Passed to
  [`GlobalOptions::setGlobalOptions()`](https://rdrr.io/pkg/GlobalOptions/man/set_opt.html).

- LOCAL:

  Passed to
  [`GlobalOptions::setGlobalOptions()`](https://rdrr.io/pkg/GlobalOptions/man/set_opt.html).

- ADD:

  Passed to
  [`GlobalOptions::setGlobalOptions()`](https://rdrr.io/pkg/GlobalOptions/man/set_opt.html).

## Value

The current option value (when getting) or, invisibly, the full option
list (when setting).

## Details

Supported options:

- `config` A character vector of parser configuration flags (default
  `c("bundling", "no_ignore_case")`). The following values are
  recognized:

  - `"bundling"`: allow single-letter flags to be combined, so that
    `-abc` is the same as `-a -b -c` (this is the default behavior).

  - `"no_bundling"`: disable bundling; `-abc` is then treated as a
    single (unknown) option named `abc`.

  - `"ignore_case"`: match long option names case-insensitively, so that
    `--FOO` matches `--foo` (by default matching is case-sensitive).

  - `"no_ignore_case"`: keep case-sensitive matching; if both
    `"ignore_case"` and `"no_ignore_case"` are given, the latter wins.

  For example
  `argitect_options("config" = c("ignore_case", "no_bundling"))`.

- `template_tag` The tag identifying specifications in a template, in
  the format "`left_tag CODE right_tag`" (default `"<CODE>"`).

- `help_style` The style of the help message: `"one-column"` or
  `"two-column"`.

- `help_width` The width of the help message, as a vector of one or two
  numbers (default `c(90, 40)`). The first number is the total width;
  the one-column style uses only it. The second number is the width of
  the first (option) column used by the two-column style. Option names
  that are too long are wrapped, and continuation lines are aligned with
  the second option name.

- `value_sep` The separator used to split multiple values inside a
  single `name=value` pair for a vector sub-argument. The default
  `"auto"` automatically detects the separator among `","`, `";"`,
  `":"`, `"|"` and `"/"` by picking the one that satisfies the type and
  count constraints.

`argitect_options(...)` should be put before calling
[`argitect()`](argitect.md) or [`sub_commands()`](sub_commands.md).

## Examples

``` r
argitect_options("help_width")
#> [1] 90 40
old = argitect_options("help_width")
argitect_options("help_width" = 100)
argitect_options("help_width")
#> [1] 100  44
argitect_options(RESET = TRUE)
argitect_options
#>  Option       Value                   
#>  ------------:--------------------------
#>  config       bundling, no_ignore_case
#>  template_tag <CODE>                  
#>  help_style   one-column              
#>  help_width   90, 40                  
#>  value_sep    auto                    
```
