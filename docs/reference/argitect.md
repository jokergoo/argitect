# Parse command-line arguments

A pure-R command-line argument parser that reimplements the
functionality of the Perl module `Getopt::Long` without any external
dependency, and additionally supports positional arguments.

## Usage

``` r
argitect(
  ...,
  help_head = NULL,
  help_foot = NULL,
  envir = parent.frame(),
  argv_str = NULL,
  explain = FALSE,
  help_style = argitect_options("help_style"),
  help_width = argitect_options("help_width"),
  value_sep = argitect_options("value_sep")
)
```

## Arguments

- ...:

  Argument specifications. They can be given as
  specification-description pairs, or a single text template. See
  Details.

- help_head:

  Head of the help message shown by `--help`.

- help_foot:

  Foot of the help message shown by `--help`.

- envir:

  Environment where default values are looked up and parsed values are
  exported. A list is also accepted and coerced to an environment.

- argv_str:

  A string containing command-line arguments. Only for testing purposes.

- explain:

  If `TRUE`, print an explanation of each argument specification (its
  format and simple examples) and return without parsing any
  command-line arguments.

- help_style:

  The style of the help message: `"one-column"` or `"two-column"`.

- help_width:

  The width of the help message. A vector of one or two numbers; the
  second number is only used by the two-column style. Defaults to
  `argitect_options("help_width")`.

- value_sep:

  The separator used to split multiple values inside a single
  `name=value` pair for a vector sub-argument. The default `"auto"`
  detects the separator among `","`, `";"`, `":"`, `"|"` and `"/"` by
  picking the one that satisfies the type and count constraints.

## Value

Invisibly, a list of the parsed argument objects. Parsed values are also
exported to `envir` as variables named after the first specification
name.

## Details

Each argument specification is `name[|alias...]=spec`. The value part
`spec` consists of an optional *mode* (`i` integer, `f` numeric, `s`
character, `l` logical) and an optional *type* (`$` scalar, `#`
positional, `@` vector, `%` hash), optionally followed by a bracketed
range or sub-argument list. The `i` mode also accepts extended integer
notation: `0x`/`0X` hexadecimal, `0b`/`0B` binary, `0`/`0o`/`0O` octal
(e.g. `0123`), and decimal.

Examples are:

- `"number=i"`: a single integer scalar.

- `"name=s"`: a single character scalar.

- `"length|size=i"`: an integer scalar with two names.

- `"tag=i@"`: an integer vector.

- `"tag=i@{2,}"`: an integer vector with at least two elements.

- `"param=s%"`: name=value pairs with character values.

- `"input=s#"`: one positional argument.

- `"files=s#{}"`: one or more positional arguments.

- `"verbose!"`: a logical option, negatable with `--no-verbose`.

If a variable with the first name already exists in `envir` (and is not
a function), it is used as the default value and the option becomes
optional. Logical options always default to `FALSE`.

For more details, please refer to the vignette.

### Configuration file

Besides giving every option on the command line, all of them can be put
in a JSON or YAML file and passed as the single command-line argument
`@json=conf.json` or `@yaml=conf.yaml`:

    Rscript foo.R @json=conf.json
    Rscript foo.R @yaml=conf.yaml

The file must contain an object whose keys are the argument names. `-`
or `stdin` instead of a file name reads it from standard input. The
`jsonlite` and `yaml` packages are needed, respectively.

### help and version

`--help` and `--version` are added automatically. Defining `help` or
`version` in the specification replaces the generated one. The version
string is taken from the `version` variable in `envir`, then from
`VERSION`.

## Examples

``` r
argitect(
    "count=i", "Number of items.", 
    "verbose!", "Print messages.",
    argv_str = "--count 3 --verbose"
)
count
#> [1] 3
verbose
#> [1] TRUE
```
