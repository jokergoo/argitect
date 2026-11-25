# Dispatching to sub-commands

    ## Warning: package 'knitr' was built under R version 4.6.1

------------------------------------------------------------------------

## A quick example

When a script gets complicated, it would be better to split it into
several sub-scripts where each is focused on a specific task. Here
[`sub_commands()`](../reference/sub_commands.md) supports to dispatch
command-line arguments to sub-scripts.

The following code gives you a quick example of how the sub commands are
set. In the function [`sub_commands()`](../reference/sub_commands.md),
the name of the sub-command, the sub-script and the description of the
sub-command are organized as a 3-tuple.

The following code is put in the script called `main.R` (parent script)
which contains dispatches of sub-commands.

``` r

library(argitect)

sub_commands(
    "sub1", "sub1.R", "Description of sub command 1.",
    "sub2", "sub2.R", "Description of sub command 2."
)
```

Code in `sub1.R`:

``` r

library(argitect)

argitect("foo=i", "Set value for foo.")

cat("foo is ", foo, "\n")
```

And code in `sub2.R`:

``` r

library(argitect)

argitect("bar=i", "Set value for bar")

cat("bar is ", bar, "\n")
```

Calling `main.R` with no argument prints the list of sub-commands it
supports.

    ~\> Rscript main.R

    Usage: Rscript main.R [command] [options]

    Commands:
      sub1    Description of sub command 1.
      sub2    Description of sub command 2.

When a sub-command is called, the corresponding R script is invoked. All
command-line arguments after the sub-command `sub1` go to the correspond
R script `sub1.R`. Internally, it executes `Rscript sub1.R --help`.

    ~\> Rscript main.R sub1 --help

    Usage: Rscript main.R sub1 [options]

    Options:
      --foo, -f integer
        Set value for foo. 
      --help, -h
        Print help message and exit. 
      --version, -v
        Print version information and exit. 

Command-line arguments for `sub1.R` can be directly put after the
sub-command `sub1`, which is internally equivalent to
`Rscript sub1.R --foo 10`.

    ~\> Rscript main.R sub1 --foo 10

    foo is 10 

A [shebang](https://en.wikipedia.org/wiki/Shebang_(Unix)) line can be
added to the first line of the parent R script, then `Rscript` is not
necessary to be explicitely called. Also `.R` suffix of the parent
script can be removed to pretend as a linux utility program.

``` r

#!/usr/bin/env Rscript
library(argitect)

sub_commands(
    "sub1", "sub1.R", "This is sub-command 1.",
    "sub2", "sub2.R", "This is sub-command 2."
)
```

Make sure `main` is executable.

    ./main sub1 --foo 1

If the directory of script `main` is put into `PATH` and all sub-command
scripts are accessible from `main`, then

    main sub1 --foo 1

## The specification

Besides the 3-tuple specification demonstrated already, sub-commands can
also be specified in 2-tuples, where the first element is the sub-script
and the second element is the description of the sub-command. In this
scenario, the basename of the sub-script removing `.R`/`.r` suffix is
taken as the name of the sub-command.

``` r

sub_commands(
    "sub1.R", "This is sub-command 1.",
    "sub2.R", "This is sub-command 2."
)
```

With 3-tuple specification, names of the sub-commands can be
self-defined.

``` r

sub_commands(
    "one", "sub1.R", "This is sub-command 1.",
    "two", "sub2.R", "This is sub-command 2."
)
```

    Usage: Rscript main.R [command] [options]

    Commands:
      one    This is sub-command 1.
      two    This is sub-command 2.

As all parameters are specified as a long vector in
[`sub_commands()`](../reference/sub_commands.md), whether it is a
3-tuple format or a 2-tuple format is automatically determined.

## Sections

A separator line (a name consisting only of `-`, `=`, `+`, `#` or `%`)
starts a new section, just like in
[`argitect()`](../reference/argitect.md). The section title is the
corresponding description. With the 3-tuple form:

``` r

sub_commands(
    "----", "----", "Section one",
    "sub1", "sub1.R", "This is sub-command 1.",

    "----", "Section two", "----", 
    "sub2", "sub2.R", "This is sub-command 2."
)
```

    Usage: Rscript main.R [command] [options]

    Section one
      sub1    This is sub-command 1.

    Section two
      sub2    This is sub-command 2.

In the line of the section name, it does not matter whether the section
name is in the third position or the second position.

And with the 2-tuple form:

``` r

sub_commands(
    "----", "Section one",
    "sub1.R", "This is sub-command 1.",

    "----", "Section two",
    "sub2.R", "This is sub-command 2."
)
```

    Usage: Rscript main.R [command] [options]

    Section one
      sub1    This is sub-command 1.

    Section two
      sub2    This is sub-command 2.

## Setting help head and foot

`help_head` adds text before the help message and `help_foot` adds text
after the help message.

``` r

sub_commands(
    help_head = "This is the head of the help message.",

    "sub1", "sub1.R", "This is sub-command 1.",
    "sub2", "sub2.R", "This is sub-command 2.",

    help_foot = "This is the foot of the help message."
)
```

    This is the head of the help message.

    Usage: Rscript main.R [command] [options]

    Commands:
      sub1    This is sub-command 1.
      sub2    This is sub-command 2.

    This is the foot of the help message.

## Template

A single string is treated as a template, where each command is marked
by the `template_tag` (default `<CODE>`). The code part can be either
`script.R` (the command name is derived from the file name) or
`command=script.R`:

``` r

sub_commands(
"This is the head of the help message.

Usage: Rscript main.R [command] [options]

Commands:
  <sub1.R> This is sub-command 1.
  <two=sub2.R> This is sub-command 2.

This is the foot of the help message.
")
```

    This is the head of the help message. 
     
    Usage: Rscript main.R [command] [options] 
     
    Commands: 
      sub1    This is sub-command 1.
      two     This is sub-command 2.
     
    This is the foot of the help message. 

The same as in [`argitect()`](../reference/argitect.md), here the
template also allows `{{...}}` to contain executable code to interpolate
and to insert into the template.

``` r

sub1_script = system.file("extdata", "sub1.R", package = "argitect")
sub2_script = system.file("extdata", "sub2.R", package = "argitect")
sub_commands(
"This is the head of the help message.

Usage: Rscript main.R [command] [options]

{{ cli_h2('Section') }}
  <sub1={{ sub1_script }}> This is sub-command 1.
  <sub2={{ sub2_script }}> This is sub-command 2.

This is the foot of the help message.

Questions, please contact {{ col_magenta(style_underline(style_italic('your.name@email'))) }}
")
```

    This is the head of the help message. 
     
    Usage: Rscript main.R [command] [options] 
     
    ── Section ── 
      sub1    This is sub-command 1.
      sub2    This is sub-command 2.
     
    This is the foot of the help message. 
     
    Questions, please contact your.name@email 

## –-help and –-version

`--help` on the parent script is the same as giving no argument, which
prints the list of sub-commands. `--version` prints the `VERSION`
variable from the calling environment. Both `-h` and `-v` are accepted
as well.

``` r

VERSION = "1.0.0"
sub_commands(
    "sub1.R", "This is sub command 1.",
    "sub2.R", "This is sub command 2."
)
```

    ~\> Rscript main.R --version
    1.0.0

## Script existence

The sub-scripts must exist. If a specified sub-script path does not
exist, [`sub_commands()`](../reference/sub_commands.md) raises an error:

``` r

sub_commands(
    "sub1", "non_existing_dir/sub1.R", "This is sub command 1."
)
```

    Error:
    ! Script path(s) do not exist: non_existing_dir/sub1.R.

## Session info

``` r

sessionInfo()
```

    R version 4.6.0 (2026-04-24)
    Platform: aarch64-apple-darwin23
    Running under: macOS 27.2.1

    Matrix products: default
    BLAS:   /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRblas.0.dylib 
    LAPACK: /Library/Frameworks/R.framework/Versions/4.6/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1

    locale:
    [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

    time zone: Asia/Shanghai
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
    [1] argitect_0.1.0 knitr_1.52    

    loaded via a namespace (and not attached):
     [1] vctrs_0.7.3         cli_3.6.6           rlang_1.3.0        
     [4] xfun_0.62           otel_0.3.0          textshaping_1.0.5  
     [7] jsonlite_2.0.0      glue_1.8.1          htmltools_0.5.9    
    [10] GlobalOptions_0.1.4 ragg_1.5.2          sass_0.4.10        
    [13] rmarkdown_2.32      evaluate_1.0.5      jquerylib_0.1.4    
    [16] fastmap_1.2.0       yaml_2.3.12         lifecycle_1.0.5    
    [19] compiler_4.6.0      fs_2.1.0            htmlwidgets_1.6.4  
    [22] systemfonts_1.3.2   digest_0.6.39       R6_2.6.1           
    [25] pillar_1.11.1       bslib_0.12.0        tools_4.6.0        
    [28] pkgdown_2.2.1       cachem_1.1.0        desc_1.4.3         
