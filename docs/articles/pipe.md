# Reading from standard input

    ## Warning: package 'knitr' was built under R version 4.6.1

------------------------------------------------------------------------

Reading from standard input fromm upstream command is basically to
support special flags. In the following example, the argument `input`
expects a file or a flag representing standard input.

``` r

library(argitect)
argitect(
    "input=s", "input"
)

if(input %in% c("-", "stdin")) {
    cat(readLines(file("stdin")), sep = "\n")
} else {
    cat(readLines(input), sep = "\n")
}
```

Then from command-line, standard input can be set via `-` or `stdin`:

    ~\> echo hello | Rscript pipe.R --input -
    ~\> echo hello | Rscript pipe.R --input stdin

    hello

`input` can also be set as a positional argument, then no argument name
is needed to specify in the command-line.

``` r

library(argitect)
argitect(
    "input=s#", "input"
)

if(input %in% c("-", "stdin")) {
    cat(readLines(file("stdin")), sep = "\n")
} else {
    cat(readLines(input), sep = "\n")
}
```

`main.R` is the script we used in vignette [Dispatching to
sub-commands](sub_commands.md).

    ~\> cat main.R | Rscript pipe.R -
    ~\> cat main.R | Rscript pipe.R stdin

    library(argitect)

    sub_commands(
        system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
        system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text."
    )

\
\
\
\
\
\
\
