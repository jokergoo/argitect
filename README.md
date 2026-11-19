# argitect

A pure-R command-line argument parser that reimplements the functionality of
the Perl module [`Getopt::Long`](https://perldoc.pl/Getopt::Long) without any
external dependency, and additionally supports **positional arguments**. It
also provides rich, customizable help messages inspired by the
[**GetoptLong**](https://cran.r-project.org/package=GetoptLong) package.

## Install

```r
install.packages("argitect", repos = NULL, type = "source")
```

## Usage

```r
library(argitect)

cutoff = 0.05
argitect(
    "number=i", "Number of items.",
    "cutoff=f", "Cutoff for filtering results.",
    "verbose!", "Print message.",
    "input=s#", "Input file (positional)."
)
```

Run from the command line:

```
Rscript foo.R --number 4 --cutoff 0.01 --verbose data.txt
Rscript foo.R --number=4 -c 0.01 -v data.txt
```

Parsed values are exported as R variables in the calling environment.

## Specification language

Each argument is `name[|alias...]=spec`. The value part `spec` is an optional
**mode** and an optional **type**, optionally followed by a bracketed range or
sub-argument list.

- modes: `i` integer, `f` numeric, `s` character, `l` logical, `o` extended
  integer
- types: `$` scalar, `#` positional, `@` vector, `%` hash

| Spec          | Meaning                                   |
|---------------|-------------------------------------------|
| `number=i`    | single integer scalar                     |
| `name=s`      | single character scalar                   |
| `length\|size=i` | integer scalar with two names            |
| `tag=i@`      | integer vector                            |
| `tag=i{2,4}`  | integer vector with 2–4 elements          |
| `param=s%`    | name=value pairs with character values    |
| `input=s#`    | one positional argument                   |
| `files=s#{}`  | one or more positional arguments          |
| `arg=i%{foo, bar=f}` | hash with typed sub-arguments     |
| `verbose!`    | logical, negatable with `--no-verbose`    |

## Help messages

Help is generated automatically with `--help`; `--version` prints the value of
the `VERSION` variable. Two styles are available:

```r
argitect.options("help_style" = "two-column")
```

Option groups, head/foot text and template-based usage text are also
supported (see `?argitect`).

## Sub commands

```r
subCommands(
    "cmd1.R", "First command.",
    "cmd2.R", "Second command."
)
```

The first command-line argument selects the sub command; the remaining
arguments are passed to the corresponding script (see `?subCommands`).

## Use of AI

The package was developed with assistance of DeepSeek.
