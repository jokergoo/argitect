# Tests for help/print formatting (R/print.R and helpers)

strip_trailing = function(x) sub("[ \t]+$", "", x)

# capture the help message with colors disabled and trailing spaces removed
capture_help = function(..., envir = parent.frame(), help_style = "one-column") {
    old = options(cli.num_colors = 1)
    on.exit(options(old))
    out = capture.output(argitect(..., envir = envir, argv_str = "--help",
                                  help_style = help_style))
    strip_trailing(out)
}


test_that("one-column help message matches the expected format", {
    e = new.env(); e$cutoff = 0.05
    out = capture_help("number=i", "Number of items.",
                       "cutoff=f", "Cutoff.",
                       "verbose!", "Print message.", envir = e)

    expected = c(
        "Usage: Rscript foo.R [options]",
        "",
        "Options:",
        "  --number, -n integer",
        "    Number of items.",
        "  --cutoff, -c numeric",
        "    Cutoff.",
        "    [default: 0.05]",
        "  --verbose, --no-verbose",
        "    Print message.",
        "    [default: off]",
        "  --help, -h",
        "    Print help message and exit.",
        "  --version",
        "    Print version information and exit."
    )
    expect_identical(out, expected)
})


test_that("two-column help message matches the expected format", {
    e = new.env(); e$cutoff = 0.05
    out = capture_help("number=i", "Number of items.",
                       "cutoff=f", "Cutoff.",
                       "verbose!", "Print message.", envir = e,
                       help_style = "two-column")

    expected = c(
        "Usage: Rscript foo.R [options]",
        "",
        "Options:",
        "  --number, -n integer       Number of items.",
        "  --cutoff, -c numeric       Cutoff.",
        "                             [default: 0.05]",
        "  --verbose, --no-verbose    Print message.",
        "                             [default: off]",
        "  --help, -h                 Print help message and exit.",
        "  --version                  Print version information and exit.",
        ""
    )
    expect_identical(out, expected)
})


test_that("help head/foot and option groups are printed", {
    e = new.env()
    out = capture_help(
        help_head = "HEAD.",
        "number=i", "Number.",
        "-------", "More:",
        "verbose!", "Verbose.",
        help_foot = "FOOT.",
        envir = e)

    expected = c(
        "HEAD.",
        "",
        "Usage: Rscript foo.R [options]",
        "",
        "  --number, -n integer",
        "    Number.",
        "",
        "More:",
        "  --verbose, --no-verbose",
        "    Verbose.",
        "    [default: off]",
        "  --help, -h",
        "    Print help message and exit.",
        "  --version",
        "    Print version information and exit.",
        "FOOT."
    )
    expect_identical(out, expected)
})


test_that("template-based help is rendered", {
    spec = paste0(
        "Usage: Rscript foo.R [options]\n",
        "\n",
        "Options:\n",
        "  <number=i> Number of items.\n",
        "  <verbose!> Print messages.\n"
    )
    e = new.env()
    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(argitect(spec, envir = e, argv_str = "--help"))
    out = strip_trailing(out)

    expected = c(
        "Usage: Rscript foo.R [options]",
        "",
        "Options:",
        "  --number, -n integer       Number of items.",
        "  --verbose, --no-verbose    Print messages."
    )
    expect_identical(out, expected)
})


test_that("version message prints the VERSION variable", {
    e = new.env(); e$VERSION = "1.2.3"
    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(argitect("tag=i", "T.", envir = e, argv_str = "--version"))
    expect_identical(out, "1.2.3")
})


test_that("sub-command help matches the expected format", {
    d = tempfile("argitect_sub_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines('argitect("x=i", "X.")', file.path(d, "cmd1.R"))
    writeLines('argitect("y=s", "Y.")', file.path(d, "cmd2.R"))

    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(sub_commands(
        c(file.path(d, "cmd1.R"), "First cmd",
          file.path(d, "cmd2.R"), "Second cmd"), argv_str = ""))
    out = strip_trailing(out)

    expected = c(
        "Usage: Rscript foo.R [command] [options]",
        "",
        "Commands:",
        "  cmd1    First cmd",
        "  cmd2    Second cmd",
        ""
    )
    expect_identical(out, expected)
})


test_that("format_text wraps text to the given width", {
    out = argitect:::format_text("this is a long text that should be wrapped",
                                 prefix = "  ", width = 20)
    expect_identical(out, "    this is a\n    long text\n    that should\n    be wrapped")
})


test_that("mode_label and mode_abbr return the documented labels", {
    expect_identical(argitect:::mode_label("i"), "integer")
    expect_identical(argitect:::mode_label("f"), "numeric")
    expect_identical(argitect:::mode_label("s"), "character")
    expect_identical(argitect:::mode_label("l"), "logical")

    expect_identical(argitect:::mode_abbr("i"), "int")
    expect_identical(argitect:::mode_abbr("f"), "num")
    expect_identical(argitect:::mode_abbr("s"), "chr")
})


test_that("find_code extracts templates and code", {
    r = argitect:::find_code("<CODE>", "x <a> y <b> z")
    expect_identical(r$template, c("<a>", "<b>"))
    expect_identical(r$code, c("a", "b"))
})


test_that("SingleArgument help methods produce the expected pieces", {
    e = new.env(); e$cutoff = 0.05
    a = argitect:::SingleArgument(spec = "cutoff=f", desc = "Cutoff.", envir = e)

    expect_identical(a$opt_name_line(), "--cutoff")
    expect_identical(a$type_suffix(), " numeric")
    expect_identical(a$default_as_string(), "0.05")

    # negatable logical shows its negation form
    b = argitect:::SingleArgument(spec = "verbose!", desc = "V.", envir = new.env())
    expect_identical(b$opt_name_line(), "--verbose")
    expect_identical(b$type_suffix(), ", --no-verbose")
    expect_identical(b$default_as_string(), "off")

    # positional argument shows <name>
    c = argitect:::SingleArgument(spec = "input=s#", desc = "Input.", envir = new.env())
    expect_identical(c$opt_name_line(), "--input")
})


test_that("explain mode prints explanations without parsing", {
    e = new.env()
    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(argitect("number=i", "N.", "verbose!", "V.",
                                  "tag=i@{foo, bar}", "K.", envir = e,
                                  explain = TRUE))
    out = strip_trailing(out)

    # does not parse command-line arguments, so nothing is exported
    expect_false(exists("number", envir = e, inherits = FALSE))
    expect_false(exists("verbose", envir = e, inherits = FALSE))

    # explanation lines are present
    expect_true(any(out == "  number=i"))
    expect_true(any(grepl("single integer value", out)))
    expect_true(any(grepl("--number 1", out)))
    expect_true(any(out == "  verbose!"))
    expect_true(any(grepl("logical flag", out)))
    expect_true(any(out == "  tag=i@{foo,bar}"))
    expect_true(any(grepl("named vector", out)))

    # R storage format is explained for the script developer
    expect_true(any(grepl("integer scalar in variable `number`", out)))
    expect_true(any(grepl("logical scalar in variable `verbose`", out)))
    expect_true(any(grepl("named integer vector in variable `tag`", out)))

    # reserved help/version are not explained
    expect_false(any(grepl("Print help message", out)))
})


test_that("explain shows R storage for positional hash sub-arguments", {
    e = new.env()
    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(argitect("arg=%{foo=i#, bar=s#}", "A.",
                                  "arg2=%{foo=i#{}, bar=s#}", "B.",
                                  "arg3=%{foo=i#, bar=s#{}}", "C.",
                                  envir = e, explain = TRUE))
    out = strip_trailing(out)

    expect_true(any(grepl('list(foo = 1L, bar = "a")', out, fixed = TRUE)))
    expect_true(any(grepl('list(foo = 1:3, bar = "a")', out, fixed = TRUE)))
    expect_true(any(grepl('list(foo = 1L, bar = c("a", "b", "c"))', out, fixed = TRUE)))
})


test_that("sub-argument help text via name$sub is rendered", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    # free-form hash: foo$name1 / foo$name2 provide the descriptions
    e = new.env()
    out = capture.output(argitect(
        "foo=i%", "This is foo.",
        "foo$name1", "name1 in foo.",
        "foo$name2", "name2 in foo.",
        envir = e, argv_str = "--help"))
    out = strip_trailing(out)
    expect_true(any(out == "    name1=integer"))
    expect_true(any(out == "    name2=integer"))
    expect_true(any(grepl("name1 in foo.", out, fixed = TRUE)))
    expect_true(any(grepl("name2 in foo.", out, fixed = TRUE)))

    # declared sub-args: documented ones show a description, the rest show only type
    e = new.env()
    out2 = capture.output(argitect(
        "foo=i%{name1, name2, name3}", "This is foo.",
        "foo$name1", "name1 in foo.",
        envir = e, argv_str = "--help"))
    out2 = strip_trailing(out2)
    expect_true(any(out2 == "    name1 integer"))
    expect_true(any(out2 == "    name2 integer"))
    expect_true(any(out2 == "    name3 integer"))
    expect_true(any(grepl("name1 in foo.", out2, fixed = TRUE)))
    # name2 and name3 have no description text
    expect_false(any(grepl("name2 in foo", out2, fixed = TRUE)))
    expect_false(any(grepl("name3 in foo", out2, fixed = TRUE)))
})


test_that("two-column help shows sub-argument descriptions", {
    e = new.env()
    out = capture_help(
        "foo=i%", "This is foo.",
        "foo$name1", "name1 in foo.",
        "foo$name2", "name2 in foo.",
        envir = e, help_style = "two-column")

    expect_true(any(grepl("Sub named options:", out, fixed = TRUE)))
    expect_true(any(grepl("name1=int", out, fixed = TRUE)))
    expect_true(any(grepl("name2=int", out, fixed = TRUE)))
    expect_true(any(grepl("name1 in foo.", out, fixed = TRUE)))
    expect_true(any(grepl("name2 in foo.", out, fixed = TRUE)))
})


test_that("template help aligns options within a section", {
    spec = paste0(
        "Usage: Rscript foo.R [options]\n",
        "\n",
        "Binary options:\n",
        "  <verbose!> Show messages.\n",
        "  <flag!> A flag.\n",
        "\n",
        "Single-value options:\n",
        "  <count=i> A count.\n")
    e = new.env()
    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(argitect(spec, envir = e, argv_str = "--help"))
    out = strip_trailing(out)

    expected = c(
        "Usage: Rscript foo.R [options]",
        "",
        "Binary options:",
        "  --verbose, --no-verbose    Show messages.",
        "  --flag, -f, --no-flag      A flag.",
        "",
        "Single-value options:",
        "  --count, -c integer    A count."
    )
    expect_identical(out, expected)
})


test_that("usage line places positional arguments by declaration order", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    usage = function(argv, ...) {
        out = strip_trailing(capture.output(argitect(..., argv_str = argv)))
        grep("^Usage:", out, value = TRUE)
    }

    # no positional
    expect_identical(usage("--help", "bar=i", "B.", envir = new.env()),
        "Usage: Rscript foo.R [options]")

    # first positional: single and variable length
    expect_identical(usage("1 --help", "foo=i#", "F.", "bar=i", "B.", envir = new.env()),
        "Usage: Rscript foo.R [foo] [options]")
    expect_identical(usage("a --help", "foo=#{}", "F.", "bar=i", "B.", envir = new.env()),
        "Usage: Rscript foo.R [foo...] [options]")

    # last positional: single and variable length
    expect_identical(usage("1 --help", "bar=i", "B.", "foo=i#", "F.", envir = new.env()),
        "Usage: Rscript foo.R [options] [foo]")
    expect_identical(usage("a --help", "bar=i", "B.", "foo=#{}", "F.", envir = new.env()),
        "Usage: Rscript foo.R [options] [foo...]")

    # positional on both sides of the options
    expect_identical(usage("1 2 --help", "a=i#", "A.", "bar=i", "B.", "b=i#", "BB.",
        envir = new.env()),
        "Usage: Rscript foo.R [a] [options] [b]")
})


test_that("type suffix shows fixed keys and count ranges", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    opt_line = function(spec, argv = "--help") {
        out = strip_trailing(capture.output(argitect(spec, "A.", envir = new.env(),
            argv_str = argv)))
        out[grepl("^  (--arg|<arg>)", out)]
    }

    # fixed sub-argument set, and open (trailing comma) set
    expect_identical(opt_line("arg=i%{foo, bar}"),
        "  --arg, -a {foo=integer, bar=integer}")
    expect_identical(opt_line("arg=i%{foo, bar,}"),
        "  --arg, -a {foo=integer, bar=integer, ...}")
    expect_identical(opt_line("arg=i%"),
        "  --arg, -a {name=integer, ...}")

    # named vector (square brackets)
    expect_identical(opt_line("arg=i@{foo, bar}"),
        "  --arg, -a [foo=integer, bar=integer]")

    # named positional (square brackets)
    expect_identical(opt_line("arg=i#{foo, bar}", "1 2 --help"),
        "  <arg> [foo=integer, bar=integer]")

    # count ranges on arrays
    expect_identical(opt_line("arg=i@{2,3}"), "  --arg, -a [integer, 2-3]")
    expect_identical(opt_line("arg=i@{3,}"), "  --arg, -a [integer, 3+]")
    expect_identical(opt_line("arg=i@{3}"), "  --arg, -a [integer, 3]")
    expect_identical(opt_line("arg=i@"), "  --arg, -a [integer, ...]")

    # count ranges on positionals
    expect_identical(opt_line("arg=i#", "1 --help"), "  <arg> integer")
    expect_identical(opt_line("arg=i#{}", "1 2 --help"), "  <arg> [integer, ...]")
    expect_identical(opt_line("arg=i#{2,4}", "1 2 --help"), "  <arg> [integer, 2-4]")
    expect_identical(opt_line("arg=i#{2,}", "1 2 --help"), "  <arg> [integer, 2+]")
})


test_that("long fixed-key type suffixes are split into aligned lines", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    # one-column: five sub-args break into one aligned key per line
    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo, bar, baz, qux, quux}", "This is arg.",
        envir = new.env(), argv_str = "--help")))
    expect_true(any(out == "  --arg, -a {foo=integer,"))
    expect_true(any(out == "             bar=integer,"))
    expect_true(any(out == "             baz=integer,"))
    expect_true(any(out == "             qux=integer,"))
    expect_true(any(out == "             quux=integer}"))

    # two-column: two sub-args already break into two aligned lines
    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo, bar}", "This is arg.",
        envir = new.env(), argv_str = "--help", help_style = "two-column")))
    expect_true(any(grepl("^  --arg, -a \\{foo=integer,", out)))
    expect_true(any(grepl("^             bar=integer\\}", out)))

    # a single sub-arg stays on one line
    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo}", "This is arg.",
        envir = new.env(), argv_str = "--help", help_style = "two-column")))
    expect_true(any(grepl("^  --arg, -a \\{foo=integer\\}", out)))
})


test_that("the open-set ellipsis gets its own line when the suffix wraps", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    # two-column, open set: "..." is placed on its own aligned line
    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo, bar,}", "This is arg.",
        envir = new.env(), argv_str = "--help", help_style = "two-column")))
    expect_true(any(grepl("^  --arg, -a \\{foo=integer,", out)))
    expect_true(any(grepl("^             bar=integer,", out)))
    expect_true(any(grepl("^             \\.\\.\\.\\}", out)))

    # single-line form keeps the inline ellipsis
    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo, bar,}", "This is arg.",
        envir = new.env(), argv_str = "--help")))
    expect_true(any(grepl("^  --arg, -a \\{foo=integer, bar=integer, \\.\\.\\.\\}", out)))
})


test_that("sub-argument count ranges are shown", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    # one-column: sub named options carry the count
    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo=@{1,2}, bar=i@}", "A.",
        envir = new.env(), argv_str = "--help")))
    expect_true(any(out == "    foo [integer, 1-2]"))
    expect_true(any(out == "    bar [integer, ...]"))

    # the type suffix also shows the count
    expect_true(any(grepl("\\{foo=\\[integer, 1-2\\], bar=\\[integer, \\.\\.\\.\\]\\}", out)))

    # two-column: abbreviated count
    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo=@{1,2}, bar=i@}", "A.",
        envir = new.env(), argv_str = "--help", help_style = "two-column")))
    expect_true(any(grepl("foo=\\[int, 1-2\\]", out)))
    expect_true(any(grepl("bar=\\[int, \\.\\.\\.\\]", out)))
})


test_that("help descriptions support glue interpolation", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    out = strip_trailing(capture.output(argitect(
        "number=i", "Got {{1 + 1}} items.",
        envir = new.env(), argv_str = "--help")))
    expect_true(any(out == "    Got 2 items."))

    out = strip_trailing(capture.output(argitect(
        "arg=i%{foo}", "A.", "arg$foo", "foo has {{nchar('abcd')}} letters.",
        envir = new.env(), argv_str = "--help")))
    expect_true(any(out == "        foo has 4 letters."))
})


test_that("template help evaluates cli chunks", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    spec = paste0("Usage: Rscript foo.R [options]\n", "\n",
        "{{ cli_h2('Binary options') }}\n",
        "  <verbose!> Show messages.\n")
    out = strip_trailing(capture.output(argitect(spec, envir = new.env(), argv_str = "--help")))

    expect_true(any(out == "\u2500\u2500 Binary options \u2500\u2500"))
    # no raw glue expression or cli token should leak into the output
    expect_false(any(grepl("\\{\\{", out)))
    expect_false(any(grepl("cli-[0-9]+-[0-9]+", out)))
})


test_that("template <#name> aligns a continuation line with the option", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    spec = paste0("Usage: Rscript foo.R [options]\n", "\n",
        "  <count=i@{2,}> This is a count.\n",
        "  <#count> More count text.\n")
    out = strip_trailing(capture.output(argitect(spec, envir = new.env(), argv_str = "--help")))

    opt = out[grepl("This is a count", out)]
    cont = out[grepl("More count text", out)]
    expect_length(opt, 1)
    expect_length(cont, 1)
    # the continuation line has a blank option column (only spaces before the text)
    expect_match(cont, "^ +More count text\\.$")
    # and the description starts at the same column as the option's description
    expect_equal(regexpr("More count text", cont), regexpr("This is a count", opt))
})


test_that("argitect template evaluates glue before parsing tags", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    spec = paste0(
        "Usage: Rscript foo.R [options]\n",
        "\n",
        "Options:\n",
        "  <{{ paste0('count', '=i') }}> Number of items.\n",
        "  <{{ paste0('verbose', '!') }}> Verbose.\n")
    out = strip_trailing(capture.output(argitect(spec, envir = new.env(), argv_str = "--help")))

    expect_true(any(grepl("^  --count, -c integer", out)))
    expect_true(any(grepl("^  --verbose, --no-verbose", out)))
    expect_false(any(grepl("\\{\\{", out)))
})


test_that("template continuation is detected by leading spaces", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    # more than 4 leading spaces joins the current paragraph
    out = strip_trailing(capture.output(argitect("
Usage: Rscript foo.R [options]

Options:
  <count=i> This is a count.
     This is a 5-space continuation.
", envir = new.env(), argv_str = "--help")))
    expect_true(any(grepl("This is a count. This is a 5-space continuation", out)))

    # 4 leading spaces stays a separate line
    out = strip_trailing(capture.output(argitect("
Usage: Rscript foo.R [options]

Options:
  <count=i> This is a count.
    This is a 4-space line.
", envir = new.env(), argv_str = "--help")))
    expect_false(any(grepl("This is a count. This is a 4-space line", out)))
    expect_true(any(grepl("This is a 4-space line", out)))
})


test_that("glue interpolation can use variables from the calling frame", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    f = function() {
        myvar = "hello-local-template"
        strip_trailing(capture.output(argitect(paste0(
            "Usage: Rscript foo.R [options]\n\n",
            "  <x=s> Value is {{ myvar }}.\n"), argv_str = "--help")))
    }
    expect_true(any(grepl("hello-local-template", f())))

    g = function() {
        myvar = "hello-local-description"
        e = new.env()
        strip_trailing(capture.output(argitect("x=s",
            "Value is {{ myvar }}.", envir = e, argv_str = "--help")))
    }
    expect_true(any(grepl("hello-local-description", g())))
})


test_that("fixed-key vectors and positionals use square brackets in the help", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    out = strip_trailing(capture.output(argitect("foo=i@{a,b}", "", envir = new.env(),
        argv_str = "--help")))
    expect_true(any(grepl("\\[a=integer, b=integer\\]", out)))

    out = strip_trailing(capture.output(argitect("foo=i#{a,b}", "", envir = new.env(),
        argv_str = "--help")))
    expect_true(any(grepl("\\[a=integer, b=integer\\]", out)))

    # hashes keep curly braces
    out = strip_trailing(capture.output(argitect("foo=i%{a,b}", "", envir = new.env(),
        argv_str = "--help")))
    expect_true(any(grepl("\\{a=integer, b=integer\\}", out)))
})


test_that("long option name lists are wrapped and aligned", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    on.exit(argitect_options(RESET = TRUE), add = TRUE)

    aliases = "number|a|b|c|d|e|f|g|h|k|l|m|n|p|q|r|t|u|w|x|y|z=i"
    argitect_options("help_width" = 60)
    out = strip_trailing(capture.output(argitect(aliases, "N.", envir = new.env(),
        argv_str = "--help")))

    # the first line ends with a comma, the continuation aligns with "-a"
    first = grep("^  --number, ", out, value = TRUE)
    expect_true(length(first) == 1)
    expect_true(grepl(",$", first))
    cont = grep("^            -", out, value = TRUE)
    expect_true(length(cont) >= 1)

    # every line fits the requested width
    expect_true(all(nchar(out) <= 60))
})

test_that("two-column help_width controls the first column width", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    on.exit(argitect_options(RESET = TRUE), add = TRUE)

    aliases = "number|a|b|c|d|e|f|g|h|k|l|m|n|p|q|r|t|u|w|x|y|z=i"
    argitect_options(help_style = "two-column", help_width = c(90, 40))
    out = strip_trailing(capture.output(argitect(aliases, "N.", envir = new.env(),
        argv_str = "--help")))

    # the option names wrap at the first-column width
    expect_true(any(grepl("^  --number, -a", out)))
    expect_true(any(grepl("^            -", out)))
    # the description is placed after the first column
    desc = grep("N\\.", out, value = TRUE)
    expect_true(length(desc) >= 1)
    expect_true(all(nchar(desc) <= 90))
})


test_that("help_width is controlled by the global option", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    on.exit(argitect_options(RESET = TRUE), add = TRUE)

    aliases = "number|a|b|c|d|e|f|g|h|k|l|m|n=i"

    argitect_options("help_width" = 50)
    out = strip_trailing(capture.output(argitect(aliases, "N.", envir = new.env(),
        argv_str = "--help")))
    expect_true(all(nchar(out) <= 50))
    expect_true(any(grepl("^            -", out)))

    argitect_options("help_width" = 100)
    out = strip_trailing(capture.output(argitect(aliases, "N.", envir = new.env(),
        argv_str = "--help")))
    expect_true(any(grepl("--number, -a, .*, -n integer", out)))
})


test_that("plain template lines are wrapped to the help width", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    out = strip_trailing(capture.output(argitect("
Usage: Rscript foo.R [options]

  <count=i@{2,}>
  This is a count. This is a count. This is a count.
         This is a count. This is a count foduo  ofoaoauouf faoufo faouf .
", envir = new.env(), argv_str = "--help")))

    expect_true(all(nchar(out) <= 90))
    # the long paragraph is split into several lines
    expect_true(any(grepl("This is a count", out)))
    expect_true(any(grepl("foduo", out)))
    expect_false(any(grepl("foduo", grep("This is a count", out, value = TRUE))))
    # the leading indentation is preserved
    expect_true(all(grepl("^  ", grep("This is a count|foduo", out, value = TRUE))))
})


test_that("help_width can be set per call and overrides the global option", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    on.exit(argitect_options(RESET = TRUE), add = TRUE)

    aliases = "number|a|b|c|d|e|f|g|h|k|l|m|n=i"

    out = strip_trailing(capture.output(argitect(aliases, "N.", envir = new.env(),
        argv_str = "--help", help_width = 50)))
    expect_true(all(nchar(out) <= 50))
    expect_true(any(grepl("^            -", out)))

    # a per-call value wins over the global option
    argitect_options("help_width" = 60)
    out = strip_trailing(capture.output(argitect(aliases, "N.", envir = new.env(),
        argv_str = "--help", help_width = 100)))
    expect_true(any(grepl("--number, -a, .*, -n integer", out)))
})
