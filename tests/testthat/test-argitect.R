# Core parsing tests for argitect

test_that("scalar options parse in various forms", {
    e = new.env()
    argitect("number=i", "Number.", "cutoff=f", "Cutoff.", envir = e,
        argv_str = "--number 4 --cutoff 0.01")
    expect_equal(e$number, 4L)
    expect_equal(e$cutoff, 0.01)

    e = new.env()
    argitect("number=i", "N.", envir = e, argv_str = "--number=7")
    expect_equal(e$number, 7L)

    e = new.env()
    argitect("number=i", "N.", envir = e, argv_str = "-n 5")
    expect_equal(e$number, 5L)

    e = new.env()
    argitect("number=i", "N.", envir = e, argv_str = "-n5")
    expect_equal(e$number, 5L)
})

test_that("logical options work with negation", {
    e = new.env()
    argitect("verbose!", "V.", envir = e, argv_str = "--verbose")
    expect_true(e$verbose)

    e = new.env()
    argitect("verbose!", "V.", envir = e, argv_str = "--no-verbose")
    expect_false(e$verbose)

    e = new.env()
    argitect("verbose!", "V.", envir = e, argv_str = "--noverbose")
    expect_false(e$verbose)

    e = new.env()
    suppressWarnings(
        argitect("verbose", "V.", envir = e, argv_str = "--verbose"))
    expect_true(e$verbose)
})

test_that("array and hash options accumulate", {
    e = new.env()
    argitect("tag=i@", "T.", envir = e, argv_str = "--tag 1 2 3")
    expect_equal(e$tag, c(1L, 2L, 3L))

    e = new.env()
    argitect("tag=i@", "T.", envir = e, argv_str = "--tag 1 --tag 2")
    expect_equal(e$tag, c(1L, 2L))

    e = new.env()
    argitect("param=s%", "P.", envir = e, argv_str = "--param foo=a bar=b")
    expect_equal(e$param, list(foo = "a", bar = "b"))
})

test_that("positional arguments are captured", {
    e = new.env()
    argitect("input=s#", "Input.", "verbose!", "V.", envir = e,
        argv_str = "data.txt --verbose")
    expect_equal(e$input, "data.txt")
    expect_true(e$verbose)

    e = new.env()
    argitect("files=s#{}", "Files.", envir = e, argv_str = "a.txt b.txt c.txt")
    expect_equal(e$files, c("a.txt", "b.txt", "c.txt"))
})

test_that("positional and option arguments can be mixed in any order", {
    # options are parsed first; remaining non-option tokens become positionals
    e = new.env()
    argitect("input=s#", "Input.", "verbose!", "V.", "number=i", "N.", envir = e,
        argv_str = "--verbose data.txt --number 5")
    expect_equal(e$input, "data.txt")
    expect_true(e$verbose)
    expect_equal(e$number, 5L)

    e = new.env()
    argitect("input=s#", "Input.", "verbose!", "V.", envir = e,
        argv_str = "data.txt --verbose")
    expect_equal(e$input, "data.txt")
    expect_true(e$verbose)

    # unexpected positional arguments (no matching # spec) error
    e = new.env()
    expect_error(argitect("verbose!", "V.", envir = e, argv_str = "stray.txt"),
        "Unexpected positional")
})

test_that("hash sub-arguments are parsed", {
    e = new.env()
    argitect("arg=%{foo=i, bar=f}", "A.", envir = e, argv_str = "--arg foo=1 bar=1.5")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, 1.5)

    e = new.env()
    argitect("arg=%{foo=i@, bar=f@}", "A.", envir = e,
        argv_str = "--arg foo=1 foo=2 bar=1.5")
    expect_equal(e$arg$foo, c(1L, 2L))
    expect_equal(e$arg$bar, 1.5)

    e = new.env()
    argitect("arg=%{foo=i#, bar=s#}", "A.", envir = e, argv_str = "--arg 1 a")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, "a")
})

test_that("prefix matching and bundling work", {
    e = new.env()
    argitect("number=i", "N.", envir = e, argv_str = "--num 5")
    expect_equal(e$number, 5L)

    e = new.env()
    argitect("alpha!", "A.", "beta!", "B.", "gamma!", "G.", envir = e, argv_str = "-abg")
    expect_true(e$alpha && e$beta && e$gamma)
})

test_that("partial matching from the first character works", {
    # unique prefix -> --num, -num (distractor "nothing" shares only "n")
    e = new.env()
    argitect("number=i", "N.", "nothing!", "No.", envir = e, argv_str = "--num 5")
    expect_equal(e$number, 5L)

    e = new.env()
    argitect("number=i", "N.", "nothing!", "No.", envir = e, argv_str = "-num 6")
    expect_equal(e$number, 6L)

    # unique first letter -> -n and --n
    e = new.env()
    argitect("number=i", "N.", "count!", "C.", envir = e, argv_str = "-n 7")
    expect_equal(e$number, 7L)

    e = new.env()
    argitect("number=i", "N.", "count!", "C.", envir = e, argv_str = "--n 8")
    expect_equal(e$number, 8L)

    # ambiguous prefix / letter -> error
    e = new.env()
    expect_error(argitect("number=i", "N.", "nothing!", "No.", envir = e,
        argv_str = "--n 9"), "ambiguous")
    expect_error(argitect("number=i", "N.", "nothing!", "No.", envir = e,
        argv_str = "-n 9"), "ambiguous")
})

test_that('"-" is treated as a value (standard input)', {
    e = new.env()
    argitect("input=s#", "I.", envir = e, argv_str = "-")
    expect_equal(e$input, "-")

    e = new.env()
    argitect("input=s", "I.", envir = e, argv_str = "--input -")
    expect_equal(e$input, "-")
})
test_that("mandatory and type validation", {
    e = new.env()
    expect_error(argitect("number=i", "N.", envir = e, argv_str = ""), "mandatory")

    e = new.env()
    expect_error(argitect("tag=i", "T.", envir = e, argv_str = "--tag 0.1"), "integer")

    e = new.env()
    expect_error(argitect("tag=f", "T.", envir = e, argv_str = "--tag abc"), "number")

    e = new.env()
    expect_error(argitect("tag=i@{2,4}", "T.", envir = e, argv_str = "--tag 1"), "Insufficient")
})

test_that("defaults are detected from the environment", {
    e = new.env(); e$cutoff = 0.05
    argitect("number=i", "N.", "cutoff=f", "C.", envir = e, argv_str = "--number 4")
    expect_equal(e$number, 4L)
    expect_equal(e$cutoff, 0.05)
})

test_that("mode{...} without a type character is rejected", {
    for(s in c("arg=i{2,4}", "arg=i{2}", "arg=i{,2}", "arg=i{2,}", "arg=i{foo, bar}")) {
        expect_error(argitect:::parse_spec(s), info = paste("should error:", s))
    }
})

test_that("integer mode i accepts extended integer notation", {
    e = new.env()
    argitect("tag=i", "T.", envir = e, argv_str = "--tag 0b001001")
    expect_equal(e$tag, 9L)

    e = new.env()
    argitect("tag=i", "T.", envir = e, argv_str = "--tag 0xaf2")
    expect_equal(e$tag, 2802L)

    # octal (0-prefixed)
    e = new.env()
    argitect("tag=i", "T.", envir = e, argv_str = "--tag 0123")
    expect_equal(e$tag, 83L)

    # plain decimal still works
    e = new.env()
    argitect("tag=i", "T.", envir = e, argv_str = "--tag 123")
    expect_equal(e$tag, 123L)

    # the o mode no longer exists
    expect_error(argitect:::parse_spec("tag=o"), "Wrong specification")
})


test_that("argument aliases support long and short forms", {
    # foo|bar=i: --foo / --bar / -f / -b all set the variable foo
    cases = list(
        c("--foo 1", 1L),
        c("--bar 2", 2L),
        c("-f 3", 3L),
        c("-b 4", 4L)
    )
    for(cs in cases) {
        e = new.env()
        argitect("foo|bar=i", "F.", envir = e, argv_str = cs[1])
        expect_equal(e$foo, as.integer(cs[2]), info = cs[1])
    }

    # -b is ambiguous when another argument also starts with b
    e = new.env()
    expect_error(argitect("foo|bar=i", "F.", "baz=i", "Z.", envir = e,
        argv_str = "-b 5"), "ambiguous")
})


test_that("negative numeric values are parsed as values, not options", {
    # scalar integer / numeric
    e = new.env()
    argitect("count=i", "C.", envir = e, argv_str = "--count -1")
    expect_equal(e$count, -1L)

    e = new.env()
    argitect("x=f", "X.", envir = e, argv_str = "--x -1.5")
    expect_equal(e$x, -1.5)

    # array values
    e = new.env()
    argitect("tag=i@", "T.", envir = e, argv_str = "--tag -1 2 -3")
    expect_equal(e$tag, c(-1L, 2L, -3L))

    # positional arguments
    e = new.env()
    argitect("v=i#", "V.", envir = e, argv_str = "-5")
    expect_equal(e$v, -5L)

    # scientific notation
    e = new.env()
    argitect("x=f", "X.", envir = e, argv_str = "--x -1e-3")
    expect_equal(e$x, -0.001)
})


test_that("auto short flags do not collide with explicit single-letter aliases", {
    # "a" is an explicit alias of option_a, so "abc" must not auto-claim "-a"
    e = new.env()
    argitect("option_a|a!", "A", "option_b|b!", "B", "abc!", "abs",
        envir = e, argv_str = "-a")
    expect_true(e$option_a)
    expect_false(e$abc)

    # --abc and its unique prefix --ab still reach abc
    e = new.env()
    argitect("option_a|a!", "A", "option_b|b!", "B", "abc!", "abs",
        envir = e, argv_str = "--abc")
    expect_true(e$abc)

    e = new.env()
    argitect("option_a|a!", "A", "option_b|b!", "B", "abc!", "abs",
        envir = e, argv_str = "--ab")
    expect_true(e$abc)

    # the help message does not list -a for abc
    e = new.env()
    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(argitect("option_a|a!", "A", "option_b|b!", "B", "abc!", "abs",
        envir = e, argv_str = "--help"))
    out = sub("[ \t]+$", "", out)
    expect_true(any(out == "  --option-a, -a, --no-option-a"))
    expect_true(any(out == "  --abc, --no-abc"))
})


test_that("duplicated argument names or aliases are rejected", {
    # same first name
    expect_error(argitect("foo!", "", "foo!", "", envir = new.env(), argv_str = ""),
        "Duplicated argument name or alias")

    # an alias of one argument equals the name of another
    expect_error(argitect("bar|foo!", "", "foo!", "", envir = new.env(), argv_str = ""),
        "Duplicated argument name or alias")

    # same alias on two arguments
    expect_error(argitect("a|x!", "", "b|x!", "", envir = new.env(), argv_str = ""),
        "Duplicated argument name or alias")

    # distinct names still work
    e = new.env()
    argitect("foo!", "", "bar!", "", envir = e, argv_str = "--foo")
    expect_true(e$foo)
    expect_false(e$bar)
})


test_that("help and version are rejected as reserved without a warning", {
    capture = function(expr) {
        w = character(0)
        r = tryCatch(withCallingHandlers(expr,
                warning = function(c) {
                    w <<- c(w, conditionMessage(c))
                    invokeRestart("muffleWarning")
                }),
            error = function(e) conditionMessage(e))
        list(err = r, warned = length(w) > 0)
    }

    r = capture(argitect("help", "", envir = new.env(), argv_str = ""))
    expect_match(r$err, "reserved")
    expect_false(r$warned)

    r = capture(argitect("version", "", envir = new.env(), argv_str = ""))
    expect_match(r$err, "reserved")
    expect_false(r$warned)

    r = capture(argitect("foo|help!", "", envir = new.env(), argv_str = ""))
    expect_match(r$err, "reserved")
    expect_false(r$warned)
})

test_that("conversion errors name the argument and are not double-prefixed", {
    err = tryCatch(argitect("foo=i", "", argv_str = "--foo a"),
        error = function(e) conditionMessage(e))
    expect_match(err, "for option 'foo'")
    expect_false(grepl("^Error:", err))
    expect_false(grepl("Error: Error", err))

    err = tryCatch(argitect("arg=%{foo=i, bar=f}", "", argv_str = "--arg foo=x bar=1.5"),
        error = function(e) conditionMessage(e))
    expect_match(err, "for option 'arg\\$foo'")

    err = tryCatch(argitect("tag=i@", "", argv_str = "--tag 1 x"),
        error = function(e) conditionMessage(e))
    expect_match(err, "for option 'tag'")
})


test_that("config option controls bundling and ignore_case", {
    on.exit(argitect_options(RESET = TRUE), add = TRUE)

    # bundling is the default: -abg is -a -b -g
    e = new.env()
    argitect("alpha!", "A.", "beta!", "B.", "gamma!", "G.", envir = e, argv_str = "-abg")
    expect_true(e$alpha)
    expect_true(e$beta)
    expect_true(e$gamma)

    # no_bundling disables bundling
    argitect_options("config" = "no_bundling")
    e = new.env()
    expect_error(argitect("alpha!", "A.", "beta!", "B.", envir = e, argv_str = "-ab"),
        "Unknown option")

    # ignore_case matches names case-insensitively
    argitect_options("config" = "ignore_case")
    e = new.env()
    argitect("foo=i", "F.", envir = e, argv_str = "--FOO 5")
    expect_equal(e$foo, 5L)

    # no_ignore_case overrides ignore_case
    argitect_options("config" = c("ignore_case", "no_ignore_case"))
    e = new.env()
    expect_error(argitect("foo=i", "F.", envir = e, argv_str = "--FOO 5"), "Unknown option")
})


test_that("argitect_options is an alias of argitect_options", {
    expect_identical(argitect_options, argitect_options)

    on.exit(argitect_options(RESET = TRUE), add = TRUE)
    argitect_options("help_width" = 100)
    expect_equal(argitect_options("help_width"), 100)
    argitect_options("help_width" = 90)
    expect_equal(argitect_options("help_width"), 90)
})


test_that("a single '-' is a normal value, not an option name", {
    e = new.env()
    argitect("input=s#", "I.", envir = e, argv_str = "-")
    expect_equal(e$input, "-")

    e = new.env()
    argitect("a=s", "A.", envir = e, argv_str = "--a -")
    expect_equal(e$a, "-")

    e = new.env()
    argitect("a=s@", "A.", envir = e, argv_str = "--a - x")
    expect_equal(e$a, c("-", "x"))

    e = new.env()
    argitect("input=s#{}", "I.", envir = e, argv_str = "-- - x")
    expect_equal(e$input, c("-", "x"))
})


test_that("positional arguments support a fixed key set", {
    e = new.env()
    argitect("coord=i#{x,y}", "", envir = e, argv_str = "1 2")
    expect_equal(e$coord, c(x = 1L, y = 2L))

    e = new.env()
    expect_error(argitect("coord=i#{x,y}", "", envir = e, argv_str = "1"),
        "Insufficient")

    e = new.env()
    expect_error(argitect("coord=i#{x,y}", "", envir = e, argv_str = ""),
        "mandatory")

    # a named default is used when no token is given
    e = new.env(); e$coord = c(x = 7L, y = 8L)
    argitect("coord=i#{x,y}", "", envir = e, argv_str = "")
    expect_equal(e$coord, c(x = 7L, y = 8L))
})


test_that("multiple single-letter aliases are all accepted", {
    e = new.env()
    argitect("number|a|b|c=i", "N.", envir = e, argv_str = "-b 5")
    expect_equal(e$number, 5L)

    e = new.env()
    argitect("number|a|b|c=i", "N.", envir = e, argv_str = "-a 2")
    expect_equal(e$number, 2L)

    e = new.env()
    argitect("number|a|b|c=i", "N.", envir = e, argv_str = "--number 7")
    expect_equal(e$number, 7L)

    # the help message renders without error and lists every alias
    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(argitect("number|a|b|c=i", "N.", envir = new.env(),
        argv_str = "--help"))
    expect_true(any(grepl("--number, -a, -b, -c", out)))
})
