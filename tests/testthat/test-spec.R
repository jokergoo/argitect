# Coverage of every specification example listed in test/test.R

parse_ok = function(spec) {
    !inherits(tryCatch(suppressWarnings(argitect:::parse_spec(spec)),
                       error = function(e) e), "error")
}

parse_err = function(spec) {
    inherits(tryCatch(suppressWarnings(argitect:::parse_spec(spec)),
                      error = function(e) e), "error")
}

# parse with warning capture: returns list(ok = ..., warned = ...)
parse_warn = function(spec) {
    w = character(0)
    r = tryCatch(
        withCallingHandlers(argitect:::parse_spec(spec),
            warning = function(c) {
                w <<- c(w, conditionMessage(c))
                invokeRestart("muffleWarning")
            }),
        error = function(e) e)
    list(ok = !inherits(r, "error"), warned = length(w) > 0)
}


test_that("all strict-format specifications parse without error", {
    strict_specs = c(
        "arg",
        "arg!",
        "arg=l",
        "arg=i#",
        "arg=i#{}",
        "arg=i#{2,4}",
        "arg=i@",
        "arg=i@{}",
        "arg=i@{3}",
        "arg=i@{3,}",
        "arg=i@{,3}",
        "arg=i@{2,3}",
        "arg=i@{foo, bar}",
        "arg=i@{foo|bar}",
        "arg=i@{foo, bar,}",
        "arg=i%",
        "arg=i%{}",
        "arg=i%{foo, bar}",
        "arg=i%{foo, bar,}",
        "arg=i%{foo=@, bar=@}",
        "arg=i%{foo=f}",
        "arg=%{foo=i, bar=f}",
        "arg=%{foo=i, bar!}",
        "arg=%{foo=i@, bar=f@}",
        "arg=%{foo=i@[2], bar=f@[3]}",
        "arg=%{foo=i#, bar=s#}",
        "arg=%{foo=i#, bar=s#{}}",
        "arg=%{foo=i#{}, bar=s#{}}"
    )
    for(s in strict_specs) {
        expect_true(parse_ok(s), info = paste("spec should parse:", s))
    }
})


test_that("explicitly-invalid specifications throw errors", {
    error_specs = c(
        # bare type without a mode
        "arg=#",
        "arg=@",
        "arg=$",
        # mode{...} without an explicit type character
        "arg=i{3}",
        "arg=i{3,}",
        "arg=i{,3}",
        "arg=i{2,3}",
        "arg=i{foo, bar}",
        # bare {...} with untyped members is ambiguous
        "arg={foo, bar}",
        # empty or untyped braced hash is not allowed
        "arg=%{}",
        "arg=%{foo, bar}",
        "arg=%{foo, bar=f}",
        "arg=%{foo=@{1,2}}",
        "arg=%{foo=(1,2)}",
        # a trailing comma is not allowed when the mode is not determined
        "arg=%{foo=i, bar!,}",
        "arg=@{foo, bar,}"
    )
    for(s in error_specs) {
        expect_true(parse_err(s), info = paste("spec should error:", s))
    }
})


test_that("ambiguous bare types and brackets warn (and parse)", {
    warn_specs = c(
        "arg=%",
        "arg={}",
        "arg={foo=i, bar=f}",
        "arg={foo=i@, bar=f@}"
    )
    for(s in warn_specs) {
        r = parse_warn(s)
        expect_true(r$ok, info = paste("spec should parse:", s))
        expect_true(r$warned, info = paste("spec should warn:", s))
    }
})


test_that("fixed-key arrays become named vectors", {
    # arg=i@{foo, bar} -> --arg foo=1 bar=2 -> named integer vector
    e = new.env()
    argitect("arg=i@{foo, bar}", "A.", envir = e, argv_str = "--arg foo=1 bar=2")
    expect_equal(e$arg, c(foo = 1L, bar = 2L))

    # arg=i@{foo|bar} -> bar is an alias of foo
    e = new.env()
    argitect("arg=i@{foo|bar}", "A.", envir = e, argv_str = "--arg bar=5")
    expect_equal(e$arg, c(foo = 5L))
})


test_that("logical options accept every negation form", {
    forms = c("--no-verbose", "--not-verbose", "--verbose!", "--!verbose")
    for(form in forms) {
        e = new.env()
        argitect("verbose!", "V.", envir = e, argv_str = form)
        expect_false(e$verbose, info = paste("negation form:", form))
    }

    # plain logical (not just negatable) also supports negation
    e = new.env()
    suppressWarnings(argitect("flag", "F.", envir = e, argv_str = "--no-flag"))
    expect_false(e$flag)
})


test_that("command-line examples in test.R comments produce the documented values", {
    # arg=%{foo=i, bar!} -> --arg foo=1 bar / --arg foo=1 no-bar
    e = new.env()
    argitect("arg=%{foo=i, bar!}", "A.", envir = e, argv_str = "--arg foo=1 bar")
    expect_equal(e$arg$foo, 1L)
    expect_true(e$arg$bar)

    e = new.env()
    argitect("arg=%{foo=i, bar!}", "A.", envir = e, argv_str = "--arg foo=1 no-bar")
    expect_equal(e$arg$foo, 1L)
    expect_false(e$arg$bar)

    # arg=%{foo=i@, bar=f@} -> --arg foo=1 bar=1.1
    e = new.env()
    argitect("arg=%{foo=i@, bar=f@}", "A.", envir = e, argv_str = "--arg foo=1 bar=1.1")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, 1.1)

    # arg=%{foo=i#, bar=s#} -> --arg 1 a (positional, first to foo, rest to bar)
    e = new.env()
    argitect("arg=%{foo=i#, bar=s#}", "A.", envir = e, argv_str = "--arg 1 a")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, "a")

    # arg=%{foo=i#, bar=s#{}} -> --arg 1 2 a b
    e = new.env()
    argitect("arg=%{foo=i#, bar=s#{}}", "A.", envir = e, argv_str = "--arg 1 2 a b")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, c("2", "a", "b"))

    # arg=%{foo=i#{}, bar=s#{}} -> consecutive integers to foo, the rest to bar
    e = new.env()
    argitect("arg=%{foo=i#{}, bar=s#{}}", "A.", envir = e, argv_str = "--arg 1 2 3 a b")
    expect_equal(e$arg$foo, c(1L, 2L, 3L))
    expect_equal(e$arg$bar, c("a", "b"))
})


test_that("scalar, vector and positional types parse command lines", {
    # arg=i# (single positional integer)
    e = new.env()
    argitect("arg=i#", "A.", envir = e, argv_str = "7")
    expect_equal(e$arg, 7L)

    # arg=i#{} (positional integer vector)
    e = new.env()
    argitect("arg=i#{}", "A.", envir = e, argv_str = "1 2 3")
    expect_equal(e$arg, c(1L, 2L, 3L))

    # arg=i#{2,4} (positional integer vector with range)
    e = new.env()
    argitect("arg=i#{2,4}", "A.", envir = e, argv_str = "1 2 3")
    expect_equal(e$arg, c(1L, 2L, 3L))

    # arg=i@{3} (vector of exactly 3)
    e = new.env()
    argitect("arg=i@{3}", "A.", envir = e, argv_str = "--arg 1 2 3")
    expect_equal(e$arg, c(1L, 2L, 3L))

    # arg=i@{3,} (vector of at least 3)
    e = new.env()
    argitect("arg=i@{3,}", "A.", envir = e, argv_str = "--arg 1 2 3 4")
    expect_equal(e$arg, c(1L, 2L, 3L, 4L))

    # arg=i@{,3} (vector of at most 3)
    e = new.env()
    argitect("arg=i@{,3}", "A.", envir = e, argv_str = "--arg 1 2")
    expect_equal(e$arg, c(1L, 2L))

    # arg=i@{2,3} (vector with 2-3 elements)
    e = new.env()
    argitect("arg=i@{2,3}", "A.", envir = e, argv_str = "--arg 1 2")
    expect_equal(e$arg, c(1L, 2L))
})


test_that("free-form and typed hashes parse", {
    # arg=i% (free-form integer hash)
    e = new.env()
    argitect("arg=i%", "A.", envir = e, argv_str = "--arg a=1 b=2")
    expect_equal(e$arg, list(a = 1L, b = 2L))

    # arg=i%{} (free-form integer hash)
    e = new.env()
    argitect("arg=i%{}", "A.", envir = e, argv_str = "--arg a=1 b=2")
    expect_equal(e$arg, list(a = 1L, b = 2L))

    # arg=i%{foo, bar} (hash with sub-arguments)
    e = new.env()
    argitect("arg=i%{foo, bar}", "A.", envir = e, argv_str = "--arg foo=1 bar=2")
    expect_equal(e$arg, list(foo = 1L, bar = 2L))

    # arg=%{foo=i, bar=f}
    e = new.env()
    argitect("arg=%{foo=i, bar=f}", "A.", envir = e, argv_str = "--arg foo=1 bar=1.5")
    expect_equal(e$arg, list(foo = 1L, bar = 1.5))

    # arg=%{foo=i@[2], bar=f@[3]}
    e = new.env()
    argitect("arg=%{foo=i@[2], bar=f@[3]}", "A.",
        envir = e, argv_str = "--arg foo=1 foo=2 bar=1.1 bar=2.2 bar=3.3")
    expect_equal(e$arg$foo, c(1L, 2L))
    expect_equal(e$arg$bar, c(1.1, 2.2, 3.3))
})


test_that("logical specifications parse", {
    # arg (logical)
    e = new.env()
    suppressWarnings(argitect("arg", "A.", envir = e, argv_str = "--arg"))
    expect_true(e$arg)

    # arg! (negatable logical)
    e = new.env()
    argitect("arg!", "A.", envir = e, argv_str = "--no-arg")
    expect_false(e$arg)

    # arg=l (explicit logical scalar)
    e = new.env()
    argitect("arg=l", "A.", envir = e, argv_str = "--arg")
    expect_true(e$arg)
})


test_that("positional hash sub-arguments store the documented R values", {
    # arg=%{foo=i#, bar=s#} -> --arg 1 a -> list(foo = 1L, bar = "a")
    e = new.env()
    argitect("arg=%{foo=i#, bar=s#}", "A.", envir = e, argv_str = "--arg 1 a")
    expect_equal(e$arg, list(foo = 1L, bar = "a"))

    # arg=%{foo=i#, bar=s#{}} -> --arg 1 a b c -> list(foo = 1L, bar = c("a", "b", "c"))
    e = new.env()
    argitect("arg=%{foo=i#, bar=s#{}}", "A.", envir = e, argv_str = "--arg 1 a b c")
    expect_equal(e$arg, list(foo = 1L, bar = c("a", "b", "c")))

    # arg=%{foo=i#{}, bar=s#} -> --arg 1 2 3 a -> list(foo = 1:3, bar = "a")
    e = new.env()
    argitect("arg=%{foo=i#{}, bar=s#}", "A.", envir = e, argv_str = "--arg 1 2 3 a")
    expect_equal(e$arg, list(foo = c(1L, 2L, 3L), bar = "a"))
})


test_that("vector sub-arguments accept comma-separated values", {
    # arg=%{foo=i@, bar=f@} -> --arg foo=1,2,3 bar=1.5,2.5
    e = new.env()
    argitect("arg=%{foo=i@, bar=f@}", "A.", envir = e,
        argv_str = "--arg foo=1,2,3 bar=1.5,2.5")
    expect_equal(e$arg$foo, c(1L, 2L, 3L))
    expect_equal(e$arg$bar, c(1.5, 2.5))

    # repeated name=value still accumulates
    e = new.env()
    argitect("arg=%{foo=i@}", "A.", envir = e, argv_str = "--arg foo=1 foo=2")
    expect_equal(e$arg$foo, c(1L, 2L))

    # custom separator via argitect()
    e = new.env()
    argitect("arg=%{foo=i@}", "A.", envir = e, argv_str = "--arg foo=1;2;3",
        value_sep = ";")
    expect_equal(e$arg$foo, c(1L, 2L, 3L))

    # custom separator via argitect_options()
    argitect_options("value_sep" = ";")
    e = new.env()
    argitect("arg=%{foo=i@}", "A.", envir = e, argv_str = "--arg foo=1;2;3")
    expect_equal(e$arg$foo, c(1L, 2L, 3L))
    argitect_options(RESET = TRUE)
})


test_that("vector sub-argument separator is auto-detected", {
    # each of , ; : | / is detected by default
    for(sep in c(",", ";", ":", "|", "/")) {
        e = new.env()
        argitect("arg=%{foo=i@}", "A.", envir = e,
            argv_str = paste0("--arg foo=1", sep, "2", sep, "3"))
        expect_equal(e$arg$foo, c(1L, 2L, 3L), info = paste("separator:", sep))
    }

    # a single value without a separator is kept as a one-element vector
    e = new.env()
    argitect("arg=%{foo=i@}", "A.", envir = e, argv_str = "--arg foo=1")
    expect_equal(e$arg$foo, 1L)

    # an explicit separator still overrides auto-detection
    e = new.env()
    argitect("arg=%{foo=i@}", "A.", envir = e, argv_str = "--arg foo=1;2;3",
        value_sep = ";")
    expect_equal(e$arg$foo, c(1L, 2L, 3L))
})


test_that("argument aliases cannot be entirely numeric", {
    expect_true(parse_err("foo|1=i"))
    expect_true(parse_err("foo|123=i"))
    expect_true(parse_ok("foo|bar=i"))
})


test_that("invalid alias format gives the correct error message", {
    # an invalid character (not "numeric") must not trigger the numeric message
    expect_error(argitect:::parse_spec("foo|fd~sa=i"), "wrong format")
    expect_error(argitect:::parse_spec("foo|1=i"), "cannot be entirely numeric")
    expect_error(argitect:::parse_spec("foo|123=i"), "cannot be entirely numeric")
})


test_that("fixed-key named vectors require all keys (or merge defaults)", {
    # missing key -> error
    e = new.env()
    expect_error(argitect("arg=i@{foo,bar}", "A.", envir = e, argv_str = "--arg foo=1"),
        "Missing value")

    # default merges: only foo is overridden, bar is preserved
    e = new.env(); e$arg = c(foo = 0L, bar = 0L)
    argitect("arg=i@{foo,bar}", "A.", envir = e, argv_str = "--arg foo=1")
    expect_equal(e$arg, c(foo = 1L, bar = 0L))
})


test_that("default format is validated against the specification", {
    # wrong key
    e = new.env(); e$arg = c(baz = 1L)
    expect_error(argitect("arg=i@{foo,bar}", "A.", envir = e, argv_str = "--arg foo=1"),
        "Unknown key")

    # unnamed vector
    e = new.env(); e$arg = c(1L, 2L)
    expect_error(argitect("arg=i@{foo,bar}", "A.", envir = e, argv_str = "--arg foo=1"),
        "named")

    # wrong type
    e = new.env(); e$arg = c(foo = "a", bar = "b")
    expect_error(argitect("arg=i@{foo,bar}", "A.", envir = e, argv_str = "--arg foo=1"),
        "integer")

    # scalar default with wrong length
    e = new.env(); e$count = c(1L, 2L)
    expect_error(argitect("count=i", "C.", envir = e, argv_str = ""), "scalar")
})


test_that("logical arguments honor their default value", {
    # arg=l with default TRUE, no command-line argument -> TRUE
    e = new.env(); e$arg = TRUE
    argitect("arg=l", "A.", envir = e, argv_str = "")
    expect_true(e$arg)

    # arg! with default TRUE, no command-line argument -> TRUE
    e = new.env(); e$arg = TRUE
    argitect("arg!", "A.", envir = e, argv_str = "")
    expect_true(e$arg)

    # --no-arg overrides the default
    e = new.env(); e$arg = TRUE
    argitect("arg=l", "A.", envir = e, argv_str = "--no-arg")
    expect_false(e$arg)

    # --arg overrides a default FALSE
    e = new.env(); e$arg = FALSE
    argitect("arg=l", "A.", envir = e, argv_str = "--arg")
    expect_true(e$arg)

    # no default -> FALSE
    e = new.env()
    argitect("arg!", "A.", envir = e, argv_str = "")
    expect_false(e$arg)
})


test_that("declared sub-arguments are required, trailing comma opens the set only with a mode", {
    # i%{foo,bar}: both foo and bar are required
    e = new.env()
    expect_error(argitect("arg=i%{foo, bar}", "A.", envir = e, argv_str = "--arg foo=1"),
        "Missing value")

    # i%{foo,bar} with a default merges missing keys
    e = new.env(); e$arg = list(foo = 1L, bar = 2L)
    argitect("arg=i%{foo, bar}", "A.", envir = e, argv_str = "--arg foo=3")
    expect_equal(e$arg, list(foo = 3L, bar = 2L))

    # a trailing comma is rejected when the mode is not determined
    expect_error(argitect:::parse_spec("arg=%{foo=i, bar!,}"), "trailing comma")
    expect_error(argitect:::parse_spec("arg=@{foo, bar,}"), "trailing comma")

    # but it is allowed with an explicit mode, and accepts extra keys
    e = new.env()
    argitect("arg=i@{foo, bar,}", "A.", envir = e, argv_str = "--arg foo=1 bar=2 baz=3")
    expect_equal(e$arg, c(foo = 1L, bar = 2L, baz = 3L))

    e = new.env()
    argitect("arg=i%{foo, bar,}", "A.", envir = e, argv_str = "--arg foo=1 bar=2 baz=3")
    expect_equal(e$arg, list(foo = 1L, bar = 2L, baz = 3L))

    # extra keys are rejected when the set is not open
    e = new.env()
    expect_error(argitect("arg=i%{foo, bar}", "A.", envir = e,
        argv_str = "--arg foo=1 bar=2 baz=3"), "Unknown key")
})


test_that("logical sub-arguments default to FALSE when not specified", {
    # bar! is not specified -> implicitly FALSE
    e = new.env()
    argitect("arg=%{foo=i, bar!}", "A.", envir = e, argv_str = "--arg foo=1")
    expect_equal(e$arg, list(foo = 1L, bar = FALSE))

    # bar is specified -> TRUE
    e = new.env()
    argitect("arg=%{foo=i, bar!}", "A.", envir = e, argv_str = "--arg foo=1 bar")
    expect_equal(e$arg, list(foo = 1L, bar = TRUE))

    # no-bar -> FALSE
    e = new.env()
    argitect("arg=%{foo=i, bar!}", "A.", envir = e, argv_str = "--arg foo=1 no-bar")
    expect_equal(e$arg, list(foo = 1L, bar = FALSE))

    # non-logical sub-arguments are still required
    e = new.env()
    expect_error(argitect("arg=%{foo=i, bar=f}", "A.", envir = e, argv_str = "--arg foo=1"),
        "Missing value")
})


test_that("sub-argument type inherits, is required, or overrides the parent", {
    # parent type + no sub type -> inherit
    e = new.env()
    argitect("arg=i%{foo=@, bar=@}", "A.", envir = e, argv_str = "--arg foo=1,2 bar=3,4")
    expect_equal(e$arg, list(foo = c(1L, 2L), bar = c(3L, 4L)))

    # no parent type + no sub type -> error
    expect_error(argitect:::parse_spec("arg=%{foo, bar}"), "must specify its type")

    # parent type + sub type -> sub overrides parent
    e = new.env()
    argitect("arg=i%{foo=f}", "A.", envir = e, argv_str = "--arg foo=1.5")
    expect_equal(e$arg$foo, 1.5)

    # explicit sub types with an implicit parent
    e = new.env()
    argitect("arg=%{foo=i@, bar=f@}", "A.", envir = e,
        argv_str = "--arg foo=1,2 bar=1.5,2.5")
    expect_equal(e$arg, list(foo = c(1L, 2L), bar = c(1.5, 2.5)))
})


test_that("non-logical logical-argument defaults are coerced with a warning", {
    # arg! with a numeric default
    e = new.env(); e$arg = 1
    expect_warning(argitect("arg!", "A.", envir = e, argv_str = ""),
        "not logical")
    expect_true(e$arg)

    # arg=l with a numeric default 0 -> FALSE
    e = new.env(); e$arg = 0
    expect_warning(argitect("arg=l", "A.", envir = e, argv_str = ""),
        "not logical")
    expect_false(e$arg)

    # a logical default is left untouched and does not warn
    e = new.env(); e$arg = TRUE
    r = withCallingHandlers(
        argitect("arg!", "A.", envir = e, argv_str = ""),
        warning = function(c) {
            warning("unexpected warning: ", conditionMessage(c))
        })
    expect_true(e$arg)
})


test_that("invalid names are rejected before the plain-logical warning", {
    # invalid main name -> error without the "should end with !" warning
    expect_error(suppressWarnings(argitect:::parse_spec("foo*")),
        "valid R variable name")
    # invalid alias -> error without a warning
    expect_error(suppressWarnings(argitect:::parse_spec("foo|bar*")),
        "wrong format")
})


test_that("sub-argument documentation is validated against declared sub-arguments", {
    # all documented: no warning
    expect_warning(capture.output(argitect("arg=i%{foo, bar}", "A.",
        "arg$foo", "Foo.", "arg$bar", "Bar.",
        envir = new.env(), argv_str = "--help")), NA)

    # extra documentation for a non-declared sub-argument
    expect_error(argitect("arg=i%{foo, bar}", "A.",
        "arg$baz", "Baz.",
        envir = new.env(), argv_str = "--help"), "not declared")

    # missing documentation warns
    expect_warning(capture.output(argitect("arg=i%{foo, bar}", "A.",
        "arg$foo", "Foo.",
        envir = new.env(), argv_str = "--help")), "no documented description")

    # documentation for a non-existent argument
    expect_error(argitect("bar=i", "A.",
        "nonexist$foo", "Foo.",
        envir = new.env(), argv_str = "--help"), "is not an")

    # free-form hash accepts any sub-argument documentation
    expect_warning(capture.output(argitect("arg=i%", "A.",
        "arg$anything", "Any.",
        envir = new.env(), argv_str = "--help")), NA)
})


test_that("hash sub-arguments only accept count ranges, not fixed key sets", {
    expect_silent(argitect:::SingleArgument("arg=%{foo=i@{2,3}}", "A."))
    expect_silent(argitect:::SingleArgument("arg=%{foo=i#{2,3}}", "A."))
    expect_silent(argitect:::SingleArgument("arg=%{foo=i@[2]}", "A."))

    expect_error(argitect:::SingleArgument("arg=%{foo=i@{a,b}}", "A."), "fixed key set")
    expect_error(argitect:::SingleArgument("arg=%{foo=i#{a,b}}", "A."), "fixed key set")
    expect_error(argitect:::SingleArgument("arg=%{foo=@(a,b)}", "A."), "fixed key set")

    # a fixed key set is still fine for a top-level vector
    a = argitect:::SingleArgument("foo=i@{a,b}", "F.")
    expect_equal(a$spec_value$arg_range, c("a", "b"))
})
