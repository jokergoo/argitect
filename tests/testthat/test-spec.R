# Coverage of every specification example listed in test/test.R

parse_ok = function(spec) {
    !inherits(tryCatch(suppressWarnings(argitect:::parse_spec(spec)),
                       error = function(e) e), "error")
}

parse_err = function(spec) {
    inherits(tryCatch(suppressWarnings(argitect:::parse_spec(spec)),
                      error = function(e) e), "error")
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
        "arg=i%",
        "arg=i%{}",
        "arg=i%{foo, bar}",
        "arg=%{}",
        "arg=%{foo, bar}",
        "arg=%{foo=i, bar=f}",
        "arg=%{foo, bar=f}",
        "arg=%{foo=i, bar!}",
        "arg=%{foo=i@, bar=f@}",
        "arg=%{foo=i@[2], bar=f@[3]}",
        "arg=%{foo=@{1,2}}",
        "arg=%{foo=(1,2)}",
        "arg=%{foo=i#, bar=s#}",
        "arg=%{foo=i#, bar=s#{}}"
    )
    for(s in strict_specs) {
        expect_true(parse_ok(s), info = paste("spec should parse:", s))
    }
})


test_that("bare-type and bare-bracket specifications throw errors", {
    error_specs = c(
        "arg=#",
        "arg=@",
        "arg=$",
        "arg=%",
        "arg={}",
        "arg={foo, bar}",
        "arg={foo=i, bar=f}",
        "arg={foo, bar=f}",
        "arg={foo=i@, bar=f@}"
    )
    for(s in error_specs) {
        expect_true(parse_err(s), info = paste("spec should error:", s))
    }
})


test_that("repeat form i{...} is numeric-only", {
    # numeric repeat forms (GetoptLong syntax) are valid
    expect_true(parse_ok("arg=i{3}"))
    expect_true(parse_ok("arg=i{3,}"))
    expect_true(parse_ok("arg=i{,3}"))
    expect_true(parse_ok("arg=i{2,3}"))

    # named values are not allowed in the repeat form (use i@{...} instead)
    expect_true(parse_err("arg=i{foo, bar}"))
})


test_that("hash sub-arguments override the parent mode", {
    e = new.env()
    argitect("arg=i%{foo=f, bar}", "A.", envir = e, argv_str = "--arg foo=1.5 bar=2")
    expect_equal(e$arg$foo, 1.5)
    expect_equal(e$arg$bar, 2L)

    e = new.env()
    argitect("arg=i%{foo=f@, bar}", "A.", envir = e,
        argv_str = "--arg foo=1.5 foo=2.5 bar=3")
    expect_equal(e$arg$foo, c(1.5, 2.5))
    expect_equal(e$arg$bar, 3L)
})


test_that("command-line examples in test.R comments produce the documented values", {
    # arg=%{foo, bar=f}  -> --arg foo=a bar=1.1
    e = new.env()
    argitect("arg=%{foo, bar=f}", "A.", envir = e, argv_str = "--arg foo=a bar=1.1")
    expect_equal(e$arg$foo, "a")
    expect_equal(e$arg$bar, 1.1)

    # arg=%{foo=i, bar!} -> --arg foo=1 bar
    e = new.env()
    argitect("arg=%{foo=i, bar!}", "A.", envir = e, argv_str = "--arg foo=1 bar")
    expect_equal(e$arg$foo, 1L)
    expect_true(e$arg$bar)

    # arg=%{foo=i@, bar=f@} -> --arg foo=1 bar=1.1
    e = new.env()
    argitect("arg=%{foo=i@, bar=f@}", "A.", envir = e, argv_str = "--arg foo=1 bar=1.1")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, 1.1)

    # arg=%{foo=@{1,2}} -> --arg foo=a foo=b
    e = new.env()
    argitect("arg=%{foo=@{1,2}}", "A.", envir = e, argv_str = "--arg foo=a foo=b")
    expect_equal(e$arg$foo, c("a", "b"))

    # arg=%{foo=(1,2)} -> --arg foo=a foo=b
    e = new.env()
    argitect("arg=%{foo=(1,2)}", "A.", envir = e, argv_str = "--arg foo=a foo=b")
    expect_equal(e$arg$foo, c("a", "b"))

    # arg=%{foo=i#, bar=s#} -> --arg 1 a
    e = new.env()
    argitect("arg=%{foo=i#, bar=s#}", "A.", envir = e, argv_str = "--arg 1 a")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, "a")

    # arg=%{foo=i#, bar=s#{}} -> --arg 1 2 a b
    e = new.env()
    argitect("arg=%{foo=i#, bar=s#{}}", "A.", envir = e, argv_str = "--arg 1 2 a b")
    expect_equal(e$arg$foo, 1L)
    expect_equal(e$arg$bar, c("2", "a", "b"))
})


test_that("scalar, vector, positional and repeat types parse command lines", {
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

    # arg=i@{foo, bar} (vector with named allowed values)
    e = new.env()
    argitect("arg=s@{red, green, blue}", "A.", envir = e, argv_str = "--arg red green")
    expect_equal(e$arg, c("red", "green"))

    # arg=i@{foo|bar} (named allowed value with alias)
    e = new.env()
    argitect("arg=s@{red|r, green|g}", "A.", envir = e, argv_str = "--arg red green")
    expect_equal(e$arg, c("red", "green"))
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

    # arg=%{} (character hash)
    e = new.env()
    argitect("arg=%{}", "A.", envir = e, argv_str = "--arg a=x b=y")
    expect_equal(e$arg, list(a = "x", b = "y"))

    # arg=%{foo, bar} (character hash with sub-arguments)
    e = new.env()
    argitect("arg=%{foo, bar}", "A.", envir = e, argv_str = "--arg foo=x bar=y")
    expect_equal(e$arg, list(foo = "x", bar = "y"))

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
