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

test_that("GetoptLong repeat syntax i{...} works", {
    e = new.env()
    argitect("tag=i{2,4}", "T.", envir = e, argv_str = "--tag 1 2 3")
    expect_equal(e$tag, c(1L, 2L, 3L))

    e = new.env()
    argitect("tag=i{2}", "T.", envir = e, argv_str = "--tag 1 2")
    expect_equal(e$tag, c(1L, 2L))

    e = new.env()
    argitect("tag=i{,2}", "T.", envir = e, argv_str = "--tag 1 2 3")
    expect_equal(e$tag, c(1L, 2L))

    e = new.env()
    expect_error(argitect("tag=i{2}", "T.", envir = e, argv_str = "--tag 1 --tag 2"), "integer")

    e = new.env()
    expect_error(argitect("tag=i{2,}", "T.", envir = e, argv_str = "--tag 1"), "Insufficient")
})

test_that("extended integer mode o works", {
    e = new.env()
    argitect("tag=o", "T.", envir = e, argv_str = "--tag 0b001001")
    expect_equal(e$tag, 9L)

    e = new.env()
    argitect("tag=o", "T.", envir = e, argv_str = "--tag 0xaf2")
    expect_equal(e$tag, 2802L)
})
