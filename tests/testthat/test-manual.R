

OPT = new.env()
OPT$print = TRUE

test_argitect = function(spec, argv_str, default = NULL) {
	e = new.env()
	if(!is.null(default)) {
		for(nm in names(default)) {
			assign(nm, default[[nm]], envir = e)
		}
	}
	if(OPT$print) cat("### Explain ###\n")
	if(OPT$print) argitect(spec, "description", explain = TRUE)
	if(OPT$print) cat("\n")
	if(OPT$print) cat("### Help ###\n")
	if(OPT$print) argitect(spec, "description", argv_str = "--help", envir = e)
	if(OPT$print) cat("\n")
	if(OPT$print) cat("### Values ###\n")
	argitect(spec, "description", argv_str = argv_str, envir = e)
	for(var in ls(envir = e)) {
		if(OPT$print) cat(var, ":\n")
		if(OPT$print) print(get(var, envir = e))
	}

	return(invisible(e[[var]]))
}


test_that("whether command-line argument can be parsed", {

	OPT$print = FALSE

	expect_equal( test_argitect("arg!", ""),                   FALSE)
	expect_equal( test_argitect("arg!", "", list(arg = TRUE)), TRUE)
	expect_equal( test_argitect("arg!", "--arg"),              TRUE)
	expect_equal( test_argitect("arg!", "--ar"),               TRUE)
	expect_equal( test_argitect("arg!", "--a"),                TRUE)
	expect_equal( test_argitect("arg!", "-a"),                 TRUE)
	expect_equal( test_argitect("arg!", "--no-arg"),           FALSE)
	expect_equal( test_argitect("arg!", "--no-a"),             FALSE)
	expect_equal( test_argitect("arg!", "--no-arg", list(arg = TRUE)), FALSE)
	expect_error( test_argitect("arg!", "arg", list(arg = TRUE)) )
	expect_equal( test_argitect("arg!", "", list(arg = TRUE)), TRUE)


	expect_equal( test_argitect("arg=l", ""),                   FALSE)
	expect_equal( test_argitect("arg=l", "", list(arg = TRUE)), TRUE)
	expect_equal( test_argitect("arg=l", "--arg"),              TRUE)
	expect_equal( test_argitect("arg=l", "--no-arg"),           FALSE)
	expect_equal( test_argitect("arg=l", "--no-arg", list(arg = TRUE)), FALSE)

	expect_warning( test_argitect("arg=l", "", list(arg = 1)) )
	expect_equal( suppressWarnings(test_argitect("arg=l", "", list(arg = 1))), TRUE )
	expect_equal( suppressWarnings(test_argitect("arg=l", "", list(arg = "a"))), NA )
	expect_equal( suppressWarnings(test_argitect("arg=l", "", list(arg = NA))), NA)
	expect_equal( suppressWarnings(test_argitect("arg=l", "", list(arg = list(1)))), TRUE)
	expect_equal( suppressWarnings(test_argitect("arg=l", "", list(arg = list()))), logical(0))

	expect_equal( test_argitect("arg=i", "--arg 1"),          1L)
	expect_equal( test_argitect("arg=i", "--arg -1"),        -1L)
	expect_equal( test_argitect("arg=i", "-a 1"),          1L)
	expect_equal( test_argitect("arg=i", "-a 1 -a 2"),          2L)
	expect_error( test_argitect("arg=i", "--arg a") )
	expect_equal( test_argitect("arg=i$", "--arg 1"),         1L)
	expect_error( test_argitect("arg=i$", "--arg 1.1") )
	expect_error( test_argitect("arg=i$", "") )
	expect_equal( test_argitect("arg=i", "", list(arg=1L)),   1L)
	expect_equal( test_argitect("arg=i", "--arg 2", list(arg = 1L)), 2L)

	expect_error( test_argitect("arg=i", "--arg 1", list(arg = "a")) )

	expect_equal( test_argitect("arg=i#", "1"), 1L)
	expect_equal( test_argitect("arg=i#", "-1"), -1L)
	expect_equal( test_argitect("arg=i#", "1", list(arg = 0)), 1L)
	expect_equal( test_argitect("arg=i#", "", list(arg = 0)),  0L)
	expect_error( test_argitect("arg=i#", "1 2") )
	expect_equal( test_argitect("arg=i#", "--arg 1"), 1L)

	expect_equal( test_argitect("arg=i#{}", "1 2 3"), 1:3)
	expect_error( test_argitect("arg=i#{}", "1 2 3.1") )
	expect_error( test_argitect("arg=i#{}", "") )
	expect_equal( test_argitect("arg=i#{}", "", list(arg = integer(0))), integer(0) )

	expect_error( test_argitect("arg=i#{2}", "1 2 3") )
	expect_error( test_argitect("arg=i#{2}", "1") )
	expect_equal( test_argitect("arg=i#{2}", "1 2"), 1:2)

	expect_equal( test_argitect("arg=i#{2,4}", "1 2 3"), 1:3)
	expect_error( test_argitect("arg=i#{2,4}", "1") )
	expect_error( test_argitect("arg=i#{2,4}", "1 2 3 4 5 6") )

	expect_equal( test_argitect("arg=i#{foo,bar}", "1 2"), c("foo" = 1L, "bar" = 2L))
	expect_error( test_argitect("arg=i#{foo,bar,}", "1 2 3") )

	expect_equal( test_argitect("arg=i@", "--arg 1 2"), 1:2)
	expect_error( test_argitect("arg=i@", "--arg 1 2.1") )
	expect_error( test_argitect("arg=i@", "--arg 1 a") )

	expect_equal( test_argitect("arg=i@{}", "--arg 1 2"), 1:2)
	expect_error( test_argitect("arg=i@{}", "--arg 1 2.1") )
	expect_error( test_argitect("arg=i@{}", "--arg 1 a") )

	expect_error( test_argitect("arg=i@{3}", "--arg 1 2") )
	expect_equal( test_argitect("arg=i@{3}", "--arg 1 2 3"), 1:3)
	expect_error( test_argitect("arg=i@{3}", "--arg 1 2 3 4") )

	expect_error( test_argitect("arg=i@{3,}", "--arg 1 2") )
	expect_equal( test_argitect("arg=i@{3,}", "--arg 1 2 3"), 1:3)
	expect_equal( test_argitect("arg=i@{3,}", "--arg 1 2 3 4"), 1:4)

	expect_equal( test_argitect("arg=i@{,3}", "--arg 1 2"), 1:2 )
	expect_equal( test_argitect("arg=i@{,3}", "--arg 1 2 3"), 1:3 )
	expect_error( test_argitect("arg=i@{,3}", "--arg 1 2 3 4") )

	expect_error( test_argitect("arg=i@{2,3}", "--arg 1") )
	expect_equal( test_argitect("arg=i@{2,3}", "--arg 1 2 3"), 1:3 )
	expect_error( test_argitect("arg=i@{2,3}", "--arg 1 2 a") )
	expect_error( test_argitect("arg=i@{2,3}", "--arg 1 2 3 4") )

	expect_equal( test_argitect("arg=i@{foo,bar}", "--arg foo=1 bar=2"), c(foo = 1L, bar = 2L) )
	expect_error( test_argitect("arg=i@{foo,bar}", "--arg foo=1 bar=2.1") )
	expect_error( test_argitect("arg=i@{foo,bar}", "--arg foo=1 bar=a") )
	expect_error( test_argitect("arg=i@{foo,bar}", "--arg foo=1") )
	expect_error( test_argitect("arg=i@{foo,bar}", "--arg foo=1 bar=2 xxx=3") )
	expect_equal( test_argitect("arg=i@{foo,bar}", "--arg foo=1", list(arg = c(foo = 0L, bar = 0L))), c(foo = 1L, bar = 0L))
	expect_equal( test_argitect("arg=i@{foo,bar,}", "--arg foo=1 bar=2 xxx=3"), c(foo = 1L, bar = 2L, xxx = 3L) )


	expect_equal( test_argitect("arg=i@{foo|bar}", "--arg foo=1"), c(foo = 1L) )
	expect_equal( test_argitect("arg=i@{foo|bar}", "--arg bar=1"), c(foo = 1L) )
	expect_equal( test_argitect("arg=i@{foo|bar}", "--arg foo=1 bar=2"), c(foo = 2L) )
	expect_equal( test_argitect("arg=i@{foo|bar}", "--arg bar=2 foo=1"), c(foo = 1L) )
	expect_error( test_argitect("arg=i@{foo|bar}", "--arg foo=1 xxx=1") )
	expect_equal( test_argitect("arg=i@{foo|bar,xxx}", "--arg bar=1 xxx=1"), c(foo = 1, xxx = 1L) )

	expect_equal( test_argitect("arg=i%", "--arg foo=1"), list(foo = 1L) )
	expect_error( test_argitect("arg=i%", "--arg foo=1.1") )
	expect_equal( test_argitect("arg=i%", "--arg foo=1", list(arg=list(foo = 0L))), list(foo = 1L) )
	expect_equal( test_argitect("arg=i%", "--arg bar=1", list(arg=list(foo = 0L))), list(bar = 1L, foo = 0L))

	expect_equal( test_argitect("arg=i%{}", "--arg foo=1"), list(foo = 1L) )
	expect_equal( test_argitect("arg=i%{}", "--arg foo=1", list(arg=list(foo = 0L))), list(foo = 1L) )
	expect_equal( test_argitect("arg=i%{}", "--arg bar=1", list(arg=list(foo = 0L))), list(bar = 1L, foo = 0L) )

	expect_equal( test_argitect("arg=i%{foo, bar}", "--arg foo=1 bar=2"), list(foo = 1L, bar = 2L) )
	expect_error( test_argitect("arg=i%{foo, bar}", "--arg foo=1 bar=2.1") )
	expect_error( test_argitect("arg=i%{foo, bar}", "--arg foo=1") )
	expect_equal( test_argitect("arg=i%{foo, bar}", "--arg foo=1", list(arg = list(foo = 0L, bar = 0L))), list(foo = 1L, bar = 0L) )
	expect_error( test_argitect("arg=i%{foo, bar}", "--arg foo=1 bar=2 xxx=3") )
	expect_equal( test_argitect("arg=i%{foo, bar,}", "--arg foo=1 bar=2 xxx=3"), list(foo = 1L, bar = 2L, xxx = 3L) )


	expect_error( test_argitect("arg=%{}", "--arg foo=1 bar=2") )
	expect_error( test_argitect("arg=%{foo, bar}", "--arg foo=1 bar=a") )
	expect_error( test_argitect("arg=%{foo, bar=f}", "--arg foo=1 bar=1.1") )


	expect_equal( test_argitect("arg=%{foo=i, bar=f}", "--arg foo=1 bar=1.1"), list(foo = 1L, bar = 1.1) )
	expect_error( test_argitect("arg=%{foo=i, bar=f}", "--arg foo=1 bar=a") )
	expect_error( test_argitect("arg=%{foo=i, bar=f}", "--arg foo=1,2 bar=1.1") )

	expect_equal( test_argitect("arg=%{foo=i, bar!}", "--arg foo=1 bar"), list(foo = 1L, bar = TRUE) )
	expect_equal( test_argitect("arg=%{foo=i, bar!}", "--arg foo=1 no-bar"), list(foo = 1L, bar = FALSE) )
	expect_equal( test_argitect("arg=%{foo=i, bar!}", "--arg foo=1"), list(foo = 1L, bar = FALSE) )
	expect_equal( test_argitect("arg=%{foo=i, bar!}", "--arg foo=2", list(arg = list(foo = 1L, bar = TRUE))), list(foo = 2L, bar = TRUE) )
	expect_equal( test_argitect("arg=%{foo=i, bar!}", "--arg foo=2 no-bar", list(arg = list(foo = 1L, bar = TRUE))), list(foo = 2L, bar = FALSE) )


	expect_equal( test_argitect("arg=%{foo=i@[2], bar=f@[3]}", "--arg foo=1,2 bar=4.1,5.1,6.1"), list(foo = 1:2, bar = c(4.1, 5.1, 6.1)) )
	expect_error( test_argitect("arg=%{foo=i@[2], bar=f@[3]}", "--arg foo=1,2,3 bar=4.1,5.1,6.1") )
	expect_equal( test_argitect("arg=%{foo=i@{2}, bar=f@{3}}", "--arg foo=1,2 bar=4.1,5.1,6.1"), list(foo = 1:2, bar = c(4.1, 5.1, 6.1)) )
	expect_error( test_argitect("arg=%{foo=i@{2}, bar=f@{3}}", "--arg foo=1,2,3 bar=4.1,5.1,6.1") )


	expect_error( test_argitect("arg=%{foo=@{1,2}}", "--arg foo=1,2") )
	expect_equal( test_argitect("arg=%{foo=i@{1,2}}", "--arg foo=1,2"), list(foo = 1:2) )
	expect_equal( test_argitect("arg=%{foo=i@[1,2]}", "--arg foo=1,2"), list(foo = 1:2) )
	expect_error( test_argitect("arg=%{foo=i@{1,2}}", "--arg foo=1,2.1") )

	expect_equal( test_argitect("arg=%{ foo=i#, bar=s# }", "--arg 1 a"), list(foo = 1L, bar = "a") )
	expect_error( test_argitect("arg=%{ foo=i#, bar=s# }", "--arg a 1") )
	expect_equal( test_argitect("arg=%{ foo=i#, bar=s }", "--arg 1 bar=a"), list(bar = "a", foo = 1L) )
	expect_equal( test_argitect("arg=%{ bar=s, foo=i# }", "--arg bar=a 1"), list(bar = "a", foo = 1L) )
	expect_equal( test_argitect("arg=%{ bar=s, foo=i# }", "--arg 1 bar=a "), list(bar = "a", foo = 1L) )
	expect_equal( test_argitect("arg=%{ foo=i#, bar=s# }", "--arg foo=1 bar=a"), list(foo = 1L, bar = "a") )
	expect_equal( test_argitect("arg=%{ foo=i#, bar=s#{} }", "--arg 1 a b c"), list(foo = 1L, bar = c("a", "b", "c")) )
	expect_equal( test_argitect("arg=%{ foo=i#, bar=s#{} }", "--arg 1 a,b,c"), list(foo = 1L, bar = c("a", "b", "c")) )
	expect_equal( test_argitect("arg=%{ foo=i#{}, bar=s# }", "--arg 1 2 3 a"), list(foo = 1:3, bar = "a") )
	expect_equal( test_argitect("arg=%{ foo=i#{}, bar=s# }", "--arg 1,2,3 a"), list(foo = 1:3, bar = "a") )
	expect_equal( test_argitect("arg=%{ foo=i#{}, bar=s#{} }", "--arg 1 2 3 a b c"), list(foo = 1:3, bar = c("a", "b", "c")) )
	expect_equal( test_argitect("arg=%{ foo=i#{}, bar=s#{} }", "--arg 1,2,3 a,b,c"), list(foo = 1:3, bar = c("a", "b", "c")) )
	expect_equal( test_argitect("arg=%{ foo=i@, bar=s#{} }", "--arg foo=1,2,3 a,b,c"), list(foo = 1:3, bar = c("a", "b", "c")) )
	expect_equal( test_argitect("arg=%{ foo=i@, bar=s#{} }", "--arg foo=1,2,3 a b c"), list(foo = 1:3, bar = c("a", "b", "c")) )
	expect_equal( test_argitect("arg=%{ foo=i#{2}, bar=i#{3} }", "--arg 1 2 3 4 5"), list(foo = 1:2, bar = 3:5) )
	expect_error( test_argitect("arg=%{ foo=i#{}, bar=i#{} }", "--arg 1 2 3 4 5") )

	expect_equal( test_argitect("arg=i%{foo=@, bar=@}", "--arg foo=1,2,3 bar=4,5,6"), list(foo = 1:3, bar = 4:6) )
	expect_equal( test_argitect("arg=%{foo=i@, bar=f@}", "--arg foo=1,2,3 bar=4.1,5.1,6.1"), list(foo = 1:3, bar = c(4.1, 5.1, 6.1)) )
	expect_equal( test_argitect("arg=%{foo=i@, bar=f@}", "--arg foo=1 bar=2.1"), list(foo = 1L, bar = 2.1) )
	expect_equal( test_argitect("arg=i%{foo=f@}", "--arg foo=1.1,1.2"), list(foo = c(1.1, 1.2)) )
	expect_equal( test_argitect("arg=f%{foo=i@}", "--arg foo=1;2;3"), list(foo = 1:3) )


	expect_equal( test_argitect("foo|bar=i", "--foo 1"), 1L )
	expect_equal( test_argitect("foo|bar=i", "--bar 2"), 2L )
	expect_equal( test_argitect("foo|bar=i", "--foo 1 --bar 2"), 2L )
	expect_equal( test_argitect("foo|bar=i", "--foo 1 --bar 2 --foo 2"), 2L )


	expect_equal( test_argitect("foo=i", "--foo 0b11"), 3L )
	expect_equal( test_argitect("foo=i", "--foo 011"), 9L )

	expect_equal( test_argitect("foo_bar=i", "--foo_bar 10"), 10L )
	expect_equal( test_argitect("foo_bar=i", "--foo-bar 10"), 10L )
})


test_that("multiple arguments", {
	e = new.env()
	argitect("option_a|a!", "A",
		     "option_b|b!", "B",
		     "option_c|c!", "C",
		     argv_str = "-a -b -c", envir = e)
	expect_equal(c(e$option_a, e$option_b, e$option_c), c(TRUE, TRUE, TRUE))

	e = new.env()
	argitect("option_a|a!", "A",
		     "option_b|b!", "B",
		     "option_c|c!", "C",
		     argv_str = "-abc", envir = e)
	expect_equal(c(e$option_a, e$option_b, e$option_c), c(TRUE, TRUE, TRUE))

	e = new.env()
	argitect("option_a|a!", "A",
		     "option_b|b!", "B",
		     "option_c|c!", "C",
		     argv_str = "-ab -c", envir = e)
	expect_equal(c(e$option_a, e$option_b, e$option_c), c(TRUE, TRUE, TRUE))

	e = new.env()
	argitect("option_a|a!", "A",
		     "option_b|b!", "B",
		     "abc!", "abs",
		     argv_str = "-a -b -ab", envir = e)
	expect_equal(c(e$option_a, e$option_b, e$abc), c(TRUE, TRUE, TRUE))

	e = new.env()
	expect_error(argitect("option_a|aa!", "A",
		     "option_b|b!", "B",
		     "abc!", "abs",
		     argv_str = "-a", envir = e))

	e = new.env()
	argitect("foo=i#", "foo1",
		     "bar=i#{}", "foo2",
		     argv_str = "1 2 3 4", envir = e)
	expect_equal(e$foo, 1L)
	expect_equal(e$bar, 2:4)

	e = new.env()
	argitect("foo1=i#{2}", "foo1",
		     "foo2=i#{2}", "foo2",
		     "foo3=i#{2}", "foo2",
		     argv_str = "1 2 3 4 5 6", envir = e)
	expect_equal(e$foo1, 1:2)
	expect_equal(e$foo2, 3:4)
	expect_equal(e$foo3, 5:6)

	e = new.env()
	argitect("foo|=i", "", argv_str = "-f 1", envir = e)
	expect_equal(e$foo, 1L)

	e = new.env()
	argitect("foo|h=i", "foo",
		     argv_str = "-h 1", envir = e)
	expect_equal(e$foo, 1L)
	
	e = new.env()
	expect_error( argitect("foo|h=i", "foo",
		     "bar|h=i", "bar",
		     argv_str = "--help", envir = e) )
})

test_that("argument format", {
	expect_error(argitect("@a", ""))
	expect_error(argitect("foo*", ""))
	expect_error(argitect("foo=!", ""))
	expect_error(argitect("foo|-1][=!", ""))
	expect_error(argitect("foo=&{}", ""))
	expect_error(argitect("foo=@{", ""))
	expect_error(argitect("foo=@{a=i}", ""))
	expect_error(argitect("foo=%{a=i@{2)}", "", argv_str = "-h"))
	

	expect_error(argitect("foo!", "", "foo!", "", argv_str = "-h"))
	expect_error(argitect("bar|foo!", "", "foo!", "", argv_str = "-h"))

	# a user-defined help is allowed now
	expect_error(capture.output(argitect("help", "", argv_str = "--help", envir = new.env())), NA)
})

test_that("improper value", {
	expect_error(argitect("foo=i", "", argv_str = "--foo a"), "foo")
	expect_error(argitect("foo=i@{a}", "", argv_str = "--foo 1"), "foo")
	expect_error(argitect("foo=i@{a}", "", argv_str = "--foo b=2"), "foo")
})


capture_help = function(..., envir = parent.frame(), help_style = "one-column") {
    old = options(cli.num_colors = 1)
    on.exit(options(old))
    out = capture.output(argitect(..., envir = envir, argv_str = "--help",
                                  help_style = help_style))
    paste(out, collapse = "\n")
}


test_that("help text", {

capture_help(
    "count=i",  paste("This is a count. This is a count. This is a count.",
                      "This is a count.  This is a count. This is a count."),
    "number=f", paste("This is a number. This is a number. This is a number.",
                      "This is a number. This is a number. This is a number."),
    "array=f@", paste("This is an array. This is an array. This is an array.",
                      "This is an array. This is an array. This is an array."),
    "hash=s%",  paste("This is a hash. This is a hash. This is a hash.",
                      "This is a hash. This is a hash. This is a hash."),
    "verbose!", "Whether show messages",
    "flag!",     "a non-sense option"
) -> output
expect_match(output, "--count, -c integer", fixed = TRUE)
expect_match(output, "--hash {name=character, ...}", fixed = TRUE)
expect_match(output, "--flag, -f, --no-flag", fixed = TRUE)


e = new.env()
e$count = 1
e$number = 0.1
e$array = c(1, 2)
e$hash = list("foo" = "a", "bar" = "b")
e$verbose = TRUE

capture_help(
    "count=i",  paste("This is a count. This is a count. This is a count.",
                      "This is a count.  This is a count. This is a count."),
    "number=f", paste("This is a number. This is a number. This is a number.",
                      "This is a number. This is a number. This is a number."),
    "array=f@", paste("This is an array. This is an array. This is an array.",
                      "This is an array. This is an array. This is an array."),
    "hash=s%",  paste("This is a hash. This is a hash. This is a hash.",
                      "This is a hash. This is a hash. This is a hash."),
    "verbose!", "Whether show messages",
    "flag!",     "a non-sense option",
    envir = e
) -> output
expect_match(output, "[default: 1]", fixed = TRUE)
expect_match(output, "[default: 0.1]", fixed = TRUE)
expect_match(output, "[default: 1, 2]", fixed = TRUE)
expect_match(output, "[default: foo=a, bar=b]", fixed = TRUE)
expect_match(output, "[default: on]", fixed = TRUE)

capture_help(
	"array1=i@{1,2}", "array1",
	"array2=i@{2,}", "array2",
	"array3=i@{,3}", "array3",
	"hash1=i%{foo=@{1,2}}", "hash1",
	envir = e
) -> output
expect_match(output, "--array1 [integer, 1-2]", fixed = TRUE)
expect_match(output, "--array2 [integer, 2+]", fixed = TRUE)
expect_match(output, "--array3 [integer, 1-3]", fixed = TRUE)
expect_match(output, "--hash1 {foo=[integer, 1-2]}", fixed = TRUE)


capture_help(
    "count=i",  paste("This is a count. This is a count. This is a count.",
                      "This is a count.  This is a count. This is a count."),
    "number=f", paste("This is a number. This is a number. This is a number.",
                      "This is a number. This is a number. This is a number."),
    "array=f@", paste("This is an array. This is an array. This is an array.",
                      "This is an array. This is an array. This is an array."),
    "hash=s%",  paste("This is a hash. This is a hash. This is a hash.",
                      "This is a hash. This is a hash. This is a hash."),
    "verbose!", "Whether show messages",
    "flag!",     "a non-sense option",
    envir = e, help_style = "two-column"
) -> output
expect_match(output, "--array, -a [numeric, ...]    This is an array", fixed = TRUE)
expect_match(output, "--help                        Print help message and exit.", fixed = TRUE)
expect_match(output, "                              [default: 1, 2]", fixed = TRUE)



e = new.env()
e$foo = list(name1 = 1, name2 = 2)
capture_help(
    "foo=i%{name1,name2}", paste("This is foo. This is foo. This is foo. This is foo.",
                    "This is foo. This is foo. This is foo. This is foo."),
    "foo$name1", paste("name1 in foo. name1 in foo. name1 in foo. name1 in foo.",
                       "name1 in foo. name1 in foo. name1 in foo. name1 in foo."),
    "foo$name2", paste("name2 in foo. name2 in foo. name2 in foo. name2 in foo.",
                       "name2 in foo. name2 in foo. name2 in foo. name2 in foo."),
    "bar=s%", paste("This is bar. This is bar. This is bar. This is bar.",
                    "This is bar. This is bar. This is bar. This is bar."),
    "bar$name3", paste("name3 in bar. name3 in bar. name3 in bar. name3 in bar.",
                       "name3 in bar. name3 in bar. name3 in bar. name3 in bar."),
    "bar$name4", paste("name4 in bar. name4 in bar. name4 in bar. name4 in bar.",
                       "name4 in bar. name4 in bar. name4 in bar. name4 in bar."),
    envir = e
) -> output
expect_match(output, "name1=integer", fixed = TRUE)
expect_match(output, "Sub named options:", fixed = TRUE)
expect_match(output, "--foo, -f {name1=integer, name2=integer}", fixed = TRUE)



capture_help(
    "foo=i%{name1,name2}", paste("This is foo. This is foo. This is foo. This is foo.",
                    "This is foo. This is foo. This is foo. This is foo."),
    "foo$name1", paste("name1 in foo. name1 in foo. name1 in foo. name1 in foo.",
                       "name1 in foo. name1 in foo. name1 in foo. name1 in foo."),
    "foo$name2", paste("name2 in foo. name2 in foo. name2 in foo. name2 in foo.",
                       "name2 in foo. name2 in foo. name2 in foo. name2 in foo."),
    "bar=s%", paste("This is bar. This is bar. This is bar. This is bar.",
                    "This is bar. This is bar. This is bar. This is bar."),
    "bar$name3", paste("name3 in bar. name3 in bar. name3 in bar. name3 in bar.",
                       "name3 in bar. name3 in bar. name3 in bar. name3 in bar."),
    "bar$name4", paste("name4 in bar. name4 in bar. name4 in bar. name4 in bar.",
                       "name4 in bar. name4 in bar. name4 in bar. name4 in bar."),
    envir = e, help_style = "two-column"
) -> output
expect_match(output, "--foo, -f {name1=integer,     This is foo.", fixed = TRUE)


capture_help(
    help_head = "This is a demonstration of adding usage head and foot.",

    "number=i", "Number of items.",
    "cutoff=f", "Cutoff for filtering results.",
    "verbose!",  "Print message.",

    help_foot = "Please please please please please contact contact contact contact contact contact contact contact contact name@address."
) -> output

expect_match(output, "This is a demonstration of adding usage head and foot.", fixed = TRUE)
expect_match(output, "Please please please please please contact contact contact contact contact\n", fixed = TRUE)


e = new.env()
e$count = 1
e$array = c(0.1, 0.2)
capture_help(
    "--------", "Binary options:",
    "verbose!", "Whether show messages",
    "flag!",     "a non-sense option",

    "-------", "Single-value options:",
    "count=i",  paste("This is a count. This is a count. This is a count.",
                      "This is a count.  This is a count. This is a count."),
    "number=f", paste("This is a number. This is a number. This is a number.",
                      "This is a number. This is a number. This is a number."),
    
    "--------", paste("Multiple-vlaue options: long text long text long text",
                      " long text long text long text long text long text"),
    "array=f@", paste("This is an array. This is an array. This is an array.",
                      "This is an array. This is an array. This is an array."),
    "hash=s%",  paste("This is a hash. This is a hash. This is a hash.",
                      "This is a hash. This is a hash. This is a hash."),
    
    "-------", "Other options:",
    envir = e
) -> output
expect_match(output, "Binary options:", fixed = TRUE)
expect_match(output, "\n\nSingle-value options:", fixed = TRUE)


capture_help(
    "--------", "Binary options:",
    "verbose!", "Whether show messages",
    "flag!",     "a non-sense option",

    "-------", "Single-value options:",
    "count=i",  paste("This is a count. This is a count. This is a count.",
                      "This is a count.  This is a count. This is a count."),
    "number=f", paste("This is a number. This is a number. This is a number.",
                      "This is a number. This is a number. This is a number."),
    
    "--------", paste("Multiple-vlaue options: long text long text long text",
                      " long text long text long text long text long text"),
    "array=f@", paste("This is an array. This is an array. This is an array.",
                      "This is an array. This is an array. This is an array."),
    "hash=s%",  paste("This is a hash. This is a hash. This is a hash.",
                      "This is a hash. This is a hash. This is a hash."),
    
    "-------", "Other options:",
    envir = e, help_style = "two-column"
) -> output
expect_match(output, "--count, -c integer     This is a count.", fixed = TRUE)
expect_match(output, "--array, -a [numeric, ...]    This is an array", fixed = TRUE)


capture_help("
This is a demonstration of using template as the option specification.

Usage: Rscript foo.R [options]

{{ cli_h2('Binary options') }}
  <verbose!> Whether show messages
  <flag!> A non-sense option

Single-value options:
  <count=i> This is a count. This is a count.
  <number=f> This is a number. This is a number. 

Multiple-value options:
  <array=f@> This is an array. This is an array. 
  <hash=s%> This is a hash. This is a hash.

Questions, please contact {{ col_magenta(style_underline(style_italic('your.name@email'))) }}
"
) -> output

expect_match(output, "── Binary options ─", fixed = TRUE)


capture_help("
Usage: Rscript foo.R [options]

  <count=i@{2,}> This is a count. This is a count. This is a count.
  <#count> This is a count. This is a count.
  <number=f> This is a number. This is a number. This is a number.
  <#number> This is a number. This is a number.
"
) -> output
expect_match(output, "--count, -c [integer, 2+]    This is a count.", fixed = TRUE)
expect_match(output, "                             count.", fixed = TRUE)

capture_help("
Usage: Rscript foo.R [options]

  <count=i@{2,}> This is a count. This is a count. This is a count.
                 This is a count. This is a count.
  <number=f> This is a number. This is a number. This is a number.
             This is a number. This is a number.
"
) -> output
expect_match(output, "--count, -c [integer, 2+]    This is a count.", fixed = TRUE)
expect_match(output, "                             count.", fixed = TRUE)


})


capture_subcommand_output = function(...) {
    old = options(cli.num_colors = 1)
    on.exit(options(old))
    out = capture.output(sub_commands(...))
    paste(out, collapse = "\n")
}


test_that("sub command", {

expect_error( sub_commands(
	"sub1", "sub1.R", "This is the description of sub command 1, which will be a long long long text.",
    "sub2", "sub2.R", "This is the description of sub command 2, which will be a long long long text."
) )

expect_error( sub_commands(
	"sub1", system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
    "sub2", "sub2.R", "This is the description of sub command 2, which will be a long long long text."
) )

expect_error( sub_commands(
	"sub1", system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
	"sub2", system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text.",
	argv_str = "-xxx"
) )

capture_subcommand_output(
	"sub1", system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
    "sub2", system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text."
) -> output
expect_match(output, "sub1    This is the description of sub command 1", fixed = TRUE)

capture_subcommand_output(
	system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
    system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text."
) -> output
expect_match(output, "sub1    This is the description of sub command 1", fixed = TRUE)

capture_subcommand_output(
	system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
    system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text.",
    argv_str = "sub1 --help"
) -> output
expect_match(output, "Usage: Rscript foo.R sub1 [options]", fixed = TRUE)

capture_subcommand_output(
	system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
    system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text.",
    argv_str = "--help"
) -> output
expect_match(output, "Usage: Rscript foo.R [command] [options]", fixed = TRUE)


capture_subcommand_output(
    "----", "----", "This is section1",
    "sub1", system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",

    "----", "----", "This is section2",
    "sub2", system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text."
) -> output
expect_match(output, "This is section", fixed = TRUE)


capture_subcommand_output(
    "----", "This is section1",
    system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",

    "----", "This is section2",
    system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text."
) -> output
expect_match(output, "This is section", fixed = TRUE)


capture_subcommand_output(
    help_head = "This is the head of the help message.",

    "sub1", system.file("extdata", "sub1.R", package = "argitect"), 
            "This is the description of sub command 1, which will be a long long long text.",
    "sub2", system.file("extdata", "sub2.R", package = "argitect"),
            "This is the description of sub command 2, which will be a long long long text.",

    help_foot = "This is the foot of the help message."
) -> output
expect_match(output, "This is the head of the help message.", fixed = TRUE)


capture_subcommand_output("
This is the head of the help message.

Usage: Rscript main.R [command] [options]


{{ cli_h2('Section 1') }}
  <sub1={{ system.file('extdata', 'sub1.R', package = 'argitect') }}> This is the description of sub command 1, which will be a long long
          long text.

Section 2
  <longlonglong={{ system.file('extdata', 'sub2.R', package = 'argitect') }}> This is the description of sub command 2, which will be a long long
          long text.

This is the foot of the help message.
"
) -> output
expect_match(output, "── Section 1 ──", fixed = TRUE)

capture_subcommand_output("
This is the head of the help message.

Usage: Rscript main.R [command] [options]

  <sub1={{ system.file('extdata', 'sub1.R', package = 'argitect') }}> This is the description of sub command 1, 
             which will be a long long
          long text.
  <longlonglong={{ system.file('extdata', 'sub2.R', package = 'argitect') }}> This is the description of sub command 2, 
           which will be a long long long text.

This is the foot of the help message.
"
) -> output
expect_match(output, "                long long text.", fixed = TRUE)



main = shQuote(system.file("extdata", "main.R", package = "argitect"))
cmd = paste("Rscript", main)
output = system(cmd, intern = TRUE)
output = paste(output, collapse = "\n")
expect_match(output, "Commands", fixed = TRUE)
expect_match(output, "sub1\\s+This is the description of sub command 1")

cmd = paste("Rscript", main, "sub1 --help")
output = system(cmd, intern = TRUE)
output = paste(output, collapse = "\n")
expect_match(output, "--foo, -f integer", fixed = TRUE)

cmd = paste("Rscript", main, "sub1 --foo 1")
output = system(cmd, intern = TRUE)
output = paste(output, collapse = "\n")
expect_match(output, "foo is 1", fixed = TRUE)


})
