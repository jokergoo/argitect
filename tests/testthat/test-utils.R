test_that("get script name and get script dir", {
	file = system.file("extdata", "foo.R", package = "argitect")
	# use system() rather than pipe(): pipe() does not handle quoted paths on Windows
	output = system(paste(shQuote(rscript_bin()), shQuote(file)), intern = TRUE)
	expect_true(length(output) >= 2)
	if(length(output) < 2) return(invisible(NULL))
	# normalizePath() uses "\\" on Windows, so compare normalized forms
	expect_equal(normalizePath(output[1], winslash = "/"), normalizePath(file, winslash = "/"))
	expect_equal(normalizePath(output[2], winslash = "/"), normalizePath(dirname(file), winslash = "/"))

	expect_equal(get_scriptname(), NULL)
	expect_equal(get_scriptdir(), NULL)
})


test_that("color helpers are backed by cli", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    expect_identical(as.character(argitect:::red("x")), "x")
    expect_identical(as.character(argitect:::bold("x")), "x")

    options(cli.num_colors = 256)
    expect_true(grepl("\033\\[", argitect:::red("x")))
    expect_true(grepl("\033\\[", argitect:::bold("x")))
})


test_that("descriptions are rendered through glue", {
    expect_identical(argitect:::render_desc("plain text"), "plain text")
    expect_identical(argitect:::render_desc("value = {{1 + 1}}"), "value = 2")
    # single braces are literal now
    expect_identical(argitect:::render_desc("set {1, 2}"), "set {1, 2}")
})
