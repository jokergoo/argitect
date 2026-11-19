# Sub-command dispatch tests

make_sub_scripts = function(dir) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    writeLines(c(
        'argitect("x=i", "X value.", "verbose!", "V.")',
        'cat("CMD1 x=", x, " verbose=", verbose, "\n", sep = "")'
    ), file.path(dir, "cmd1.R"))
    writeLines(c(
        'argitect("y=s", "Y value.")',
        'cat("CMD2 y=", y, "\n", sep = "")'
    ), file.path(dir, "cmd2.R"))
    invisible(dir)
}

test_that("subCommands dispatches to the correct script", {
    d = tempfile("argitect_sub_")
    make_sub_scripts(d)

    spec = c(file.path(d, "cmd1.R"), "First command",
             file.path(d, "cmd2.R"), "Second command")

    out = capture.output(subCommands(spec, argv_str = "cmd1 --x 5 --verbose"))
    expect_true(any(grepl("CMD1 x=5 verbose=TRUE", out, fixed = TRUE)))

    out = capture.output(subCommands(spec, argv_str = "cmd2 --y hello"))
    expect_true(any(grepl("CMD2 y=hello", out, fixed = TRUE)))
})

test_that("subCommands prints help when no command is given", {
    d = tempfile("argitect_sub_")
    make_sub_scripts(d)

    spec = c(file.path(d, "cmd1.R"), "First command",
             file.path(d, "cmd2.R"), "Second command")

    out = capture.output(subCommands(spec, argv_str = ""))
    expect_true(any(grepl("Usage:", out)))
    expect_true(any(grepl("cmd1", out)))
})

test_that("subCommands reports unknown command", {
    d = tempfile("argitect_sub_")
    make_sub_scripts(d)

    spec = c(file.path(d, "cmd1.R"), "First command",
             file.path(d, "cmd2.R"), "Second command")

    expect_error(subCommands(spec, argv_str = "nope"), "wrong command")
})
