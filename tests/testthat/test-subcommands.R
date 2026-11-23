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

test_that("sub_commands dispatches to the correct script", {
    d = tempfile("argitect_sub_")
    make_sub_scripts(d)

    spec = c(file.path(d, "cmd1.R"), "First command",
             file.path(d, "cmd2.R"), "Second command")

    out = capture.output(sub_commands(spec, argv_str = "cmd1 --x 5 --verbose"))
    expect_true(any(grepl("CMD1 x=5 verbose=TRUE", out, fixed = TRUE)))

    out = capture.output(sub_commands(spec, argv_str = "cmd2 --y hello"))
    expect_true(any(grepl("CMD2 y=hello", out, fixed = TRUE)))
})

test_that("sub_commands prints help when no command is given", {
    d = tempfile("argitect_sub_")
    make_sub_scripts(d)

    spec = c(file.path(d, "cmd1.R"), "First command",
             file.path(d, "cmd2.R"), "Second command")

    out = capture.output(sub_commands(spec, argv_str = ""))
    expect_true(any(grepl("Usage:", out)))
    expect_true(any(grepl("cmd1", out)))
})

test_that("sub_commands reports unknown command", {
    d = tempfile("argitect_sub_")
    make_sub_scripts(d)

    spec = c(file.path(d, "cmd1.R"), "First command",
             file.path(d, "cmd2.R"), "Second command")

    expect_error(sub_commands(spec, argv_str = "nope"), "wrong command")
})


test_that("a sub-command script stops quietly after --help", {
    d = tempfile("argitect_sub_help_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines(c(
        'library(argitect)',
        'argitect("foo=i", "Set value for foo.")',
        'cat("AFTER_HELP", "\n")'),
        file.path(d, "cmd1.R"))
    writeLines('argitect("y=s", "Y.")', file.path(d, "cmd2.R"))

    old = options(cli.num_colors = 1); on.exit(options(old))
    out = capture.output(sub_commands(
        c(file.path(d, "cmd1.R"), "First cmd",
          file.path(d, "cmd2.R"), "Second cmd"), argv_str = "cmd1 --help"))

    # help is printed, but the rest of the sub-script never runs
    expect_true(any(grepl("Set value for foo", out)))
    expect_false(any(grepl("AFTER_HELP", out)))
})


test_that("sub_commands supports --help and --version", {
    d = tempfile("argitect_sub_hv_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines('argitect("x=i", "X.")', file.path(d, "cmd1.R"))
    writeLines('argitect("y=s", "Y.")', file.path(d, "cmd2.R"))
    spec = c(file.path(d, "cmd1.R"), "First cmd", file.path(d, "cmd2.R"), "Second cmd")

    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)

    # --help is identical to giving no argument
    out0 = strip(capture.output(sub_commands(spec, argv_str = "")))
    outh = strip(capture.output(sub_commands(spec, argv_str = "--help")))
    expect_identical(out0, outh)
    expect_true(any(grepl("Commands:", outh)))

    # --version prints VERSION from the calling environment
    VERSION = "9.8.7"
    outv = capture.output(sub_commands(spec, argv_str = "--version"))
    expect_identical(outv, "9.8.7")

    # -h and -v aliases work too
    outh2 = strip(capture.output(sub_commands(spec, argv_str = "-h")))
    expect_identical(out0, outh2)
    outv2 = capture.output(sub_commands(spec, argv_str = "-v"))
    expect_identical(outv2, "9.8.7")
})


test_that("sub_commands supports triple-form section markers", {
    d = tempfile("argitect_sub_sec1_")
    make_sub_scripts(d)
    spec = c("----", "----", "Section one",
             "cmd1", file.path(d, "cmd1.R"), "First command",
             "----", "----", "Section two",
             "cmd2", file.path(d, "cmd2.R"), "Second command")

    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)
    out = strip(capture.output(sub_commands(spec, argv_str = "")))

    expected = c(
        "Usage: Rscript foo.R [command] [options]",
        "",
        "Section one",
        "  cmd1    First command",
        "",
        "Section two",
        "  cmd2    Second command",
        ""
    )
    expect_identical(out, expected)
})


test_that("sub_commands supports pair-form section markers", {
    d = tempfile("argitect_sub_sec2_")
    make_sub_scripts(d)
    spec = c("----", "Section one",
             file.path(d, "cmd1.R"), "First command",
             "----", "Section two",
             file.path(d, "cmd2.R"), "Second command")

    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)
    out = strip(capture.output(sub_commands(spec, argv_str = "")))

    expected = c(
        "Usage: Rscript foo.R [command] [options]",
        "",
        "Section one",
        "  cmd1    First command",
        "",
        "Section two",
        "  cmd2    Second command",
        ""
    )
    expect_identical(out, expected)
})


test_that("triple-form section markers need existing scripts", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)
    d = tempfile("argitect_sub_sec3_")
    make_sub_scripts(d)

    out = strip(capture.output(sub_commands(
        "----", "----", "This is section1",
        "cmd1", file.path(d, "cmd1.R"), "First desc",
        "----", "----", "This is section2",
        "cmd2", file.path(d, "cmd2.R"), "Second desc",
        argv_str = "")))
    expect_true(any(grepl("^This is section1$", out)))
    expect_true(any(grepl("^This is section2$", out)))
    expect_true(any(grepl("^  cmd1 +First desc$", out)))
    expect_true(any(grepl("^  cmd2 +Second desc$", out)))

    # missing scripts raise an error
    expect_error(sub_commands(
        "----", "----", "This is section1",
        "sub1", "sub1.R", "First desc",
        "----", "----", "This is section2",
        "sub2", "sub2.R", "Second desc",
        argv_str = ""), "do not exist")
})


test_that("6-element vectors choose pairs vs triples by R-script paths", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)
    d = tempfile("argitect_sub_6el_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines('cat("a\\n")', file.path(d, "a.R"))
    writeLines('cat("b\\n")', file.path(d, "b.R"))

    # triples: (command, script, description)
    out = strip(capture.output(sub_commands(
        "sub1", file.path(d, "a.R"), "First desc",
        "sub2", file.path(d, "b.R"), "Second desc",
        argv_str = "")))
    expect_true(any(grepl("^  sub1 +First desc$", out)))
    expect_true(any(grepl("^  sub2 +Second desc$", out)))

    # pairs: (script, description)
    out2 = strip(capture.output(sub_commands(
        file.path(d, "a.R"), "desc a",
        file.path(d, "b.R"), "desc b",
        argv_str = "")))
    expect_true(any(grepl("^  a +desc a$", out2)))
    expect_true(any(grepl("^  b +desc b$", out2)))
})


test_that("6-element vectors with non-.R scripts fall back on command names", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)
    d = tempfile("argitect_sub_nonr_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines('cat("a\\n")', file.path(d, "run1"))
    writeLines('cat("b\\n")', file.path(d, "run2"))

    out = strip(capture.output(sub_commands(
        "sub1", file.path(d, "run1"), "long description with spaces here",
        "sub2", file.path(d, "run2"), "another long description here",
        argv_str = "")))

    expect_true(any(grepl("^  sub1 +long description with spaces here$", out)))
    expect_true(any(grepl("^  sub2 +another long description here$", out)))
})


test_that("missing sub-command scripts raise an error", {
    old = options(cli.num_colors = 1); on.exit(options(old))

    expect_error(sub_commands(
        "sub1", "sub1.R", "First desc",
        "sub2", "sub2.R", "Second desc",
        argv_str = ""), "do not exist")

    d = tempfile("argitect_sub_exists_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines('cat("a\\n")', file.path(d, "a.R"))
    writeLines('cat("b\\n")', file.path(d, "b.R"))
    expect_error(capture.output(sub_commands(
        file.path(d, "a.R"), "desc a",
        file.path(d, "b.R"), "desc b",
        argv_str = "")), NA)
})


test_that("sub_commands supports templates with glue interpolation", {
    d = tempfile("argitect_sub_tmpl_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines('cat("c1\\n")', file.path(d, "sub1.R"))
    writeLines('cat("c2\\n")', file.path(d, "sub2.R"))

    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)

    tmpl = paste0(
        "Head.\n",
        "\n",
        "Usage: Rscript main.R [command] [options]\n",
        "\n",
        "{{ cli_h2('Section 1') }}\n",
        "  <sub1=", file.path(d, "sub1.R"), "> First description.\n",
        "\n",
        "Section 2\n",
        "  <sub2=", file.path(d, "sub2.R"), "> Second description.\n",
        "\n",
        "Foot.\n")
    out = strip(capture.output(sub_commands(tmpl)))

    expect_true(any(grepl("Head", out)))
    expect_true(any(grepl("Section 1", out)))
    expect_true(any(grepl("^  sub1 +First description\\.$", out)))
    expect_true(any(grepl("Section 2", out)))
    expect_true(any(grepl("^  sub2 +Second description\\.$", out)))
    expect_true(any(grepl("Foot", out)))

    # no raw glue expression should leak through
    expect_false(any(grepl("\\{\\{", out)))
})


test_that("sub_commands template errors about missing scripts", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    tmpl = paste0("Usage: Rscript main.R [command] [options]\n",
        "\n", "  <sub1=sub1.R> First description.\n")
    expect_error(sub_commands(tmpl), "do not exist")
})


test_that("sub_commands template evaluates glue before parsing tags", {
    old = options(cli.num_colors = 1); on.exit(options(old))
    strip = function(x) sub("[ \t]+$", "", x)

    tmpl = paste0(
        "Usage: Rscript main.R [command] [options]\n",
        "\n",
        "  <sub1={{ system.file('extdata', 'sub1.R', package = 'argitect') }}> Sub1 desc.\n")
    out = strip(capture.output(sub_commands(tmpl)))

    expect_true(any(grepl("^  sub1 +Sub1 desc\\.$", out)))
    expect_false(any(grepl("\\{\\{", out)))
})


test_that("wrong-command error is not double-prefixed", {
    d = tempfile("argitect_sub_err_")
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
    writeLines('cat("a\\n")', file.path(d, "a.R"))
    err = tryCatch(sub_commands(c(file.path(d, "a.R"), "A"), argv_str = "nope"),
        error = function(e) conditionMessage(e))
    expect_match(err, "wrong command")
    expect_false(grepl("^Error:", err))
})
