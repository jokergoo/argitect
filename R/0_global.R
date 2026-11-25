
# ============================================================
# Global options for argitect()
# ============================================================

# internal context: the environment where "{{ ... }}" chunks in templates and
# descriptions are evaluated. argitect() and sub_commands() set it to their
# calling frame, so that local variables are visible to glue interpolation.
.argitect_ctx = new.env(parent = emptyenv())
.argitect_ctx$glue_envir = NULL

#' Get or set global options for `argitect()`
#'
#' Get or set global options used by [`argitect()`]. This function is backed by
#' [`GlobalOptions::setGlobalOptions()`], so options can be read and written with
#' `argitect_options("name")`, `argitect_options(name = value)` or `argitect_options$name`.
#'
#' @param ... Named options to set, or a single option name to get.
#' @param RESET Whether to reset all options to their default values.
#' @param READ.ONLY Passed to [`GlobalOptions::setGlobalOptions()`].
#' @param LOCAL Passed to [`GlobalOptions::setGlobalOptions()`].
#' @param ADD Passed to [`GlobalOptions::setGlobalOptions()`].
#'
#' @details
#' Supported options:
#'
#' - `config` A character vector of parser configuration flags (default
#'   `c("bundling", "no_ignore_case")`). The following values are recognized:
#'     - `"bundling"`: allow single-letter flags to be combined, so that
#'       `-abc` is the same as `-a -b -c` (this is the default behavior).
#'     - `"no_bundling"`: disable bundling; `-abc` is then treated as a single
#'       (unknown) option named `abc`.
#'     - `"ignore_case"`: match long option names case-insensitively, so that
#'       `--FOO` matches `--foo` (by default matching is case-sensitive).
#'     - `"no_ignore_case"`: keep case-sensitive matching; if both
#'       `"ignore_case"` and `"no_ignore_case"` are given, the latter wins.
#'
#'   For example `argitect_options("config" = c("ignore_case", "no_bundling"))`.
#' - `template_tag` The tag identifying specifications in a template, in the
#'   format "`left_tag CODE right_tag`" (default `"<CODE>"`).
#' - `help_style` The style of the help message: `"one-column"` or
#'   `"two-column"`.
#' - `help_width` The width of the help message, as a vector of one or two
#'   numbers (default `c(90, 40)`). The first number is the total width; the
#'   one-column style uses only it. The second number is the width of the
#'   first (option) column used by the two-column style. Option names that are
#'   too long are wrapped, and continuation lines are aligned with the second
#'   option name.
#' - `value_sep` The separator used to split multiple values inside a single
#'   `name=value` pair for a vector sub-argument. The default `"auto"`
#'   automatically detects the separator among `","`, `";"`, `":"`, `"|"` and
#'   `"/"` by picking the one that satisfies the type and count constraints.
#'
#' `argitect_options(...)` should be put before calling [`argitect()`] or [`sub_commands()`].
#'
#' @return The current option value (when getting) or, invisibly, the full
#'   option list (when setting).
#'
#' @examples
#' argitect_options("help_width")
#' old = argitect_options("help_width")
#' argitect_options("help_width" = 100)
#' argitect_options("help_width")
#' argitect_options(RESET = TRUE)
#' argitect_options
#'
#' @export
argitect_options = setGlobalOptions(
    config = list(.value = c("bundling", "no_ignore_case"),
                  .class = "character"),
    template_tag = list(.value = "<CODE>",
                        .length = 1,
                        .validate = function(x) grepl("CODE", x)),
    help_style = list(.value = "one-column",
                      .length = 1,
                      .filter = function(x) {
                          if(x == "one column") {
                              x = "one-column"
                          } else if(x == "one_column") {
                              x = "one-column"
                          } else if(x == "two column") {
                              x = "two-column"
                          } else if(x == "two_column") {
                              x = "two-column"
                          } else if(x == "two columns") {
                              x = "two-column"
                          } else if(x == "two_columns") {
                              x = "two-column"
                          }
                          x
                      },
                      .validate = function(x) x %in% c("one-column", "two-column"),
                      .failed_msg = "help_style should be one of 'one-column' and 'two-column'"),
    "help_width" = list(.value = c(90, 40),
                        .filter = function(x) {
                            if(length(x) == 1) {
                                x = c(x, round(x*4/9))
                            } else if(length(x) > 2) {
                                x = x[1:2]
                            }
                            x
                        },
                        .class = "numeric"),
    value_sep = list(.value = "auto",
                     .length = 1,
                     .class = "character"),
    "__argv_str__" = list(.value = NULL,
                          .length = c(0, 1),
                          .visible = FALSE),
    "__script_name__" = list(.value = NULL,
                             .private = TRUE,
                             .visible = FALSE),
    "__prefix__" = list(.value = NULL,
                        .visible = FALSE)
)

#' @rdname argitect_options
#' @export
argitect.options = argitect_options


# ============================================================
# Script-name helpers
# ============================================================

#' File or directory name of the current script
#'
#' Returns the file or directory name of the currently running R script.
#'
#' @return A character string, or `NULL` if the R script is not run from the
#'   command line.
#'
#' @examples
#' # NULL when the script is not run from the command line
#' get_scriptname()
#' get_scriptdir()
#' 
#' f = tempfile(fileext = ".R")
#' writeLines(c(
#'     'library(argitect)',
#'     'cat(get_scriptname(), "\n")',
#'     'cat(get_scriptdir(),  "\n")'
#' ), f)
#' system(paste("Rscript", f), intern = TRUE)
#'
#' @export
#' @rdname get_scriptname
get_scriptname = function() {
    args = commandArgs()

    if(length(args) == 1) {
        return(NULL)
    }
    f = grep("^--file=", args, value = TRUE)
    if(length(f)) {
        f = gsub("^--file=(.*)$", "\\1", f[1])
        f = normalizePath(f)
        return(f)
    } else {
        sn = argitect_options("__script_name__")
        if(is.null(sn)) return(NULL)
        return(normalizePath(sn))
    }
}

#' @rdname get_scriptname
#' @export
get_scriptdir = function() {
    args = commandArgs()

    f = grep("^--file=", args, value = TRUE)
    if(length(f)) {
        f = gsub("^--file=(.*)$", "\\1", f[1])
        return(dirname(normalizePath(f)))
    } else {
        return(NULL)
    }
}

#' Source an R script with command-line arguments
#'
#' Sources an R script while making command-line arguments available to it.
#' This is convenient for running external R scripts with different
#' command-line arguments from within an interactive R session.
#'
#' @param file The R script to source.
#' @param ... Passed to [`base::source()`].
#' @param argv_str The command-line arguments made available to the script.
#'
#' @return The value returned by [`base::source()`], invisibly.
#'
#' @examples
#' f = tempfile(fileext = ".R")
#' writeLines(c(
#'     'library(argitect)',
#'     'argitect("count=i", "Number of items.")',
#'     'cat("count =", count, "\\n")'
#' ), f)
#' source_script(f, argv_str = "--count 3")
#'
#' @export
source_script = function(file, ..., argv_str = NULL) {
    argitect_options("__argv_str__" = argv_str)
    argitect_options("__script_name__" = file)
    on.exit({
        argitect_options("__script_name__" = NULL)
        argitect_options("__argv_str__" = NULL)
    })

    # in the interactive mode, command-line arguments are provided by argitect_options("__argv_str__")
    # a caught "argitect_exit" means --help / --version was handled and the script should stop
    tryCatch(base::source(file, ...), argitect_exit = function(e) invisible(NULL))
}
