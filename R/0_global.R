
# ============================================================
# Global options for argitect()
# ============================================================

#' Get or set global options for `argitect()`
#'
#' Get or set global options used by [`argitect()`]. This function is backed by
#' [`GlobalOptions::setGlobalOptions()`], so options can be read and written with
#' `argitect.options("name")`, `argitect.options("name" = value)`,
#' `argitect.options(name = value)` or `argitect.options$name`.
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
#' - `config` A character vector of parser configuration flags. The following
#'   values are recognized:
#'     - `"bundling"`: allow single-letter flags to be combined, so that
#'       `-abc` is the same as `-a -b -c` (this is the default behavior).
#'     - `"no_bundling"`: disable bundling; `-abc` is then treated as a single
#'       (unknown) option named `abc`.
#'     - `"ignore_case"`: match long option names case-insensitively, so that
#'       `--FOO` matches `--foo` (by default matching is case-sensitive).
#'     - `"no_ignore_case"`: keep case-sensitive matching; if both
#'       `"ignore_case"` and `"no_ignore_case"` are given, the latter wins.
#'
#'   For example `argitect.options("config" = c("ignore_case", "no_bundling"))`.
#' - `template_tag` The tag identifying specifications in a template, in the
#'   format "`left_tag CODE right_tag`" (default `"<CODE>"`).
#' - `help_style` The style of the help message: `"one-column"` or
#'   `"two-column"`.
#' - `help_width` The maximal width of the help message.
#' - `value_sep` The separator used to split multiple values inside a single
#'   `name=value` pair for a vector sub-argument. The default `"auto"`
#'   automatically detects the separator among `","`, `";"`, `":"`, `"|"` and
#'   `"/"` by picking the one that satisfies the type and count constraints.
#'
#' `argitect.options(...)` should be put before calling [`argitect()`].
#'
#' @return The current option value (when getting) or, invisibly, the full
#'   option list (when setting).
#'
#' @examples
#' argitect.options("help_width")
#' old = argitect.options("help_width")
#' argitect.options("help_width" = 100)
#' argitect.options("help_width")
#' argitect.options("help_width" = old)
#'
#' @export
argitect.options = setGlobalOptions(
    config = list(.value = NULL,
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
    "help_width" = 90,
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

#' @rdname argitect.options
#' @export
argitect_options = argitect.options


# ============================================================
# Script-name helpers
# ============================================================

#' File name of the current script
#'
#' Returns the file name of the currently running R script.
#'
#' @return A character string, or `NULL` if the R script is not run from the
#'   command line.
#'
#' @examples
#' # NULL when the script is not run from the command line
#' get_scriptname()
#'
#' @export
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
        sn = argitect.options("__script_name__")
        if(is.null(sn)) return(NULL)
        return(normalizePath(sn))
    }
}

#' Directory of the current script
#'
#' Returns the directory of the currently running R script.
#'
#' @return A character string, or `NULL` if the R script is not run from the
#'   command line.
#'
#' @examples
#' get_scriptdir()
#'
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
    argitect.options("__argv_str__" = argv_str)
    argitect.options("__script_name__" = file)
    on.exit({
        argitect.options("__script_name__" = NULL)
        argitect.options("__argv_str__" = NULL)
    })

    # in the interactive mode, command-line arguments are provided by argitect.options("__argv_str__")
    # a caught "argitect_exit" means --help / --version was handled and the script should stop
    tryCatch(base::source(file, ...), argitect_exit = function(e) invisible(NULL))
}
