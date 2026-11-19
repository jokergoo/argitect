
# ============================================================
# Global options for argitect()
# ============================================================

#' Get or set global options for \code{argitect()}
#'
#' Get or set global options used by \code{\link{argitect}}. This function is
#' backed by \code{GlobalOptions::setGlobalOptions}, so options can be read and
#' written with \code{argitect.options("name")},
#' \code{argitect.options("name" = value)}, \code{argitect.options(name = value)}
#' or \code{argitect.options$name}.
#'
#' @param ... Named options to set, or a single option name to get.
#' @param RESET Whether to reset all options to their default values.
#' @param READ.ONLY Passed to \code{GlobalOptions::setGlobalOptions}.
#' @param LOCAL Passed to \code{GlobalOptions::setGlobalOptions}.
#' @param ADD Passed to \code{GlobalOptions::setGlobalOptions}.
#'
#' @details
#' Supported options:
#' \itemize{
#'   \item \code{config} Configuration of the parser, e.g. \code{"bundling"}
#'         or \code{"no_bundling"}.
#'   \item \code{template_tag} The tag identifying specifications in a
#'         template, in the format \code{left_tag CODE right_tag} (default
#'         \code{"<CODE>"}).
#'   \item \code{help_style} The style of the help message:
#'         \code{"one-column"} or \code{"two-column"}.
#'   \item \code{help_width} The maximal width of the help message.
#' }
#'
#' \code{argitect.options(...)} should be put before calling
#' \code{\link{argitect}}.
#'
#' @return The current option value (when getting) or, invisibly, the full
#'   option list (when setting).
#'
#' @export
argitect.options = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
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
    "__argv_str__" = list(.value = NULL,
                          .length = c(0, 1),
                          .visible = FALSE),
    "__script_name__" = list(.value = NULL,
                             .private = TRUE,
                             .visible = FALSE),
    "__prefix__" = list(.value = NULL,
                        .visible = FALSE)
)


# ============================================================
# Script-name helpers
# ============================================================

#' File name of the current script
#'
#' Returns the file name of the currently running R script.
#'
#' @return A character string, or \code{NULL} if the R script is not run from
#'   the command line.
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
        return(f)
    } else {
        return(argitect.options("__script_name__"))
    }
}

#' Directory of the current script
#'
#' Returns the directory of the currently running R script.
#'
#' @return A character string, or \code{NULL} if the R script is not run from
#'   the command line.
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
#' @param ... Passed to \code{\link[base]{source}}.
#' @param argv_str The command-line arguments made available to the script.
#'
#' @return The value returned by \code{\link[base]{source}}, invisibly.
#'
#' @export
source_script = function(file, ..., argv_str = NULL) {
    argitect.options("__argv_str__" = argv_str)
    argitect.options("__script_name__" = file)
    on.exit({
        argitect.options("__script_name__" = NULL)
        argitect.options("__argv_str__" = NULL)
    })

    base::source(file, ...)
}
