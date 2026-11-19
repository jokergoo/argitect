
# ============================================================
# Sub-command dispatch
# ============================================================

#' Setting sub commands
#'
#' Dispatches to an external R script based on the first command-line
#' argument. The remaining command-line arguments are passed to the selected
#' script.
#'
#' @param ... Specification of commands. See Details.
#' @param help_head Head of the help message.
#' @param help_foot Foot of the help message.
#' @param argv_str A string containing command-line arguments. Only for
#'   testing purposes.
#'
#' @details
#' The specification can be one of the following:
#' \enumerate{
#'   \item A two-column matrix: \code{(script path, description)}. The command
#'         name is the basename of the script path without its extension.
#'   \item A three-column matrix: \code{(command, script path, description)}.
#'   \item A vector with length a multiple of 2:
#'         \code{(script path, description, ...)}.
#'   \item A vector with length a multiple of 3:
#'         \code{(command, script path, description, ...)}.
#'   \item A single string template where commands are marked by the template
#'         tag (default \code{<CODE>}).
#' }
#'
#' Commands can be grouped into sections by a separator line, e.g.
#' \code{"----", "----", "This is a section"}.
#'
#' @return Invisible \code{NULL}.
#'
#' @export
subCommands = function(..., help_head = NULL, help_foot = NULL, argv_str = NULL) {

    IS_UNDER_COMMAND_LINE = !is.null(get_scriptname())

    if(is.null(argv_str)) {
        argv_str = argitect.options("__argv_str__")
    }

    OUT = if(IS_UNDER_COMMAND_LINE || is.null(argv_str)) stderr() else stdout()

    spec_info = parse_sub_commands_spec(list(...))

    script_name = get_scriptname()
    if(is.null(script_name)) script_name = "foo.R" else script_name = basename(script_name)

    if(is.null(argv_str)) {
        ARGV = commandArgs(TRUE)
    } else {
        ARGV = strsplit(argv_str, "\\s+")[[1]]
        if(length(ARGV) == 1 && ARGV[1] == "") ARGV = character(0)
    }

    if(length(ARGV) == 0) {
        print_help_msg_sub_commands(spec_info, file = stdout(), script_name = script_name,
            head = help_head, foot = help_foot)
        if(IS_UNDER_COMMAND_LINE) {
            q(save = "no", status = 0)
        } else {
            return(invisible(NULL))
        }
    }

    cmd = ARGV[1]
    ARGV_string = paste(ARGV[-1], collapse = " ")

    if(grepl("^-", cmd)) {
        if(IS_UNDER_COMMAND_LINE) {
            cat(red(paste0("Error: a command should be specified.\n")), file = OUT)
            print_help_msg_sub_commands(spec_info, file = OUT, script_name = script_name,
                head = help_head, foot = help_foot)
            q(save = "no", status = 127)
        } else {
            stop_wrap("Error: a command should be specified.")
        }
    }

    if(!cmd %in% spec_info$all_commands) {
        if(IS_UNDER_COMMAND_LINE) {
            cat(red(paste0("Error: wrong command: ", cmd, ".\n")), file = OUT)
            print_help_msg_sub_commands(spec_info, file = OUT, script_name = script_name,
                head = help_head, foot = help_foot)
            q(save = "no", status = 127)
        } else {
            stop_wrap("Error: wrong command: ", cmd, ".")
        }
    }

    script = spec_info$spec[spec_info$spec[, 1] == cmd, 2]
    argitect.options("__prefix__" = paste0("Rscript ", script_name, " ", cmd))
    on.exit(argitect.options("__prefix__" = NULL))

    source_script(script, argv_str = ARGV_string)
}


parse_sub_commands_spec = function(spec_list) {
    if(length(spec_list) == 1) {
        spec = spec_list[[1]]
    } else {
        spec = unlist(spec_list, use.names = FALSE)
    }

    template = NULL

    if(is.matrix(spec)) {
        if(ncol(spec) == 2) {
            l_group = grepl("^[-=+#%]*$", spec[, 1])
            if(!all(file.exists(spec[!l_group, 1]))) {
                stop_wrap("The specification is set as a two-column matrix. The first column should be paths of the scripts.")
            }
            cmd = gsub("\\.[^.]*$", "", basename(spec[, 1]))
            spec = cbind(command = cmd, script = spec[, 1], description = spec[, 2])
        } else if(ncol(spec) == 3) {
            l_group = grepl("^[-=+#%]*$", spec[, 1])
            if(!all(file.exists(spec[!l_group, 2]))) {
                stop_wrap("The specification is set as a three-column matrix. The second column should be paths of the scripts.")
            }
            colnames(spec) = c("command", "script", "description")
        } else {
            stop_wrap("If the specification is a matrix, it should be a two-column or three-column matrix.")
        }

    } else if(is.vector(spec)) {
        if(length(spec) == 1) {
            template = spec
            template = sub("^\\n", "", template)
            code = find_code(argitect.options("template_tag"), template)$code
            lt = strsplit(code, "=", fixed = TRUE)
            lt = lapply(lt, function(x) {
                if(length(x) == 1) c(gsub("\\.[^.]*$", "", x), x) else x
            })
            spec = do.call(rbind, lt)
            spec = cbind(spec, description = rep("", nrow(spec)))
            colnames(spec) = c("command", "script", "description")

        } else if(length(spec) %% 6 == 0) {
            # ambiguous between (script, desc) pairs and (command, script, desc) triples
            l = seq_along(spec) %% 2 == 1
            spec2 = cbind(command = gsub("\\.[^.]*$", "", basename(spec[l])),
                          script = spec[l], description = spec[!l])
            l_group = grepl("^[-=+#%]*$", spec2[, 1])
            if(all(file.exists(spec2[!l_group, 2]))) {
                spec = spec2
            } else {
                spec3 = cbind(
                    command = spec[seq_along(spec) %% 3 == 1],
                    script = spec[seq_along(spec) %% 3 == 2],
                    description = spec[seq_along(spec) %% 3 == 0])
                l_group = grepl("^[-=+#%]*$", spec3[, 1])
                if(all(file.exists(spec3[!l_group, 2]))) {
                    spec = spec3
                } else {
                    stop_wrap("Cannot determine whether the specification is (script, description) pairs or (command, script, description) triples.")
                }
            }

        } else if(length(spec) %% 2 == 0) {
            l = seq_along(spec) %% 2 == 1
            spec = cbind(command = gsub("\\.[^.]*$", "", basename(spec[l])),
                         script = spec[l], description = spec[!l])
            l_group = grepl("^[-=+#%]*$", spec[, 1])
            if(!all(file.exists(spec[!l_group, 2]))) {
                stop_wrap("The specification is set as a vector. Elements with odd indices should be paths of the scripts.")
            }

        } else if(length(spec) %% 3 == 0) {
            spec = cbind(
                command = spec[seq_along(spec) %% 3 == 1],
                script = spec[seq_along(spec) %% 3 == 2],
                description = spec[seq_along(spec) %% 3 == 0])
            l_group = grepl("^[-=+#%]*$", spec[, 1])
            if(!all(file.exists(spec[!l_group, 2]))) {
                stop_wrap("The specification is set as a vector. Elements with indices 2, 5, 8, ... should be paths of the scripts.")
            }

        } else {
            stop_wrap("The specification is set as a vector. Its length must be a multiple of 2 or 3, or it is a template.")
        }
    } else {
        stop_wrap("Wrong specification.")
    }

    # detect command groups (separator lines)
    l_group = grepl("^[-=+#%]*$", spec[, 1])
    if(any(l_group)) {
        opt_group = list()
        opt_group_desc = NULL
        i_group = 0
        if(!l_group[1]) {
            opt_group[[1]] = numeric(0)
            opt_group_desc = ""
            i_group = 1
        }
        for(i in seq_along(l_group)) {
            if(l_group[i]) {
                i_group = i_group + 1
                opt_group[[i_group]] = numeric(0)
                opt_group_desc = c(opt_group_desc, spec[i, 3])
            } else {
                opt_group[[i_group]] = c(opt_group[[i_group]], i)
            }
        }
    } else {
        opt_group = list(seq_len(nrow(spec)))
        opt_group_desc = "Commands:"
    }

    if(!is.null(template)) {
        l_group = rep(FALSE, nrow(spec))
    }

    all_commands = spec[!l_group, 1]

    list(spec = spec, template = template, opt_group = opt_group,
         opt_group_desc = opt_group_desc, all_commands = all_commands)
}


print_help_msg_sub_commands = function(spec_info, file = stdout(), script_name = NULL,
    head = NULL, foot = NULL) {

    spec = spec_info$spec
    opt_group = spec_info$opt_group
    opt_group_desc = spec_info$opt_group_desc
    template = spec_info$template

    if(is.null(template)) {
        if(is.null(script_name)) {
            script_name = get_scriptname()
            if(is.null(script_name)) script_name = "foo.R" else script_name = basename(script_name)
        }

        if(!is.null(head)) {
            cat(format_text(head, prefix = NULL), file = file)
            cat("\n\n", file = file)
        }

        cat(paste0("Usage: Rscript ", script_name, " [command] [options]\n"), file = file)
        cat("\n", file = file)

        for(ig in seq_along(opt_group)) {
            ind = opt_group[[ig]]
            if(!grepl("^\\s*$", opt_group_desc[ig])) {
                cat(format_text(opt_group_desc[ig], prefix = NULL), file = file)
                cat("\n", file = file)
            }

            if(length(ind) == 0) next

            opt_width = max(nchar(spec[ind, 1]))
            prefix = strrep(" ", opt_width + 6)
            for(i in ind) {
                cat("  ", spec[i, 1], strrep(" ", opt_width - nchar(spec[i, 1])), "    ",
                    file = file, sep = "")
                desc = strwrap(spec[i, 3], width = max(10, 0.9 * argitect.options("help_width") - opt_width - 6))
                if(length(desc) > 1) {
                    desc[-1] = paste0(prefix, desc[-1])
                }
                cat(paste(desc, collapse = "\n"), file = file)
                cat("\n", file = file)
            }
            cat("\n", file = file)
        }

        if(!is.null(foot)) {
            cat(format_text(foot, prefix = NULL), file = file)
            cat("\n", file = file)
        }

    } else {
        lines = strsplit(template, "\n")[[1]]
        lines2 = NULL
        for(i in seq_along(lines)) {
            if(i == 1) {
                lines2[i] = lines[i]
            } else {
                if(grepl("^\\s{6,}\\w", lines[i], perl = TRUE)) {
                    lines2[length(lines2)] = paste0(lines2[length(lines2)], " ",
                        gsub("^\\s+", "", lines[i]))
                } else {
                    lines2 = c(lines2, lines[i])
                }
            }
        }
        lines = lines2

        lt_parsed = lapply(lines, function(x) {
            res = find_code(argitect.options("template_tag"), x)
            code = res$code
            code = strsplit(code, "=", fixed = TRUE)
            res$cmd = vapply(code, function(s) {
                if(length(s) == 1) gsub("\\.[^.]*$", "", s) else s[1]
            }, character(1))
            res
        })

        all_cmd = unique(unlist(lapply(lt_parsed, function(x) x$cmd)))
        max_cmd_width = max(nchar(all_cmd))

        for(i in seq_along(lines)) {
            lt = lt_parsed[[i]]
            if(length(lt$template) == 0) {
                cat(paste(lines[i], collapse = "\n"), "\n", file = file)
                next
            }

            prefix = strrep(" ", max_cmd_width + 6)
            for(j in seq_along(lt$template)) {
                cmd = lt$cmd[j]
                desc = gsub(lt$template[[j]], "", lines[[i]], fixed = TRUE)
                desc = gsub("^\\s+", "", desc)

                cmd2 = paste0("  ", cmd, strrep(" ", max_cmd_width - nchar(cmd)))
                cat(cmd2, "    ", file = file, sep = "")

                desc = strwrap(desc, width = max(10, 0.9 * argitect.options("help_width") - max_cmd_width - 6))
                if(length(desc) > 1) {
                    desc[-1] = paste0(prefix, desc[-1])
                }
                cat(paste(desc, collapse = "\n"), file = file)
                cat("\n", file = file)
            }
        }
    }
}
