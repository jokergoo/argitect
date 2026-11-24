
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
#'
#' 1. A two-column matrix: `(script path, description)`. The command name is
#'    the basename of the script path without its extension.
#' 2. A three-column matrix: `(command, script path, description)`.
#' 3. A vector with length a multiple of 2: `(script path, description, ...)`.
#' 4. A vector with length a multiple of 3:
#'    `(command, script path, description, ...)`.
#' 5. A single string template where commands are marked by the template tag
#'    (default `<CODE>`).
#'
#' Commands can be grouped into sections by a separator line, e.g.
#' `"----", "----", "This is a section"`.
#'
#' @return Invisible `NULL`.
#'
#' @examples
#' d = tempfile("argitect_sub_")
#' dir.create(d)
#' writeLines(c(
#'     'library(argitect)',
#'     'argitect("x=i", "X.")',
#'     'cat("x =", x, "\\n")'
#' ), file.path(d, "cmd1.R"))
#' writeLines(c(
#'     'library(argitect)',
#'     'argitect("y=s", "Y.")',
#'     'cat("y =", y, "\\n")'
#' ), file.path(d, "cmd2.R"))
#' sub_commands(
#'     file.path(d, "cmd1.R"), "First command",
#'     file.path(d, "cmd2.R"), "Second command",
#'     argv_str = "cmd1 --x 5")
#'
#' @export
sub_commands = function(..., help_head = NULL, help_foot = NULL, argv_str = NULL) {

    # make local variables of the calling frame visible to "{{ ... }}"
    old_glue_envir = .argitect_ctx$glue_envir
    .argitect_ctx$glue_envir = parent.frame()
    on.exit({ .argitect_ctx$glue_envir = old_glue_envir }, add = TRUE)

    IS_UNDER_COMMAND_LINE = !is.null(get_scriptname())

    if(is.null(argv_str)) {
        argv_str = argitect_options("__argv_str__")
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

    # --help (same as giving no argument) and --version are supported on the
    # parent script level
    if(length(ARGV) == 0 || ARGV[1] %in% c("--help", "-h")) {
        print_help_msg_sub_commands(spec_info, file = stdout(), script_name = script_name,
            head = help_head, foot = help_foot)
        if(IS_UNDER_COMMAND_LINE) {
            q(save = "no", status = 0)
        } else {
            return(invisible(NULL))
        }
    }

    if(ARGV[1] %in% c("--version", "-v")) {
        print_version_msg(parent.frame(), file = stdout())
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
            cat(bold(red(paste0("Error: a command should be specified.\n"))), file = OUT)
            print_help_msg_sub_commands(spec_info, file = OUT, script_name = script_name,
                head = help_head, foot = help_foot)
            q(save = "no", status = 127)
        } else {
            stop_wrap("a command should be specified.")
        }
    }

    if(!cmd %in% spec_info$all_commands) {
        if(IS_UNDER_COMMAND_LINE) {
            cat(bold(red(paste0("Error: wrong command: ", cmd, ".\n"))), file = OUT)
            print_help_msg_sub_commands(spec_info, file = OUT, script_name = script_name,
                head = help_head, foot = help_foot)
            q(save = "no", status = 127)
        } else {
            stop_wrap("wrong command: ", cmd, ".")
        }
    }

    script = spec_info$spec[spec_info$spec[, 1] == cmd, 2]
    if(!all(file.exists(script))) {
        stop_wrap("Script '", script[which(!file.exists(script))[1]],
                  "' for command '", cmd, "' does not exist.")
    }
    argitect_options("__prefix__" = paste0("Rscript ", script_name, " ", cmd))
    on.exit(argitect_options("__prefix__" = NULL), add = TRUE)

    source_script(script, argv_str = ARGV_string)
}


# a template code is either "script" or "command=script". A script path may
# itself contain "=" (or other special characters), so "=" is only treated as
# the command/script separator when the part before it is a valid command name.
split_cmd_script = function(code) {
    eq = regexpr("=", code, fixed = TRUE)[1]
    if(eq > 0) {
        cmd = substr(code, 1, eq - 1)
        if(grepl("^[A-Za-z0-9._-]+$", cmd)) {
            return(c(cmd, substr(code, eq + 1, nchar(code))))
        }
    }
    c(gsub("\\.[^.]*$", "", basename(code)), code)
}

# error about script paths in the specification that do not exist
check_missing_scripts = function(spec) {
    l_group = grepl("^[-=+#%]*$", spec[, 1])
    scripts = spec[!l_group, 2]
    missing = scripts[!file.exists(scripts)]
    if(length(missing)) {
        stop_wrap("Script path(s) do not exist: ", paste(missing, collapse = ", "), ".")
    }
    invisible(NULL)
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
            # evaluate "{{ ... }}" chunks before the template is parsed
            template = render_desc(template, .envir = glue_env())
            code = find_code(argitect_options("template_tag"), template)$code
            lt = lapply(code, split_cmd_script)
            spec = do.call(rbind, lt)
            spec = cbind(spec, description = rep("", nrow(spec)))
            colnames(spec) = c("command", "script", "description")
            check_missing_scripts(spec)

        } else if(length(spec) %% 6 == 0) {
            # ambiguous between (script, desc) pairs and (command, script, desc) triples
            l = seq_along(spec) %% 2 == 1
            spec2 = cbind(command = gsub("\\.[^.]*$", "", basename(spec[l])),
                          script = spec[l], description = spec[!l])
            spec3 = cbind(
                command = spec[seq_along(spec) %% 3 == 1],
                script = spec[seq_along(spec) %% 3 == 2],
                description = spec[seq_along(spec) %% 3 == 0])

            # a section marker in the triple form is "(----, ----, title)", i.e.
            # two consecutive separator elements; this is a strong structural hint
            l_sep = grepl("^[-=+#%]*$", spec)
            has_double_sep = length(spec) > 1 && any(l_sep[-length(spec)] & l_sep[-1])

            # score each layout: script paths should look like R scripts and
            # exist; sub-command names are short alphanumeric words (a real
            # command name is unlikely to contain spaces or punctuation)
            score_layout = function(sp) {
                l_group = grepl("^[-=+#%]*$", sp[, 1])
                cmd = sp[!l_group, 1]
                scr = sp[!l_group, 2]
                if(length(scr) == 0) return(0)
                score = 0
                if(all(grepl("\\.R$", scr, ignore.case = TRUE))) score = score + 10
                if(all(file.exists(scr))) score = score + 5
                if(all(grepl("^[A-Za-z0-9._-]+$", cmd))) score = score + 4
                score
            }

            score2 = score_layout(spec2)
            score3 = score_layout(spec3)
            if(has_double_sep) score3 = score3 + 2

            if(score3 > score2) {
                spec = spec3
            } else {
                spec = spec2
            }

            # once the layout is fixed, verify that its scripts exist
            check_missing_scripts(spec)

        } else if(length(spec) %% 2 == 0) {
            l = seq_along(spec) %% 2 == 1
            spec = cbind(command = gsub("\\.[^.]*$", "", basename(spec[l])),
                         script = spec[l], description = spec[!l])
            check_missing_scripts(spec)

        } else if(length(spec) %% 3 == 0) {
            spec = cbind(
                command = spec[seq_along(spec) %% 3 == 1],
                script = spec[seq_along(spec) %% 3 == 2],
                description = spec[seq_along(spec) %% 3 == 0])
            check_missing_scripts(spec)

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

        cat(bold(paste0("Usage: Rscript ", script_name, " [command] [options]\n")), file = file)
        cat("\n", file = file)

        for(ig in seq_along(opt_group)) {
            ind = opt_group[[ig]]
            if(!grepl("^\\s*$", opt_group_desc[ig])) {
                cat(bold(format_text(opt_group_desc[ig], prefix = NULL)), file = file)
                cat("\n", file = file)
            }

            if(length(ind) == 0) next

            opt_width = max(nchar(spec[ind, 1]))
            prefix = strrep(" ", opt_width + 6)
            for(i in ind) {
                cat("  ", spec[i, 1], strrep(" ", opt_width - nchar(spec[i, 1])), "    ",
                    file = file, sep = "")
                desc = strwrap(spec[i, 3], width = max(10, 0.9 * help_widths()$total - opt_width - 6))
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
                if(grepl("^\\s{5,}\\w", lines[i], perl = TRUE)) {
                    lines2[length(lines2)] = paste0(lines2[length(lines2)], " ",
                        gsub("^\\s+", "", lines[i]))
                } else {
                    lines2 = c(lines2, lines[i])
                }
            }
        }
        lines = lines2

        # evaluate "{{ ... }}" chunks (e.g. {{ cli_h2("Section") }}) in the template
        lines = render_desc(lines, .envir = glue_env())

        tag = argitect_options("template_tag")
        n_line = length(lines)

        # first tag on each line, and the command name it stands for
        line_cmd = rep(NA_character_, n_line)
        line_tmp = rep(NA_character_, n_line)
        for(i in seq_len(n_line)) {
            lt = find_code(tag, lines[i])
            if(length(lt$template) == 0) next
            line_cmd[i] = split_cmd_script(lt$code[1])[1]
            line_tmp[i] = lt$template[1]
        }

        # group lines into sections separated by blank lines
        section = integer(n_line)
        sec = 0L
        for(i in seq_len(n_line)) {
            if(i > 1 && grepl("^\\s*$", lines[i - 1])) sec = sec + 1L
            section[i] = sec
        }

        # a section shares one width: the widest command in it
        sec_width = tapply(nchar(line_cmd[!is.na(line_cmd)]),
                           section[!is.na(line_cmd)], max)

        for(i in seq_len(n_line)) {
            if(is.na(line_cmd[i])) {
                cat(lines[i], "\n", file = file)
                next
            }
            s = as.character(section[i])
            cmd_width = if(!is.na(sec_width[s])) sec_width[s] else nchar(line_cmd[i])
            prefix = strrep(" ", cmd_width + 6)

            desc = gsub(line_tmp[i], "", lines[[i]], fixed = TRUE)
            desc = gsub("^\\s+", "", desc)

            cmd2 = paste0("  ", line_cmd[i], strrep(" ", max(0, cmd_width - nchar(line_cmd[i]))))
            cat(cmd2, "    ", file = file, sep = "")

            desc = strwrap(desc, width = max(10, 0.9 * help_widths()$total - cmd_width - 6))
            if(length(desc) > 1) {
                desc[-1] = paste0(prefix, desc[-1])
            }
            cat(paste(desc, collapse = "\n"), file = file)
            cat("\n", file = file)
        }
    }
}
