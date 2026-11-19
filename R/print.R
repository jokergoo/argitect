
# ============================================================
# Help messages
# ============================================================

# human-readable name of a mode
mode_label = function(mode) {
    c("i" = "integer", "f" = "numeric", "s" = "character",
      "l" = "logical", "o" = "extended_integer")[[mode]]
}

mode_abbr = function(mode) {
    c("i" = "int", "f" = "num", "s" = "chr", "l" = "logical", "o" = "int")[[mode]]
}


# the "--name, -n" part of a help line
SingleArgument$methods(
    opt_name_line = function() {
        parts = character(0)
        for(nm in .self$arg_name) {
            if(nchar(nm) == 1) {
                parts = c(parts, paste0("-", nm))
            } else {
                parts = c(parts, paste0("--", gsub("_", "-", nm)))
            }
        }
        paste(parts, collapse = ", ")
    }
)


# the type suffix: "integer", "[integer, ...]", "{name=integer, ...}"
SingleArgument$methods(
    type_suffix = function() {
        sv = .self$spec_value
        type = sv$arg_type
        mode = sv$arg_mode

        if(type == "#") {
            return("")
        }
        if(type == "$") {
            if(mode == "l") {
                if(isTRUE(sv$negatable)) {
                    return(paste0(", --no-", gsub("_", "-", .self$arg_name[1])))
                }
                return("")
            }
            return(paste0(" ", mode_label(mode)))
        }
        if(type == "@") {
            return(paste0(" [", mode_label(mode), ", ...]"))
        }
        if(type == "%") {
            return(paste0(" {name=", mode_label(mode), ", ...}"))
        }
        ""
    }
)


SingleArgument$methods(
    default_as_string = function() {
        if(!.self$has_default) return(NULL)
        sv = .self$spec_value
        mode = sv$arg_mode
        type = sv$arg_type

        if(type == "$" && mode == "l") {
            if(isTRUE(sv$negatable)) {
                return(if(isTRUE(.self$default)) "on" else "off")
            }
            return(NULL)
        }
        if(is.null(.self$default)) return("NULL")
        if(type == "$") return(as.character(.self$default))
        if(type == "@" || type == "#") return(paste(.self$default, collapse = ", "))
        if(type == "%") return(paste(names(.self$default), .self$default, sep = "=", collapse = ", "))
        NULL
    }
)


# one-column help block for a single argument
SingleArgument$methods(
    help_line = function(prefix = "  ", width = argitect.options("help_width"), which = c("opt_line", "desc_line")) {
        msg = ""

        if("opt_line" %in% which) {
            if(.self$spec_value$arg_type == "#") {
                opt_line = paste0(prefix, "<", .self$arg_name[1], ">")
                rng = .self$spec_value$arg_range
                if(is.numeric(rng) && !(rng[1] == 1 && rng[2] == 1)) {
                    opt_line = paste0(opt_line, " [", mode_label(.self$spec_value$arg_mode), ", ...]")
                } else {
                    opt_line = paste0(opt_line, " ", mode_label(.self$spec_value$arg_mode))
                }
            } else {
                opt_line = paste0(prefix, .self$opt_name_line())
                opt_line = paste0(opt_line, .self$type_suffix())
            }
            msg = paste0(msg, opt_line, "\n")
        }

        if("desc_line" %in% which) {
            desc_line = format_text(.self$desc, prefix = prefix, width = width)
            msg = paste0(msg, desc_line, "\n")
            default_str = .self$default_as_string()
            if(!is.null(default_str)) {
                msg = paste0(msg, prefix, "  [default: ", default_str, "]\n")
            }

            if(.self$spec_value$arg_type == "%") {
                if(length(.self$spec_value$arg_sub)) {
                    msg = paste0(msg, .self$sub_opt_lines(prefix))
                } else if(length(.self$sub_opt)) {
                    msg = paste0(msg, .self$sub_opt_desc_lines(prefix))
                }
            }
        }

        msg
    }
)


# sub-argument documentation for a hash (from the spec itself)
SingleArgument$methods(
    sub_opt_lines = function(prefix = "  ") {
        msg = paste0("\n", prefix, "  Sub named options:\n")
        for(sub in .self$spec_value$arg_sub) {
            nm = sub$arg_name[1]
            m = if(is.na(sub$arg_mode)) .self$spec_value$arg_mode else sub$arg_mode
            type_str = if(sub$arg_type == "@") paste0(" [", mode_label(m), ", ...]") else paste0(" ", mode_label(m))
            if(sub$arg_type == "#") type_str = paste0(" <positional ", mode_label(m), ">")
            str1 = paste0(prefix, "  ", nm, type_str)
            d = if(!is.null(.self$sub_opt) && nm %in% names(.self$sub_opt)) .self$sub_opt[[nm]] else ""
            msg = paste0(msg, str1, "\n")
            if(nzchar(d)) {
                msg = paste0(msg, format_text(d, prefix = paste0(prefix, "    ")), "\n")
            }
        }
        msg
    }
)


# sub-argument descriptions supplied via "name$sub" specs when no sub-args declared
SingleArgument$methods(
    sub_opt_desc_lines = function(prefix = "  ") {
        msg = paste0("\n", prefix, "  Sub named options:\n")
        for(nm in names(.self$sub_opt)) {
            str1 = paste0(prefix, "  ", nm, "=", mode_label(.self$spec_value$arg_mode))
            msg = paste0(msg, str1, "\n")
            msg = paste0(msg, format_text(.self$sub_opt[[nm]], prefix = paste0(prefix, "    ")), "\n")
        }
        msg
    }
)


# two-column help line: returns opt_line only or the full aligned block
SingleArgument$methods(
    help_line_two_columns = function(prefix = "  ", only_opt = FALSE, opt_width = NULL,
        width = max(argitect.options("help_width"), opt_width + 60)) {

        if(.self$spec_value$arg_type == "#") {
            opt_line = paste0(prefix, "<", .self$arg_name[1], ">")
        } else {
            opt_line = paste0(prefix, .self$opt_name_line())
        }
        opt_line = paste0(opt_line, .self$type_suffix())

        if(.self$spec_value$arg_type != "#") {
            if(.self$spec_value$arg_type == "$" && .self$spec_value$arg_mode == "l") {
                # logical: no type line
            } else if(.self$spec_value$arg_type == "$") {
                opt_line = c(opt_line, paste0(prefix, "  [type: ", mode_abbr(.self$spec_value$arg_mode), "]"))
            } else if(.self$spec_value$arg_type == "@") {
                opt_line = c(opt_line, paste0(prefix, "  [type: [", mode_abbr(.self$spec_value$arg_mode), ", ...]]"))
            } else if(.self$spec_value$arg_type == "%") {
                opt_line = c(opt_line, paste0(prefix, "  [type: {name=", mode_abbr(.self$spec_value$arg_mode), ", ...}]"))
            }
        }

        if(only_opt) return(opt_line)

        prefix2 = strrep(" ", opt_width + 2)
        desc_line = format_text(.self$desc, prefix = prefix2, width = width)

        default_str = .self$default_as_string()
        if(!is.null(default_str)) {
            desc_line = paste0(desc_line, prefix2, "\n", prefix2, "  [default: ", default_str, "]")
        }

        if(.self$spec_value$arg_type == "%" && length(.self$spec_value$arg_sub)) {
            sub_opt_line = paste0(prefix2, "\n", prefix2, "  Sub named options:\n")
            for(sub in .self$spec_value$arg_sub) {
                nm = sub$arg_name[1]
                m = if(is.na(sub$arg_mode)) .self$spec_value$arg_mode else sub$arg_mode
                str1 = paste0(prefix2, "  ", nm, "=", mode_abbr(m))
                d = if(!is.null(.self$sub_opt) && nm %in% names(.self$sub_opt)) .self$sub_opt[[nm]] else ""
                str2 = format_text(d, prefix = paste0(prefix2, "    "))
                sub_opt_line = paste0(sub_opt_line, str1, "\n", str2, "\n")
            }
            desc_line = paste0(desc_line, prefix2, "\n", sub_opt_line, "\n")
        }

        desc_line = strsplit(desc_line, "\n")[[1]]

        n1 = length(opt_line)
        n2 = length(desc_line)
        msg = character(max(n1, n2))
        for(i in seq_along(msg)) {
            if(i <= n1 && i <= n2) {
                msg[i] = desc_line[i]
                substr(msg[i], 1, nchar(opt_line[i])) = opt_line[i]
            } else if(i > n1) {
                msg[i] = desc_line[i]
            } else {
                msg[i] = opt_line[i]
            }
        }
        paste(msg, collapse = "\n")
    }
)


# ============================================================
# Full help message
# ============================================================

print_help_msg = function(arguments, file = stdout(), script_name = NULL, head = NULL, foot = NULL,
    template = NULL, template_control = NULL, style = c("one-column", "two-column"),
    opt_group = NULL, opt_group_desc = NULL) {

    style = match.arg(style)

    if(is.null(template)) {

        if(is.null(script_name)) {
            script_name = get_scriptname()
            if(is.null(script_name)) script_name = "foo.R" else script_name = basename(script_name)
        }

        if(!is.null(head)) {
            cat(format_text(head, prefix = NULL), file = file)
            cat("\n\n", file = file)
        }

        # usage line
        positional = arguments[vapply(arguments, function(a) identical(a$spec_value$arg_type, "#"), logical(1))]
        usage_args = ""
        if(length(positional)) {
            usage_args = paste0(" ", paste(vapply(positional, function(a) {
                rng = a$spec_value$arg_range
                if(is.numeric(rng) && rng[1] == 1 && rng[2] == 1) {
                    paste0("<", a$arg_name[1], ">")
                } else {
                    paste0("<", a$arg_name[1], "> ...")
                }
            }, character(1)), collapse = " "))
        }

        prefix = argitect.options("__prefix__")
        if(is.null(prefix)) {
            cat(paste0("Usage: Rscript ", script_name, " [options]", usage_args, "\n"), file = file)
        } else {
            cat(paste0("Usage: ", prefix, " [options]", usage_args, "\n"), file = file)
        }
        cat("\n", file = file)

        for(ig in seq_along(opt_group)) {
            ind = opt_group[[ig]]
            if(nzchar(trimws(opt_group_desc[ig]))) {
                cat(format_text(opt_group_desc[ig], prefix = NULL), file = file)
                cat("\n", file = file)
            }

            if(style == "one-column") {
                for(i in ind) {
                    if(i <= length(arguments)) {
                        cat(arguments[[i]]$help_line(), "\n", file = file)
                    }
                }
            } else {
                opt_line = NULL
                for(i in ind) {
                    if(i <= length(arguments)) {
                        opt_line = c(opt_line, arguments[[i]]$help_line_two_columns(only_opt = TRUE))
                    }
                }
                opt_width = max(nchar(opt_line))
                for(i in ind) {
                    if(i <= length(arguments)) {
                        cat(arguments[[i]]$help_line_two_columns(opt_width = opt_width), "\n", file = file)
                    }
                }
                cat("\n", file = file)
            }
        }

        if(!is.null(foot)) {
            cat(format_text(foot, prefix = NULL), file = file)
            cat("\n", file = file)
        }

    } else {
        # template-based help
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

        for(i in seq_along(lines)) {
            lt = find_code(argitect.options("template_tag"), lines[i])
            if(length(lt$template) == 0) {
                cat(lines[i], "\n", file = file)
                next
            }

            for(j in seq_along(lt$template)) {
                for(k in seq_along(arguments)) {
                    if(arguments[[k]]$spec == lt$code[j]) {

                        data_type = TRUE
                        if(!is.null(template_control$data_type)) {
                            if(length(template_control$data_type) == 1 && is.null(names(template_control$data_type))) {
                                data_type = template_control$data_type
                            } else {
                                data_type = template_control$data_type[arguments[[k]]$arg_name[1]]
                                if(is.na(data_type)) data_type = TRUE
                            }
                        }

                        opt_str = gsub("\n$", "", arguments[[k]]$help_line(prefix = "", which = "opt_line"))
                        if(!data_type) {
                            opt_str = paste0(arguments[[k]]$opt_name_line())
                        }

                        opt_width = nchar(opt_str)
                        if(!is.null(template_control$opt_width)) {
                            if(length(template_control$opt_width) == 1 && is.null(names(template_control$opt_width))) {
                                opt_width = template_control$opt_width
                            } else {
                                w = template_control$opt_width[arguments[[k]]$arg_name[1]]
                                if(!is.na(w)) opt_width = w
                            }
                        }

                        prefix = strrep(" ", opt_width + 6)
                        desc = gsub(lt$template[j], "", lines[[i]], fixed = TRUE)
                        desc = gsub("^\\s+", "", desc)

                        opt_str2 = paste0("  ", opt_str, strrep(" ", max(0, opt_width - nchar(opt_str))))
                        cat(opt_str2, "    ", file = file, sep = "")

                        desc = strwrap(desc, width = max(10, 0.9 * argitect.options("help_width") - opt_width - 6))
                        if(length(desc) > 1) {
                            desc[-1] = paste0(prefix, desc[-1])
                        }
                        cat(paste(desc, collapse = "\n"), file = file)
                        cat("\n", file = file)
                    }
                }
            }
        }
    }
}
