
# ============================================================
# Help messages
# ============================================================

# human-readable name of a mode
mode_label = function(mode) {
    c("i" = "integer", "f" = "numeric", "s" = "character",
      "l" = "logical")[[mode]]
}

mode_abbr = function(mode) {
    c("i" = "int", "f" = "num", "s" = "chr", "l" = "logical")[[mode]]
}


# one sub-argument entry for the two-column help style: "name=int description..."
# with continuation lines aligned to the start of the description
format_sub_opt_entry = function(label, desc, prefix, width) {
    label_w = nchar(label)
    head = paste0(prefix, "  ", label)
    if(!nzchar(desc)) return(head)
    desc = render_desc(desc)
    indent = strrep(" ", label_w + 1)
    avail = max(10, 0.9 * (width - nchar(prefix) - 2 - label_w - 1))
    w = strwrap(desc, width = avail)
    first = paste0(head, " ", w[1])
    if(length(w) > 1) {
        paste(c(first, paste0(prefix, "  ", indent, w[-1])), collapse = "\n")
    } else {
        first
    }
}


# expand an argument into the option string shown for a template placeholder
template_opt_str = function(a, template_control = NULL) {
    name = a$arg_name[1]
    data_type = TRUE
    if(!is.null(template_control$data_type)) {
        if(length(template_control$data_type) == 1 && is.null(names(template_control$data_type))) {
            data_type = template_control$data_type
        } else {
            data_type = template_control$data_type[name]
            if(is.na(data_type)) data_type = TRUE
        }
    }
    if(isTRUE(data_type)) {
        gsub("\n$", "", a$help_line(prefix = "", which = "opt_line"))
    } else {
        a$opt_name_line()
    }
}


# a compact "number of values" description used in type suffixes:
# "..." (unbounded), "3" (exactly), "2-3" (range), "3+" (at least)
count_spec = function(rng) {
    if(!is.numeric(rng) || length(rng) != 2) return("...")
    lo = rng[1]
    hi = rng[2]
    if(lo == 1 && hi == MAX_NV) return("...")
    if(lo == hi) return(as.character(lo))
    if(hi == MAX_NV) return(paste0(lo, "+"))
    paste0(lo, "-", hi)
}


# compact type of a hash sub-argument, e.g. "integer", "[integer, ...]"
sub_type_str = function(sub, parent_mode, mode_fun = mode_label) {
    m = if(is.na(sub$arg_mode)) parent_mode else sub$arg_mode
    if(sub$arg_type == "@") {
        return(paste0("[", mode_fun(m), ", ", count_spec(sub$arg_range), "]"))
    }
    if(sub$arg_type == "#") {
        return(paste0("<positional ", mode_fun(m), ">"))
    }
    mode_fun(m)
}


# render a fixed-key set "{a=integer, b=integer}" on one line or, when it is too
# wide, on several aligned lines (one key per line, aligned after the "{");
# the trailing "..." (open set) is treated as one more entry and may get its
# own line as well
render_fixed_suffix = function(parts, open, base = "", width = Inf) {
    if(isTRUE(open)) parts = c(parts, "...")
    single = paste0(" {", paste(parts, collapse = ", "), "}")
    if(length(parts) <= 1 || nchar(single) <= width) return(single)
    n = length(parts)
    indent = strrep(" ", nchar(base) + 2)
    lines = character(n)
    lines[1] = paste0(" {", parts[1], ",")
    if(n > 1) {
        for(i in 2:n) {
            lines[i] = paste0(indent, parts[i], if(i < n) "," else "}")
        }
    }
    paste(lines, collapse = "\n")
}

# compute the (possibly overridden) option width of a template entry
template_opt_width = function(a, opt_str, template_control = NULL) {
    w = nchar(opt_str)
    if(!is.null(template_control$opt_width)) {
        if(length(template_control$opt_width) == 1 && is.null(names(template_control$opt_width))) {
            w = as.integer(template_control$opt_width)
        } else {
            ww = template_control$opt_width[a$arg_name[1]]
            if(!is.na(ww)) w = as.integer(ww)
        }
    }
    w
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
    type_suffix = function(base = "", width = Inf) {
        sv = .self$spec_value
        type = sv$arg_type
        mode = sv$arg_mode

        if(type == "#") {
            rng = sv$arg_range
            if(is.numeric(rng) && (rng[1] == 1 && rng[2] == 1)) {
                return(paste0(" ", mode_label(mode)))
            }
            return(paste0(" [", mode_label(mode), ", ", count_spec(rng), "]"))
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
        if(type == "@" && is.character(sv$arg_range)) {
            keys = sv$arg_range
            parts = vapply(keys, function(k) paste0(k, "=", mode_label(mode)), character(1))
            return(render_fixed_suffix(parts, isTRUE(attr(keys, "open")), base, width))
        }
        if(type == "@") {
            return(paste0(" [", mode_label(mode), ", ", count_spec(sv$arg_range), "]"))
        }
        if(type == "%") {
            if(length(sv$arg_sub)) {
                parts = vapply(sv$arg_sub, function(sub) {
                    paste0(sub$arg_name[1], "=", sub_type_str(sub, mode))
                }, character(1))
                return(render_fixed_suffix(parts, isTRUE(attr(sv$arg_range, "open")), base, width))
            }
            return(render_fixed_suffix(paste0("name=", mode_label(mode)), TRUE, base, width))
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
            } else {
                opt_line = paste0(prefix, .self$opt_name_line())
            }
            opt_line = paste0(opt_line, .self$type_suffix(base = opt_line, width = 0.6 * width))
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
            type_str = if(sub$arg_type == "@") paste0(" [", mode_label(m), ", ", count_spec(sub$arg_range), "]") else paste0(" ", mode_label(m))
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
        opt_line = paste0(opt_line, .self$type_suffix(base = opt_line, width = 0))

        if(only_opt) return(opt_line)

        prefix2 = strrep(" ", opt_width + 2)
        desc_line = format_text(.self$desc, prefix = prefix2, width = width)

        default_str = .self$default_as_string()
        if(!is.null(default_str)) {
            desc_line = paste0(desc_line, prefix2, "\n", prefix2, "  [default: ", default_str, "]")
        }

        # collect sub-argument entries (declared sub-args, or free-form "name$sub" descriptions)
        sub_entries = list()
        if(.self$spec_value$arg_type == "%") {
            if(length(.self$spec_value$arg_sub)) {
                for(sub in .self$spec_value$arg_sub) {
                    nm = sub$arg_name[1]
                    d = if(!is.null(.self$sub_opt) && nm %in% names(.self$sub_opt)) .self$sub_opt[[nm]] else ""
                    sub_entries[[length(sub_entries) + 1]] = list(
                        nm = nm, type = sub_type_str(sub, .self$spec_value$arg_mode, mode_abbr), d = d)
                }
            } else if(length(.self$sub_opt)) {
                for(nm in names(.self$sub_opt)) {
                    sub_entries[[length(sub_entries) + 1]] = list(
                        nm = nm, type = mode_abbr(.self$spec_value$arg_mode), d = .self$sub_opt[[nm]])
                }
            }
        }
        if(length(sub_entries)) {
            sub_opt_line = paste0(prefix2, "  Sub named options:")
            for(e in sub_entries) {
                label = paste0(e$nm, "=", e$type)
                sub_opt_line = paste0(sub_opt_line, "\n",
                    format_sub_opt_entry(label, e$d, prefix2, width))
            }
            desc_line = paste0(desc_line, prefix2, "\n", sub_opt_line)
        }

        desc_line = strsplit(desc_line, "\n")[[1]]
        opt_line = strsplit(opt_line, "\n")[[1]]

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

        # usage line: positional arguments go before/after "[options]" according
        # to their declaration order; "[foo]" for a single one, "[foo...]" for many
        is_pos = vapply(arguments, function(a) identical(a$spec_value$arg_type, "#"), logical(1))
        pos_str = function(a) {
            rng = a$spec_value$arg_range
            if(is.numeric(rng) && rng[1] == 1 && rng[2] == 1) {
                paste0("[", a$arg_name[1], "]")
            } else {
                paste0("[", a$arg_name[1], "...]")
            }
        }

        pos_idx = which(is_pos)
        first_non_pos = which(!is_pos)[1]
        if(length(pos_idx) && !is.na(first_non_pos)) {
            pre_pos = pos_idx[pos_idx < first_non_pos]
            post_pos = pos_idx[pos_idx > first_non_pos]
        } else {
            pre_pos = pos_idx
            post_pos = integer(0)
        }

        usage_pre = if(length(pre_pos)) {
            paste0(" ", paste(vapply(arguments[pre_pos], pos_str, character(1)), collapse = " "))
        } else ""
        usage_post = if(length(post_pos)) {
            paste0(" ", paste(vapply(arguments[post_pos], pos_str, character(1)), collapse = " "))
        } else ""

        prefix = argitect.options("__prefix__")
        if(is.null(prefix)) {
            cat(bold(paste0("Usage: Rscript ", script_name, usage_pre, " [options]", usage_post, "\n")), file = file)
        } else {
            cat(bold(paste0("Usage: ", prefix, usage_pre, " [options]", usage_post, "\n")), file = file)
        }
        cat("\n", file = file)

        for(ig in seq_along(opt_group)) {
            ind = opt_group[[ig]]
            # one-column mode: separate a new section from the previous one
            if(style == "one-column" && ig > 1) {
                cat("\n", file = file)
            }
            if(nzchar(trimws(opt_group_desc[ig]))) {
                cat(bold(format_text(opt_group_desc[ig], prefix = NULL)), file = file)
                cat("\n", file = file)
            }

            if(style == "one-column") {
                for(i in ind) {
                    if(i <= length(arguments)) {
                        hl = arguments[[i]]$help_line()
                        hl_lines = strsplit(hl, "\n")[[1]]
                        hl_lines[1] = cyan(hl_lines[1])
                        cat(paste(hl_lines, collapse = "\n"), "\n", file = file)
                    }
                }
            } else {
                opt_line = NULL
                for(i in ind) {
                    if(i <= length(arguments)) {
                        ol = arguments[[i]]$help_line_two_columns(only_opt = TRUE)
                        opt_line = c(opt_line, strsplit(ol, "\n")[[1]])
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

        # evaluate "{{ ... }}" chunks (e.g. {{ cli_h2("Options") }}) in the template
        lines = render_desc(lines, .envir = glue_env())

        tag = argitect.options("template_tag")
        n_line = length(lines)

        # first tag on each line, and the matching argument index
        line_arg = rep(NA_integer_, n_line)
        line_opt = rep(NA_character_, n_line)
        line_width = rep(NA_integer_, n_line)
        line_blank = rep(NA_character_, n_line)   # "<#name>" continuation line
        for(i in seq_len(n_line)) {
            lt = find_code(tag, lines[i])
            if(length(lt$template) == 0) next
            code = lt$code[1]
            if(grepl("^#", code)) {
                line_blank[i] = sub("^#", "", code)
                next
            }
            k = which(vapply(arguments, function(a) a$spec == code, logical(1)))
            if(length(k) == 0) next
            line_arg[i] = k[1]
            line_opt[i] = template_opt_str(arguments[[k[1]]], template_control)
            line_width[i] = template_opt_width(arguments[[k[1]]], line_opt[i], template_control)
        }

        # group lines into sections separated by blank lines
        section = integer(n_line)
        sec = 0L
        for(i in seq_len(n_line)) {
            if(i > 1 && grepl("^\\s*$", lines[i - 1])) sec = sec + 1L
            section[i] = sec
        }

        # a section shares one width: the widest expanded option in it
        sec_width = tapply(line_width[!is.na(line_width)],
                           section[!is.na(line_width)], max)

        for(i in seq_len(n_line)) {
            s = as.character(section[i])
            section_w = if(!is.na(sec_width[s])) sec_width[s] else 0L

            # "<#name>" reserves the same option column as the option "name"
            if(!is.na(line_blank[i])) {
                k = which(vapply(arguments, function(a) a$arg_name[1] == line_blank[i], logical(1)))
                own = if(length(k)) {
                    template_opt_width(arguments[[k[1]]],
                        template_opt_str(arguments[[k[1]]], template_control), template_control)
                } else 0L
                opt_width = max(section_w, own)

                desc = gsub(find_code(tag, lines[[i]])$template[1], "", lines[[i]], fixed = TRUE)
                desc = gsub("^\\s+", "", desc)

                cat("  ", strrep(" ", opt_width), "    ", file = file, sep = "")
                if(nzchar(desc)) {
                    prefix = strrep(" ", opt_width + 6)
                    desc = render_desc(desc)
                    desc = strwrap(desc, width = max(10, 0.9 * argitect.options("help_width") - opt_width - 6))
                    if(length(desc) > 1) desc[-1] = paste0(prefix, desc[-1])
                    cat(paste(desc, collapse = "\n"), file = file)
                }
                cat("\n", file = file)
                next
            }

            if(is.na(line_arg[i])) {
                cat(lines[i], "\n", file = file)
                next
            }

            section_w = if(!is.na(sec_width[s])) sec_width[s] else line_width[i]

            lt = find_code(tag, lines[i])
            for(j in seq_along(lt$template)) {
                k = which(vapply(arguments, function(a) a$spec == lt$code[j], logical(1)))
                if(length(k) == 0) next
                a = arguments[[k[1]]]

                opt_str = template_opt_str(a, template_control)
                opt_width = max(section_w,
                    template_opt_width(a, opt_str, template_control))
                prefix = strrep(" ", opt_width + 6)

                desc = gsub(lt$template[j], "", lines[[i]], fixed = TRUE)
                desc = gsub("^\\s+", "", desc)
                desc = render_desc(desc)

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


# ============================================================
# Specification explanation (argitect(explain = TRUE))
# ============================================================

# sample R literal for a single hash sub-argument
sub_r_storage = function(sub, parent_mode) {
    m = if(is.na(sub$arg_mode)) parent_mode else sub$arg_mode
    type = sub$arg_type
    r_val = c("i" = "1L", "f" = "1.5", "s" = '"a"', "l" = "TRUE")[[m]]

    if(type == "$") return(r_val)
    if(type == "#") {
        single = is.numeric(sub$arg_range) && sub$arg_range[1] == 1 && sub$arg_range[2] == 1
        if(single) return(r_val)
        return(c("i" = "1:3", "f" = "c(1.5, 2.5, 3.5)",
                 "s" = 'c("a", "b", "c")',
                 "l" = "c(TRUE, FALSE)")[[m]])
    }
    if(type == "@") {
        if(is.character(sub$arg_range)) {
            keys = sub$arg_range
            return(paste0("c(", paste(paste0(keys, "=", r_val), collapse = ", "), ")"))
        }
        return(c("i" = "c(1L, 2L, 3L)", "f" = "c(1.5, 2.5, 3.5)",
                 "s" = 'c("a", "b", "c")',
                 "l" = "c(TRUE, FALSE)")[[m]])
    }
    r_val
}


# sample command-line token(s) for a single hash sub-argument
sub_cli_sample = function(sub, parent_mode) {
    m = if(is.na(sub$arg_mode)) parent_mode else sub$arg_mode
    type = sub$arg_type
    cli_vec = function(n = 3) {
        if(m == "i") paste(seq_len(n), collapse = " ")
        else if(m == "f") paste(seq_len(n) + 0.5, collapse = " ")
        else paste(letters[seq_len(n)], collapse = " ")
    }

    if(type == "$" && m == "l") return(sub$arg_name[1])
    if(type == "$") return(paste0(sub$arg_name[1], "=", c("i"="1","f"="1.5","s"="a")[[m]]))
    if(type == "#") {
        single = is.numeric(sub$arg_range) && sub$arg_range[1] == 1 && sub$arg_range[2] == 1
        if(single) return(c("i"="1","f"="1.5","s"="a")[[m]])
        return(cli_vec())
    }
    if(type == "@") {
        if(is.character(sub$arg_range)) {
            keys = sub$arg_range
            return(paste(paste0(keys, "=", c("i"="1","f"="1.5","s"="a")[[m]]), collapse = " "))
        }
        # vector sub-argument: values separated by value_sep inside name=value
        vec = if(m == "i") "1,2,3" else if(m == "f") "1.5,2.5,3.5" else "a,b,c"
        return(paste0(sub$arg_name[1], "=", vec))
    }
    paste0(sub$arg_name[1], "=", c("i"="1","f"="1.5","s"="a")[[m]])
}


# describe how an argument is stored in the R variable (for script developers)
r_storage = function(a) {
    sv = a$spec_value
    mode = sv$arg_mode
    type = sv$arg_type
    nm = a$arg_name[1]
    r_type = c("i" = "integer", "f" = "numeric", "s" = "character",
               "l" = "logical")[[mode]]

    if(type == "$") {
        return(paste0(r_type, " scalar in variable `", nm, "`"))
    }
    if(type == "#") {
        single = is.numeric(sv$arg_range) && sv$arg_range[1] == 1 && sv$arg_range[2] == 1
        shape = if(single) "scalar" else "vector"
        return(paste0(r_type, " ", shape, " in variable `", nm, "`"))
    }
    if(type == "@") {
        if(is.character(sv$arg_range)) {
            keys = sv$arg_range
            val = c("i" = "1L", "f" = "1.5", "s" = '"a"', "l" = "TRUE")[[mode]]
            ex = paste(paste0(keys, "=", val), collapse = ", ")
            return(paste0("named ", r_type, " vector in variable `", nm, "` (e.g. c(", ex, "))"))
        }
        return(paste0(r_type, " vector in variable `", nm, "`"))
    }
    if(type == "%") {
        if(length(sv$arg_sub)) {
            parts = vapply(sv$arg_sub, function(sub) {
                paste0(sub$arg_name[1], " = ", sub_r_storage(sub, mode))
            }, character(1))
            return(paste0("named list in variable `", nm, "` (e.g. list(", paste(parts, collapse = ", "), "))"))
        }
        val = c("i" = "1L", "f" = "1.5", "s" = '"a"', "l" = "TRUE")[[mode]]
        return(paste0("named list in variable `", nm, "` (e.g. list(foo=", val, ", bar=", val, "))"))
    }
    "unknown"
}


# explain the format of a single argument specification with a simple example
explain_argument = function(a) {
    sv = a$spec_value
    mode = sv$arg_mode
    type = sv$arg_type
    nm = a$arg_name[1]
    mode_word = c("i" = "integer", "f" = "numeric", "s" = "character",
                  "l" = "logical")[[mode]]
    sample = c("i" = "1", "f" = "0.5", "s" = "value", "l" = "")[[mode]]

    if(type == "$" && mode == "l") {
        fmt = "logical flag (takes no value)"
        ex = paste0("--", nm, "  (TRUE);  --no-", nm, "  (FALSE)")
    } else if(type == "$") {
        fmt = paste0("single ", mode_word, " value")
        ex = paste0("--", nm, " ", sample)
    } else if(type == "#") {
        fmt = paste0("positional ", mode_word, " (no option name)")
        ex = paste0(sample, "  (the value itself)")
    } else if(type == "@") {
        if(is.character(sv$arg_range)) {
            keys = sv$arg_range
            fmt = paste0("named vector with fixed keys: ", paste(keys, collapse = ", "))
            ex = paste0("--", nm, " ", paste(paste0(keys, "=", sample), collapse = " "))
        } else {
            fmt = paste0("vector of ", mode_word, " values")
            ex = paste0("--", nm, " ", paste(rep(sample, 3), collapse = " "))
        }
    } else if(type == "%") {
        if(length(sv$arg_sub)) {
            parts = vapply(sv$arg_sub, function(sub) sub_cli_sample(sub, mode), character(1))
            ex = paste0("--", nm, " ", paste(parts, collapse = " "))
            has_pos = any(vapply(sv$arg_sub, function(s) identical(s$arg_type, "#"), logical(1)))
            has_named = any(!vapply(sv$arg_sub, function(s) identical(s$arg_type, "#"), logical(1)))
            if(has_pos && !has_named) {
                fmt = "positional sub-arguments (no names)"
            } else if(has_named && !has_pos) {
                fmt = "name=value sub-arguments"
            } else {
                fmt = "name=value and positional sub-arguments"
            }
        } else {
            fmt = paste0("list of name=value pairs (", mode_word, " values)")
            ex = paste0("--", nm, " foo=", sample, " bar=", sample)
        }
    } else {
        fmt = "unknown"; ex = ""
    }

    c(
        paste0("  ", a$spec),
        paste0("      ", fmt),
        paste0("      e.g.  ", ex),
        paste0("      R:    ", r_storage(a))
    )
}


# print an explanation of every argument specification
print_explain_msg = function(arguments, file = stdout()) {
    cat("Argument specification explanation:\n\n", file = file)
    for(a in arguments) {
        if(a$arg_name[1] %in% DEFAULT_ARGUMENTS) next
        cat(paste(explain_argument(a), collapse = "\n"), "\n\n", file = file)
    }
    invisible(NULL)
}
