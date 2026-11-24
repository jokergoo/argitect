
# ============================================================
# argitect(): main entry point
# ============================================================

#' Parse command-line arguments
#'
#' A pure-R command-line argument parser that reimplements the functionality
#' of the Perl module `Getopt::Long` without any external dependency, and
#' additionally supports positional arguments.
#'
#' @param ... Argument specifications. They can be given as
#'   `(specification, description)` pairs, a two-column matrix, or a single
#'   text template. See Details.
#' @param help_head Head of the help message shown by `--help`.
#' @param help_foot Foot of the help message shown by `--help`.
#' @param envir Environment where default values are looked up and parsed
#'   values are exported. A list is also accepted and coerced to an
#'   environment.
#' @param argv_str A string containing command-line arguments. Only for
#'   testing purposes.
#' @param template_control A list controlling template help rendering, e.g.
#'   `list(opt_width = 21)`.
#' @param help_style The style of the help message: `"one-column"` or
#'   `"two-column"`.
#' @param help_width The width of the help message. A vector of one or two
#'   numbers; the second number is only used by the two-column style. Defaults
#'   to `argitect_options("help_width")`.
#' @param explain If `TRUE`, print an explanation of each argument
#'   specification (its format and simple examples) and return without
#'   parsing any command-line arguments.
#' @param value_sep The separator used to split multiple values inside a
#'   single `name=value` pair for a vector sub-argument. The default `"auto"`
#'   detects the separator among `","`, `";"`, `":"`, `"|"` and `"/"` by
#'   picking the one that satisfies the type and count constraints.
#'
#' @details
#' Each argument specification is `name[|alias...]=spec`. The value part
#' `spec` consists of an optional *mode* (`i` integer, `f` numeric, `s`
#' character, `l` logical) and an optional *type* (`$`
#' scalar, `#` positional, `@` vector, `%` hash), optionally followed by a
#' bracketed range or sub-argument list. The `i` mode also accepts extended
#' integer notation: `0x`/`0X` hexadecimal, `0b`/`0B` binary, `0`/`0o`/`0O`
#' octal (e.g. `0123`), and decimal.
#'
#' - `"number=i"`: a single integer scalar.
#' - `"name=s"`: a single character scalar.
#' - `"length|size=i"`: an integer scalar with two names.
#' - `"tag=i@"`: an integer vector.
#' - `"tag=i@{2,}"`: an integer vector with at least two elements.
#' - `"param=s%"`: name=value pairs with character values.
#' - `"input=s#"`: one positional argument.
#' - `"files=s#{}"`: one or more positional arguments.
#' - `"verbose!"`: a logical option, negatable with `--no-verbose`.
#'
#' If a variable with the first name already exists in `envir` (and is not a
#' function), it is used as the default value and the option becomes optional.
#' Logical options always default to `FALSE`.
#'
#' @return Invisibly, a list of the parsed argument objects. Parsed values are
#'   also exported to `envir` as variables named after the first specification
#'   name.
#'
#' @examples
#' e = new.env()
#' argitect("count=i", "Number of items.", "verbose!", "Print messages.",
#'     envir = e, argv_str = "--count 3 --verbose")
#' e$count
#' e$verbose
#'
#' @export
argitect = function(..., help_head = NULL, help_foot = NULL, envir = parent.frame(),
    argv_str = NULL, explain = FALSE, template_control = list(), 
    help_style = argitect_options("help_style"),
    help_width = argitect_options("help_width"), 
    value_sep = argitect_options("value_sep")) {

    if(is.list(envir)) envir = as.environment(envir)

    # make local variables of the calling frame visible to "{{ ... }}"
    old_glue_envir = .argitect_ctx$glue_envir
    .argitect_ctx$glue_envir = parent.frame()
    on.exit({ .argitect_ctx$glue_envir = old_glue_envir }, add = TRUE)

    # per-call help width; the global default comes from argitect_options
    old_help_width = .argitect_ctx$help_width
    .argitect_ctx$help_width = help_width
    on.exit({ .argitect_ctx$help_width = old_help_width }, add = TRUE)

    if(is.null(get_scriptname())) {
        IS_UNDER_COMMAND_LINE = FALSE
    } else {
        IS_UNDER_COMMAND_LINE = TRUE
    }

    if(is.null(argv_str)) {
        argv_str = argitect_options("__argv_str__")
    }

    spec_info = parse_specification(list(...))

    # reserved options are rejected before building (and before any warning)
    reserved_hit = any(vapply(spec_info$spec[, 1], function(s) {
        any(spec_names(s) %in% DEFAULT_ARGUMENTS)
    }, logical(1)))
    if(reserved_hit) {
        stop_wrap("`help` and `version` are reserved options. Please do not use them.")
    }

    # build argument objects
    arguments = list()
    for(i in seq_len(nrow(spec_info$spec))) {
        a = SingleArgument(spec = spec_info$spec[i, 1], desc = spec_info$spec[i, 2], envir = envir)
        arguments[[length(arguments) + 1]] = a
    }
    names(arguments) = vapply(arguments, function(a) a$arg_name[1], character(1))

    # argument names and aliases must be unique
    all_names = unlist(lapply(arguments, function(a) a$arg_name), use.names = FALSE)
    dup = unique(all_names[duplicated(all_names)])
    if(length(dup)) {
        stop_wrap("Duplicated argument name or alias: ", paste(dup, collapse = ", "), ".")
    }

    # attach sub-option descriptions (spec of the form "name$sub")
    if(length(spec_info$sub_opt)) {
        for(parent in names(spec_info$sub_opt)) {
            if(!parent %in% names(arguments)) {
                stop_wrap("Sub-argument description(s) are given for '", parent,
                          "', which is not an argument.")
            }
            a = arguments[[parent]]
            doc_names = names(spec_info$sub_opt[[parent]])
            declared = vapply(a$spec_value$arg_sub, function(s) s$arg_name[1], character(1))

            # fixed sub-argument set: documentation must match the declared names
            if(length(declared)) {
                extra = setdiff(doc_names, declared)
                if(length(extra)) {
                    stop_wrap("Sub-argument", if(length(extra) > 1) "s" else "",
                              " ", paste0("'", extra, "'", collapse = ", "),
                              " documented for '", parent, "' ", if(length(extra) > 1) "are" else "is",
                              " not declared in its specification.")
                }
                missing = setdiff(declared, doc_names)
                if(length(missing)) {
                    warning_wrap("Sub-argument", if(length(missing) > 1) "s" else "",
                                 " ", paste0("'", missing, "'", collapse = ", "),
                                 " of '", parent, "' ",
                                 if(length(missing) > 1) "have" else "has",
                                 " no documented description.")
                }
            }

            a$sub_opt = c(a$sub_opt, spec_info$sub_opt[[parent]])
            arguments[[parent]] = a
        }
    }

    # add reserved help / version
    arguments$help = suppressWarnings(
        SingleArgument(spec = "help", desc = "Print help message and exit.", envir = envir))
    arguments$version = suppressWarnings(
        SingleArgument(spec = "version", desc = "Print version information and exit.", envir = envir))

    # single-letter flags
    arguments = add_short_flags(arguments)

    # append help / version to the last option group
    n_arg = length(arguments)
    spec_info$opt_group[[length(spec_info$opt_group)]] =
        c(spec_info$opt_group[[length(spec_info$opt_group)]], n_arg - 1, n_arg)

    # explain mode: only print the specification explanation, no parsing
    if(isTRUE(explain)) {
        print_explain_msg(arguments, file = stdout())
        return(invisible(arguments))
    }

    # parse argv
    if(is.null(argv_str)) {
        argv = commandArgs(TRUE)
    } else {
        argv = strsplit(argv_str, "\\s+")[[1]]
        if(length(argv) == 1 && argv[1] == "") argv = character(0)
    }

    config = argitect_options("config")

    err = NULL
    tryCatch({
        parse_argv(arguments, argv, config = config, value_sep = value_sep)
    }, error = function(e) {
        err <<- conditionMessage(e)
    })

    if(!is.null(err)) {
        if(IS_UNDER_COMMAND_LINE && is.null(argitect_options("__script_name__"))) {
            cat(bold(red(paste0("Error: ", err, "\n"))), file = stderr())
            print_help_msg(arguments, file = stderr(), head = help_head, foot = help_foot,
                template = spec_info$template, template_control = template_control,
                style = help_style, opt_group = spec_info$opt_group,
                opt_group_desc = spec_info$opt_group_desc)
            q(save = "no", status = 127)
        } else {
            stop_wrap(err)
        }
    }

    # --help
    if(isTRUE(arguments$help$value)) {
        print_help_msg(arguments, file = stdout(), head = help_head, foot = help_foot,
            template = spec_info$template, template_control = template_control,
            style = help_style, opt_group = spec_info$opt_group,
            opt_group_desc = spec_info$opt_group_desc)
        if(!is.null(argitect_options("__script_name__"))) {
            # inside source_script(): stop the sourced script quietly
            stop_script()
        } else if(IS_UNDER_COMMAND_LINE) {
            q(save = "no", status = 0)
        } else {
            return(invisible(NULL))
        }
    }

    # --version
    if(isTRUE(arguments$version$value)) {
        print_version_msg(envir, file = stdout())
        if(!is.null(argitect_options("__script_name__"))) {
            stop_script()
        } else if(IS_UNDER_COMMAND_LINE) {
            q(save = "no", status = 0)
        } else {
            return(invisible(NULL))
        }
    }

    # validate mandatory / ranges
    err = NULL
    tryCatch({
        validate_all(arguments)
    }, error = function(e) {
        err <<- conditionMessage(e)
    })

    if(!is.null(err)) {
        if(IS_UNDER_COMMAND_LINE) {
            cat(bold(red(paste0("Error: ", err, "\n"))), file = stderr())
            print_help_msg(arguments, file = stderr(), head = help_head, foot = help_foot,
                template = spec_info$template, template_control = template_control,
                style = help_style, opt_group = spec_info$opt_group,
                opt_group_desc = spec_info$opt_group_desc)
            q(save = "no", status = 127)
        } else {
            stop_wrap(err)
        }
    }

    export_to_env(arguments, envir = envir)

    invisible(arguments)
}



# ============================================================
# Specification parsing
# ============================================================

# extract all argument names (first name and aliases) from a raw spec string
spec_names = function(spec) {
    name_part = strsplit(spec, "=", fixed = TRUE)[[1]][1]
    name_part = sub("!$", "", name_part)
    strsplit(name_part, "|", fixed = TRUE)[[1]]
}

parse_specification = function(spec_list) {
    if(length(spec_list) == 1) {
        spec = spec_list[[1]]
    } else {
        spec = unlist(spec_list, use.names = FALSE)
    }

    template = NULL
    if(is.matrix(spec)) {
        if(ncol(spec) != 2) {
            stop_wrap("If the specification is a matrix, it should be a two-column matrix.")
        }
    } else if(is.vector(spec)) {
        if(length(spec) == 1) {
            template = spec
            template = sub("^\\n", "", template)
            # evaluate "{{ ... }}" chunks before the template is parsed
            template = render_desc(template, .envir = glue_env())
            spec = parse_spec_template(template)
            if(nrow(spec) == 0) stop_wrap("Cannot detect argument specification in the template.")
        } else if(length(spec) %% 2) {
            stop_wrap("Since the specification is a vector, it should have an even number of elements.")
        } else {
            spec = matrix(spec, ncol = 2, byrow = TRUE)
        }
    } else {
        stop_wrap("Wrong specification.")
    }

    # extract sub-option descriptions: "name$sub"
    l_sub = grepl("\\w+\\$\\S+$", spec[, 1])
    sub_opt = NULL
    if(sum(l_sub)) {
        sub_opt = lapply(split(seq_len(nrow(spec))[l_sub],
                gsub("\\$.*$", "", spec[l_sub, 1])), function(ind) {
            nm = gsub("^.*?\\$", "", spec[ind, 1])
            structure(spec[ind, 2], names = nm)
        })
    }
    spec = spec[!l_sub, , drop = FALSE]

    # detect option groups (separator lines)
    l_group = grepl("^[-=+#%]*$", spec[, 1])
    if(any(l_group)) {
        opt_group = list()
        opt_group_desc = NULL
        i_group = 0
        i_opt = 0
        if(!l_group[1]) {
            opt_group[[1]] = list()
            opt_group_desc = ""
            i_group = 1
        }
        for(i in seq_along(l_group)) {
            if(l_group[i]) {
                i_group = i_group + 1
                opt_group[[i_group]] = list()
                opt_group_desc = c(opt_group_desc, spec[i, 2])
            } else {
                i_opt = i_opt + 1
                opt_group[[i_group]] = c(opt_group[[i_group]], i_opt)
            }
        }
        spec = spec[!l_group, , drop = FALSE]
    } else {
        opt_group = list(seq_len(nrow(spec)))
        opt_group_desc = "Options:"
    }

    list(spec = spec, template = template, opt_group = opt_group,
         opt_group_desc = opt_group_desc, sub_opt = sub_opt)
}


parse_spec_template = function(template, tag = argitect_options("template_tag")) {
    lt = find_code(tag, template)
    code = lt$code
    spec = cbind(code, rep("", length(code)))
    spec = spec[!grepl("^#", spec[, 1]), , drop = FALSE]
    spec
}


# ============================================================
# Short-flag detection
# ============================================================

add_short_flags = function(arguments) {
    # explicit single-letter aliases are already taken
    taken = unique(unlist(lapply(arguments, function(a) {
        a$arg_name[nchar(a$arg_name) == 1]
    }), use.names = FALSE))

    # count the first letter of every multi-char name (first name and aliases)
    first_letters = unlist(lapply(arguments, function(a) {
        vapply(a$arg_name, function(nm) {
            if(nchar(nm) > 1) substr(nm, 1, 1) else NA_character_
        }, character(1))
    }), use.names = FALSE)
    first_letters = first_letters[!is.na(first_letters)]
    tb = table(first_letters)

    for(i in seq_along(arguments)) {
        a = arguments[[i]]
        # add the first letter of each multi-char name as a short flag if it is
        # unique and not already used as an explicit alias elsewhere
        for(nm in a$arg_name) {
            if(nchar(nm) == 1) next  # single-char alias is already an explicit flag
            le = substr(nm, 1, 1)
            if(tb[[le]] == 1 && !le %in% taken && !le %in% a$arg_name) {
                a$arg_name = c(a$arg_name, le)
                if(!nzchar(a$flag)) a$flag = le
            }
        }
        arguments[[i]] = a
    }
    arguments
}


# ============================================================
# Native command-line parser
# ============================================================

norm_key = function(x, ignore_case = FALSE) {
    x = gsub("[-_]", "", x)
    if(ignore_case) x = tolower(x)
    x
}


build_lookup = function(arguments, ignore_case = FALSE) {
    long = list()
    flag = list()

    for(i in seq_along(arguments)) {
        a = arguments[[i]]
        for(nm in a$arg_name) {
            if(nchar(nm) == 1) {
                if(is.null(flag[[nm]])) flag[[nm]] = i
            }
            k = norm_key(nm, ignore_case)
            if(is.null(long[[k]])) long[[k]] = i
        }
        if(nzchar(a$flag)) {
            if(is.null(flag[[a$flag]])) flag[[a$flag]] = i
        }
    }

    list(long = long, flag = flag, all_keys = names(long))
}


is_value_taking = function(a) {
    sv = a$spec_value
    !(sv$arg_type == "$" && sv$arg_mode == "l")
}


is_logical = function(a) {
    sv = a$spec_value
    sv$arg_type == "$" && sv$arg_mode == "l"
}


# try every negation form for a logical option:
#   --no-X, --not-X, --noX, --notX, --X!, --!X
try_negation = function(name, arguments, lookup, ignore_case) {
    bases = character(0)
    if(startsWith(name, "no-")) bases = c(bases, substr(name, 4, nchar(name)))
    if(startsWith(name, "not-")) bases = c(bases, substr(name, 5, nchar(name)))
    if(startsWith(name, "no")) bases = c(bases, substr(name, 3, nchar(name)))
    if(startsWith(name, "not")) bases = c(bases, substr(name, 4, nchar(name)))
    if(startsWith(name, "!")) bases = c(bases, substr(name, 2, nchar(name)))
    if(nchar(name) > 1 && substr(name, nchar(name), nchar(name)) == "!") {
        bases = c(bases, substr(name, 1, nchar(name) - 1))
    }
    for(base in unique(bases)) {
        if(!nchar(base)) next
        bkey = norm_key(base, ignore_case)
        idx = lookup$long[[bkey]]
        if(!is.null(idx) && is_logical(arguments[[idx]])) {
            return(list(index = idx, negate = TRUE))
        }
    }
    NULL
}


looks_like_option = function(tok) {
    if(identical(tok, "-")) return(FALSE)  # "-" means standard input (a value)
    if(grepl("^-", tok)) {
        # a negative number is a value, not an option: -1, -1.5, -1e-3, -.5
        if(grepl("^-[0-9]", tok)) return(FALSE)
        if(grepl("^-\\.[0-9]", tok)) return(FALSE)
        return(TRUE)
    }
    FALSE
}


# resolve a long option name (exact -> negation -> unique prefix)
resolve_long = function(name, arguments, lookup, ignore_case) {
    key = norm_key(name, ignore_case)

    idx = lookup$long[[key]]
    if(!is.null(idx)) return(list(index = idx, negate = FALSE))

    # negation --no-X / --not-X / --X! / --!X
    neg = try_negation(name, arguments, lookup, ignore_case)
    if(!is.null(neg)) return(neg)

    # unique prefix (auto abbreviation)
    cand = lookup$all_keys[startsWith(lookup$all_keys, key)]
    if(length(cand) == 1) {
        return(list(index = lookup$long[[cand]], negate = FALSE))
    } else if(length(cand) > 1) {
        stop_wrap("Option '--", name, "' is ambiguous.")
    }

    stop_wrap("Unknown option: '--", name, "'.")
}


# resolve a short (single-dash) body; returns index + negate + attached value
resolve_short = function(body, arguments, lookup, bundling, ignore_case) {
    # single character
    if(nchar(body) == 1) {
        idx = lookup$flag[[body]]
        if(!is.null(idx)) return(list(index = idx, negate = FALSE, attached = NULL))
        key = norm_key(body, ignore_case)
        cand = lookup$all_keys[startsWith(lookup$all_keys, key)]
        if(length(cand) == 1) return(list(index = lookup$long[[cand]], negate = FALSE, attached = NULL))
        if(length(cand) > 1) stop_wrap("Option '-", body, "' is ambiguous.")
        stop_wrap("Unknown option: '-", body, "'.")
    }

    # multi character: long name / abbreviation first
    key = norm_key(body, ignore_case)
    idx = lookup$long[[key]]
    if(!is.null(idx)) return(list(index = idx, negate = FALSE, attached = NULL))
    cand = lookup$all_keys[startsWith(lookup$all_keys, key)]
    if(length(cand) == 1) return(list(index = lookup$long[[cand]], negate = FALSE, attached = NULL))

    # negation (e.g. -no-verbose, -verbose!)
    neg = try_negation(body, arguments, lookup, ignore_case)
    if(!is.null(neg)) return(list(index = neg$index, negate = TRUE, attached = NULL))

    # bundling / attached value
    if(bundling) {
        ch = substr(body, 1, 1)
        idx2 = lookup$flag[[ch]]
        if(!is.null(idx2)) {
            rest = substr(body, 2, nchar(body))
            if(is_value_taking(arguments[[idx2]])) {
                return(list(index = idx2, negate = FALSE, attached = rest))
            } else {
                # logical flag; the caller will process the rest as a bundle
                return(list(index = idx2, negate = FALSE, attached = NULL, rest = rest))
            }
        }
    }

    stop_wrap("Unknown option: '-", body, "'.")
}


# gather subsequent value tokens for an array/hash option
gather_values = function(arguments, idx, first_values, argv, i, ignore_case, force_min = 0) {
    a = arguments[[idx]]
    sv = a$spec_value
    maxv = MAX_NV
    if(sv$arg_type %in% c("@", "#") && is.numeric(sv$arg_range) && length(sv$arg_range) == 2) {
        maxv = sv$arg_range[2]
    }

    vals = first_values
    while(length(vals) < maxv && i + 1 <= length(argv)) {
        nxt = argv[[i + 1]]
        if(looks_like_option(nxt) && length(vals) >= force_min) break
        vals = c(vals, nxt)
        i = i + 1
    }
    list(values = vals, i = i)
}


# consume values for a matched option starting at position i
consume = function(arguments, idx, negate, inline, has_inline, argv, i, ignore_case, value_sep = ",") {
    a = arguments[[idx]]
    sv = a$spec_value
    type = sv$arg_type
    mode = sv$arg_mode

    # logical
    if(type == "$" && mode == "l") {
        a$value = !negate
        return(i)
    }

    first = character(0)
    if(has_inline) first = inline

    if(type == "$") {
        if(length(first)) {
            val = first
        } else {
            if(i + 1 > length(argv)) stop_wrap("Option '--", a$arg_name[1], "' requires an argument.")
            i = i + 1
            val = argv[[i]]
        }
        a$value = convert_value(val, mode, name = a$arg_name[1])
        return(i)

    } else if(type == "@") {
        if(is.character(sv$arg_range)) {
            # fixed-key array -> named vector built from key=value pairs
            g = gather_values(arguments, idx, first, argv, i, ignore_case)
            assign_named_vector(a, g$values, value_sep = value_sep)
            return(g$i)
        }
        g = gather_values(arguments, idx, first, argv, i, ignore_case)
        if(length(g$values)) {
            cv = convert_value(g$values, mode, name = a$arg_name[1])
            a$value = c(a$value, cv)
        }
        return(g$i)

    } else if(type == "#") {
        # positional option should not appear; but tolerate as array-like
        g = gather_values(arguments, idx, first, argv, i, ignore_case)
        if(length(g$values)) {
            cv = convert_value(g$values, mode, name = a$arg_name[1])
            a$value = c(a$value, cv)
        }
        return(g$i)

    } else if(type == "%") {
        g = gather_values(arguments, idx, first, argv, i, ignore_case)
        assign_hash(a, g$values, value_sep = value_sep)
        return(g$i)
    }

    stop_wrap("Unknown argument type: ", type)
}


# find a sub-argument by name or alias
find_sub = function(subs, nm) {
    for(s in subs) {
        if(nm %in% s$arg_name) return(s)
    }
    NULL
}


# whether a raw value can be converted to a mode without error
matches_type = function(x, mode) {
    res = tryCatch(convert_value(x, mode), error = function(e) e)
    !inherits(res, "error")
}


# auto-detect the separator of a vector value among c(",", ";", ":", "|", "/")
# returns the first separator whose split satisfies the type and count, or NULL
detect_value_sep = function(val, mode, rng) {
    seps = c(",", ";", ":", "|", "/")
    for(sep in seps) {
        parts = strsplit(val, sep, fixed = TRUE)[[1]]
        if(length(parts) < 2) next
        if(!all(vapply(parts, function(p) matches_type(p, mode), logical(1)))) next
        if(is.numeric(rng) && length(rng) == 2) {
            if(length(parts) < rng[1] || length(parts) > rng[2]) next
        }
        return(sep)
    }
    NULL
}


# assign key=value pairs to a fixed-key array (stored as a named vector)
assign_named_vector = function(a, vals, value_sep = ",") {
    sv = a$spec_value
    keys = sv$arg_range
    alias = attr(keys, "alias")

    # start from the default value when available, so the command line only
    # overrides the keys that are actually provided
    cur = if(is.null(a$value)) {
        if(a$has_default && !is.null(a$default)) a$default else {
            switch(sv$arg_mode, i = integer(0), f = numeric(0), o = integer(0),
                   s = character(0), l = logical(0))
        }
    } else a$value

    for(v in vals) {
        if(!grepl("=", v, fixed = TRUE)) {
            stop_wrap("Option '", a$arg_name[1], "' expects key=value pairs.")
        }
        pos = regexpr("=", v, fixed = TRUE)[1]
        nm = substr(v, 1, pos - 1)
        val = substr(v, pos + 1, nchar(v))
        if(!is.null(alias) && nm %in% names(alias)) nm = alias[[nm]]
        if(!nm %in% keys && !isTRUE(attr(keys, "open"))) {
            stop_wrap("Unknown key '", nm, "' for option '", a$arg_name[1], "'.")
        }
        cur[nm] = convert_value(val, sv$arg_mode, name = paste0(a$arg_name[1], "$", nm))
    }
    a$value = cur
}


# assign name=value pairs (and bare positional values) to a hash argument
assign_hash = function(a, vals, value_sep = ",") {
    sv = a$spec_value
    subs = sv$arg_sub
    cur = if(is.null(a$value)) list() else a$value

    bare = character(0)

    for(v in vals) {
        if(grepl("=", v, fixed = TRUE)) {
            pos = regexpr("=", v, fixed = TRUE)[1]
            nm = substr(v, 1, pos - 1)
            val = substr(v, pos + 1, nchar(v))

            sub = find_sub(subs, nm)
            if(is.null(sub)) {
                if(length(subs) == 0 || isTRUE(attr(sv$arg_range, "open"))) {
                    cur[[nm]] = convert_value(val, sv$arg_mode, name = paste0(a$arg_name[1], "$", nm))
                } else {
                    stop_wrap("Unknown key '", nm, "' for hash option '", a$arg_name[1], "'.")
                }
            } else {
                m = if(is.na(sub$arg_mode)) sv$arg_mode else sub$arg_mode
                if(sub$arg_type == "@") {
                    # a vector sub-argument: split the value by the separator
                    sep = value_sep
                    if(identical(sep, "auto")) sep = detect_value_sep(val, m, sub$arg_range)
                    vals2 = if(is.null(sep)) val else strsplit(val, sep, fixed = TRUE)[[1]]
                    cv = convert_value(vals2, m, name = paste0(a$arg_name[1], "$", sub$arg_name[1]))
                    cur[[sub$arg_name[1]]] = c(cur[[sub$arg_name[1]]], cv)
                } else {
                    cv = convert_value(val, m, name = paste0(a$arg_name[1], "$", sub$arg_name[1]))
                    cur[[sub$arg_name[1]]] = cv
                }
            }
        } else {
            # bare token: a logical sub-argument name, its negation, or a positional value
            sub = find_sub(subs, v)
            if(!is.null(sub)) {
                if(sub$arg_type == "$" && sub$arg_mode == "l") {
                    cur[[sub$arg_name[1]]] = TRUE
                } else {
                    stop_wrap("Key '", v, "' for hash option '", a$arg_name[1], "' requires a value.")
                }
            } else {
                # negation of a logical sub-argument: no-bar / not-bar
                neg_base = NULL
                for(prefix in c("no-", "not-")) {
                    if(startsWith(v, prefix)) {
                        base = substr(v, nchar(prefix) + 1, nchar(v))
                        sub2 = find_sub(subs, base)
                        if(!is.null(sub2) && sub2$arg_type == "$" && sub2$arg_mode == "l") {
                            neg_base = base
                            break
                        }
                    }
                }
                if(!is.null(neg_base)) {
                    sub2 = find_sub(subs, neg_base)
                    cur[[sub2$arg_name[1]]] = FALSE
                } else {
                    bare = c(bare, v)
                }
            }
        }
    }

    # fill positional (#) sub-arguments with bare values, in declaration order
    if(length(bare)) {
        pos_idx = which(vapply(subs, function(s) identical(s$arg_type, "#"), logical(1)))
        if(length(pos_idx) == 0) {
            stop_wrap("Option '", a$arg_name[1], "' expects name=value pairs.")
        }
        k = 1L
        n_pos = length(pos_idx)
        for(j in seq_along(pos_idx)) {
            sub = subs[[pos_idx[j]]]
            m = if(is.na(sub$arg_mode)) sv$arg_mode else sub$arg_mode
            rng = sub$arg_range
            single = is.numeric(rng) && rng[1] == 1 && rng[2] == 1
            last = (j == n_pos)
            if(k > length(bare)) break
            if(single) {
                cur[[sub$arg_name[1]]] = convert_value(bare[k], m, name = paste0(a$arg_name[1], "$", sub$arg_name[1]))
                k = k + 1L
            } else if(last) {
                cur[[sub$arg_name[1]]] = convert_value(bare[k:length(bare)], m, name = paste0(a$arg_name[1], "$", sub$arg_name[1]))
                k = length(bare) + 1L
            } else {
                # variable positional, not last: greedily take values matching the type
                mx = if(is.numeric(rng)) rng[2] else MAX_NV
                taken = character(0)
                while(k <= length(bare) && length(taken) < mx && matches_type(bare[k], m)) {
                    taken = c(taken, bare[k])
                    k = k + 1L
                }
                cur[[sub$arg_name[1]]] = convert_value(taken, m, name = paste0(a$arg_name[1], "$", sub$arg_name[1]))
            }
        }
        if(k <= length(bare)) {
            stop_wrap("Too many positional values for hash option '", a$arg_name[1], "'.")
        }
    }

    a$value = cur
}


# main argv parser
parse_argv = function(arguments, argv, config = NULL, value_sep = ",") {
    if(is.null(config)) config = character(0)

    bundling = !("no_bundling" %in% config)
    ignore_case = ("ignore_case" %in% config) && !("no_ignore_case" %in% config)

    lookup = build_lookup(arguments, ignore_case)

    n = length(argv)
    i = 1L
    positional = character(0)

    while(i <= n) {
        tok = argv[[i]]

        if(tok == "--") {
            if(i < n) positional = c(positional, argv[(i + 1):n])
            break
        }

        # a single "-" is a normal value (commonly used to mean standard
        # input); it is never parsed as an option name
        if(identical(tok, "-")) {
            positional = c(positional, tok)
            i = i + 1
            next
        }

        if(grepl("^--", tok)) {
            body = substr(tok, 3, nchar(tok))
            eq = regexpr("=", body, fixed = TRUE)[1]
            if(eq > 0) {
                name = substr(body, 1, eq - 1)
                inline = substr(body, eq + 1, nchar(body))
                has_inline = TRUE
            } else {
                name = body
                inline = ""
                has_inline = FALSE
            }
            m = resolve_long(name, arguments, lookup, ignore_case)
            i = consume(arguments, m$index, m$negate, inline, has_inline, argv, i, ignore_case, value_sep = value_sep)

        } else if(grepl("^-", tok) && nchar(tok) > 1 && looks_like_option(tok)) {
            body = substr(tok, 2, nchar(tok))
            eq = regexpr("=", body, fixed = TRUE)[1]
            if(eq > 0) {
                name = substr(body, 1, eq - 1)
                inline = substr(body, eq + 1, nchar(body))
                has_inline = TRUE
            } else {
                name = body
                inline = ""
                has_inline = FALSE
            }

            m = resolve_short(name, arguments, lookup, bundling, ignore_case)

            if(!is.null(m$rest)) {
                # a logical flag followed by a bundle of more flags, e.g. -ab where a is logical
                arguments[[m$index]]$value = !m$negate
                rest = m$rest
                j = 1L
                while(j <= nchar(rest)) {
                    ch = substr(rest, j, j)
                    idx2 = lookup$flag[[ch]]
                    if(is.null(idx2)) stop_wrap("Unknown option: '-", ch, "'.")
                    a2 = arguments[[idx2]]
                    if(is_value_taking(a2)) {
                        attached = if(j < nchar(rest)) substr(rest, j + 1, nchar(rest)) else ""
                        if(nzchar(attached)) {
                            i = consume(arguments, idx2, FALSE, attached, TRUE, argv, i, ignore_case, value_sep = value_sep)
                        } else {
                            i = consume(arguments, idx2, FALSE, "", FALSE, argv, i, ignore_case, value_sep = value_sep)
                        }
                        j = nchar(rest) + 1L
                    } else {
                        a2$value = TRUE
                        j = j + 1L
                    }
                }
            } else {
                if(!is.null(m$attached) && nzchar(m$attached)) {
                    i = consume(arguments, m$index, m$negate, m$attached, TRUE, argv, i, ignore_case, value_sep = value_sep)
                } else {
                    i = consume(arguments, m$index, m$negate, inline, has_inline, argv, i, ignore_case, value_sep = value_sep)
                }
            }

        } else {
            positional = c(positional, tok)
        }

        i = i + 1
    }

    assign_positional(arguments, positional)

    invisible(NULL)
}


# assign leftover positional tokens to "#" arguments in declaration order
assign_positional = function(arguments, positional) {
    # --help / --version should work even when required positionals are absent
    if(isTRUE(arguments$help$value) || isTRUE(arguments$version$value)) return(invisible(NULL))

    idxs = which(vapply(arguments, function(a) identical(a$spec_value$arg_type, "#"), logical(1)))
    np = length(positional)

    if(length(idxs) == 0) {
        if(np) {
            stop_wrap("Unexpected positional argument(s): ", paste(positional, collapse = " "),
                      ". No positional argument is declared in the specification.")
        }
        return(invisible(NULL))
    }

    k = 1L
    for(j in seq_along(idxs)) {
        a = arguments[[idxs[j]]]
        rng = a$spec_value$arg_range

        # a fixed key set: take exactly one positional token per key and build
        # a named vector, just like "i@{a, b}" but filled from positional tokens
        if(is.character(rng)) {
            nk = length(rng)
            remaining = np - k + 1L
            if(remaining == 0) next   # no token: use the default / mandatory check
            if(remaining < nk) {
                stop_wrap("Insufficient positional arguments for '", a$arg_name[1], "'.")
            }
            cv = convert_value(positional[k:(k + nk - 1)], a$spec_value$arg_mode,
                               name = a$arg_name[1])
            names(cv) = rng
            a$value = cv
            k = k + nk
            next
        }

        mn = rng[1]; mx = rng[2]

        remaining = np - k + 1L
        last = (j == length(idxs))

        if(last) {
            take = min(remaining, mx)
            if(take > 0) {
                a$value = convert_value(positional[k:(k + take - 1)], a$spec_value$arg_mode, name = a$arg_name[1])
                k = k + take
            }
        } else {
            if(remaining < mn) {
                stop_wrap("Insufficient positional arguments for '", a$arg_name[1], "'.")
            }
            a$value = convert_value(positional[k:(k + mn - 1)], a$spec_value$arg_mode, name = a$arg_name[1])
            k = k + mn
        }
    }

    if(k <= np) {
        stop_wrap("Too many positional arguments: ", paste(positional[k:np], collapse = " "))
    }
    invisible(NULL)
}


# ============================================================
# Final validation (ranges + mandatory)
# ============================================================

# check that a default value matches the specification format
check_default_type = function(d, mode, name) {
    type_word = c("i" = "integer", "f" = "numeric", "s" = "character",
                  "l" = "logical")[[mode]]
    ok = TRUE
    if(mode %in% c("i", "f")) {
        ok = is.numeric(d)
    } else if(mode == "s") {
        ok = is.character(d)
    }
    if(!ok) {
        stop_wrap("The default of '", name, "' should be ", type_word, ".")
    }
    invisible(NULL)
}

check_default_format = function(a) {
    if(!a$has_default || is.null(a$default)) return(invisible(NULL))
    sv = a$spec_value
    type = sv$arg_type
    mode = sv$arg_mode
    d = a$default

    if(type == "$" && mode == "l") return(invisible(NULL))

    if(type == "$") {
        if(length(d) != 1) {
            stop_wrap("The default of '", a$arg_name[1], "' should be a scalar (length 1).")
        }
        check_default_type(d, mode, a$arg_name[1])
        return(invisible(NULL))
    }

    if(type %in% c("@", "#") && is.character(sv$arg_range)) {
        # fixed-key named vector
        if(!(is.atomic(d) && is.vector(d))) {
            stop_wrap("The default of '", a$arg_name[1], "' should be an atomic vector.")
        }
        if(is.null(names(d))) {
            stop_wrap("The default of '", a$arg_name[1], "' should be a named vector with keys: ",
                      paste(sv$arg_range, collapse = ", "), ".")
        }
        keys = sv$arg_range
        alias = attr(keys, "alias")
        allowed = c(keys, if(!is.null(alias)) names(alias))
        for(k in names(d)) {
            if(!k %in% allowed) {
                stop_wrap("Unknown key '", k, "' in the default of '", a$arg_name[1], "'.")
            }
        }
        check_default_type(d, mode, a$arg_name[1])
        return(invisible(NULL))
    }

    if(type == "@") {
        if(!(is.atomic(d) && is.vector(d))) {
            stop_wrap("The default of '", a$arg_name[1], "' should be an atomic vector.")
        }
        check_default_type(d, mode, a$arg_name[1])
        return(invisible(NULL))
    }

    if(type == "%") {
        if(!is.list(d) || is.null(names(d))) {
            stop_wrap("The default of '", a$arg_name[1], "' should be a named list.")
        }
        return(invisible(NULL))
    }

    invisible(NULL)
}


validate_all = function(arguments) {
    for(a in arguments) {
        if(a$arg_name[1] %in% DEFAULT_ARGUMENTS) next
        sv = a$spec_value
        type = sv$arg_type
        mode = sv$arg_mode

        check_default_format(a)

        if(type == "$" && mode == "l") {
            # logical arguments always have a value (FALSE or the negatable default)
            if(is.null(a$value)) a$value = a$default
            next
        }

        if(type == "%") {
            if(is.null(a$value)) {
                if(a$is_mandatory) {
                    stop_wrap("Argument '", a$arg_name[1], "' is mandatory.")
                }
                next
            }
            # merge default values for keys not specified on the command line
            if(a$has_default && is.list(a$default)) {
                old_nm = setdiff(names(a$default), names(a$value))
                if(length(old_nm)) {
                    a$value = c(a$value, a$default[old_nm])
                }
            }
            # declared sub-arguments are required; logical sub-arguments
            # default to FALSE when not specified
            for(sub in sv$arg_sub) {
                nm = sub$arg_name[1]
                if(!nm %in% names(a$value)) {
                    if(sub$arg_type == "$" && sub$arg_mode == "l") {
                        a$value[[nm]] = FALSE
                    } else {
                        stop_wrap("Missing value for sub-argument '", nm, "' of option '",
                                  a$arg_name[1], "'.")
                    }
                }
            }
            # validate sub-argument ranges
            for(sub in sv$arg_sub) {
                v = a$value[[sub$arg_name[1]]]
                if(!is.null(v) && sub$arg_type %in% c("@", "#")) {
                    check_range(v, sub$arg_range, sub$arg_type, a$arg_name[1])
                }
            }
            next
        }

        if(type == "@" && is.character(sv$arg_range)) {
            # fixed-key named vector: every key must be present
            if(is.null(a$value)) {
                if(a$is_mandatory) {
                    stop_wrap("Argument '", a$arg_name[1], "' is mandatory.")
                }
            } else {
                keys = sv$arg_range
                missing = setdiff(keys, names(a$value))
                if(length(missing)) {
                    stop_wrap("Missing value(s) for key(s) '", paste(missing, collapse = "', '"),
                              "' of option '", a$arg_name[1], "'.")
                }
            }
            next
        }

        if(is.null(a$value)) {
            if(a$is_mandatory) {
                stop_wrap("Argument '", a$arg_name[1], "' is mandatory.")
            }
        } else {
            check_range(a$value, sv$arg_range, type, a$arg_name[1])
        }
    }
    invisible(NULL)
}


check_range = function(v, rng, type, name) {
    if(type == "$") {
        if(length(v) > 1) stop_wrap("Only a single value is allowed for '", name, "'.")
        return(invisible(NULL))
    }

    if(is.numeric(rng) && length(rng) == 2) {
        n = length(v)
        if(n < rng[1]) {
            stop_wrap("Insufficient number of values for '", name, "': expected at least ",
                      rng[1], ", got ", n, ".")
        }
        if(n > rng[2]) {
            stop_wrap("Too many values for '", name, "': expected at most ", rng[2], ", got ", n, ".")
        }
    }
    invisible(NULL)
}


# ============================================================
# Export values
# ============================================================

export_to_env = function(arguments, envir = parent.frame()) {
    for(a in arguments) {
        if(a$arg_name[1] %in% DEFAULT_ARGUMENTS) next
        if(!is.null(a$value)) {
            assign(a$arg_name[1], a$value, envir = envir)
        }
    }
    invisible(NULL)
}


print_version_msg = function(envir, file = stdout()) {
    if(exists("VERSION", envir = envir, inherits = FALSE)) {
        cat(as.character(get("VERSION", envir = envir, inherits = FALSE)), file = file)
    } else {
        cat("No version information is found in the script.", file = file)
    }
    cat("\n", file = file)
}
