
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
#'   \code{(specification, description)} pairs, a two-column matrix, or a
#'   single text template. See Details.
#' @param help_head Head of the help message shown by \code{--help}.
#' @param help_foot Foot of the help message shown by \code{--help}.
#' @param envir Environment where default values are looked up and parsed
#'   values are exported. A list is also accepted and coerced to an
#'   environment.
#' @param argv_str A string containing command-line arguments. Only for
#'   testing purposes.
#' @param template_control A list controlling template help rendering, e.g.
#'   \code{list(opt_width = 21)}.
#' @param help_style The style of the help message: \code{"one-column"} or
#'   \code{"two-column"}.
#'
#' @details
#' Each argument specification is \code{name[|alias...]=spec}. The value part
#' \code{spec} consists of an optional \emph{mode} (\code{i} integer,
#' \code{f} numeric, \code{s} character, \code{l} logical, \code{o} extended
#' integer) and an optional \emph{type} (\code{$} scalar, \code{#}
#' positional, \code{@} vector, \code{\%} hash), optionally followed by a
#' bracketed range or sub-argument list.
#'
#' \itemize{
#'   \item \code{"number=i"} -- a single integer scalar.
#'   \item \code{"name=s"} -- a single character scalar.
#'   \item \code{"length|size=i"} -- an integer scalar with two names.
#'   \item \code{"tag=i@"} -- an integer vector.
#'   \item \code{"tag=i{2,}"} -- an integer vector with at least two elements.
#'   \item \code{"param=s\%"} -- name=value pairs with character values.
#'   \item \code{"input=s#"} -- one positional argument.
#'   \item \code{"files=s#\{\}"} -- one or more positional arguments.
#'   \item \code{"verbose!"} -- a logical option, negatable with
#'         \code{--no-verbose}.
#' }
#'
#' If a variable with the first name already exists in \code{envir} (and is
#' not a function), it is used as the default value and the option becomes
#' optional. Logical options always default to \code{FALSE}.
#'
#' @return Invisibly, a list of the parsed argument objects. Parsed values are
#'   also exported to \code{envir} as variables named after the first
#'   specification name.
#'
#' @export
argitect = function(..., help_head = NULL, help_foot = NULL, envir = parent.frame(),
    argv_str = NULL, template_control = list(),
    help_style = argitect.options("help_style")) {

    if(is.list(envir)) envir = as.environment(envir)

    if(is.null(get_scriptname())) {
        IS_UNDER_COMMAND_LINE = FALSE
    } else {
        IS_UNDER_COMMAND_LINE = TRUE
    }

    if(is.null(argv_str)) {
        argv_str = argitect.options("__argv_str__")
    }

    spec_info = parse_specification(list(...))

    # build argument objects
    arguments = list()
    for(i in seq_len(nrow(spec_info$spec))) {
        a = SingleArgument(spec = spec_info$spec[i, 1], desc = spec_info$spec[i, 2], envir = envir)
        arguments[[length(arguments) + 1]] = a
    }
    names(arguments) = vapply(arguments, function(a) a$arg_name[1], character(1))

    if(any(names(arguments) %in% DEFAULT_ARGUMENTS)) {
        stop_wrap("`help` and `version` are reserved options. Please do not use them.")
    }

    # attach sub-option descriptions (spec of the form "name$sub")
    if(length(spec_info$sub_opt)) {
        for(parent in names(spec_info$sub_opt)) {
            if(parent %in% names(arguments)) {
                arguments[[parent]]$sub_opt = c(arguments[[parent]]$sub_opt,
                    spec_info$sub_opt[[parent]])
            }
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

    # parse argv
    if(is.null(argv_str)) {
        argv = commandArgs(TRUE)
    } else {
        argv = strsplit(argv_str, "\\s+")[[1]]
        if(length(argv) == 1 && argv[1] == "") argv = character(0)
    }

    config = argitect.options("config")

    err = NULL
    tryCatch({
        parse_argv(arguments, argv, config = config)
    }, error = function(e) {
        err <<- conditionMessage(e)
    })

    if(!is.null(err)) {
        if(IS_UNDER_COMMAND_LINE) {
            cat(red(paste0("Error: ", err, "\n")), file = stderr())
            print_help_msg(arguments, file = stderr(), head = help_head, foot = help_foot,
                template = spec_info$template, template_control = template_control,
                style = help_style, opt_group = spec_info$opt_group,
                opt_group_desc = spec_info$opt_group_desc)
            q(save = "no", status = 127)
        } else {
            stop_wrap("Error: ", err)
        }
    }

    # --help
    if(isTRUE(arguments$help$value)) {
        print_help_msg(arguments, file = stdout(), head = help_head, foot = help_foot,
            template = spec_info$template, template_control = template_control,
            style = help_style, opt_group = spec_info$opt_group,
            opt_group_desc = spec_info$opt_group_desc)
        if(IS_UNDER_COMMAND_LINE) {
            q(save = "no", status = 0)
        } else {
            return(invisible(NULL))
        }
    }

    # --version
    if(isTRUE(arguments$version$value)) {
        print_version_msg(envir, file = stdout())
        if(IS_UNDER_COMMAND_LINE) {
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
            cat(red(paste0("Error: ", err, "\n")), file = stderr())
            print_help_msg(arguments, file = stderr(), head = help_head, foot = help_foot,
                template = spec_info$template, template_control = template_control,
                style = help_style, opt_group = spec_info$opt_group,
                opt_group_desc = spec_info$opt_group_desc)
            q(save = "no", status = 127)
        } else {
            stop_wrap("Error: ", err)
        }
    }

    export_to_env(arguments, envir = envir)

    invisible(arguments)
}


# == title
# Alias of argitect(), consistent with GetOptions() in Getopt::Long
#' Alias of \code{\link{argitect}()}
#'
#' This function is the same as \code{\link{argitect}}. It is provided for
#' consistency with the \code{GetOptions()} subroutine in the Perl module
#' \code{Getopt::Long}.
#'
#' @param ... Passed to \code{\link{argitect}}.
#' @param envir Environment where default values are looked up and parsed
#'   values are exported.
#'
#' @return See \code{\link{argitect}}.
#'
#' @export
GetOptions = function(..., envir = parent.frame()) argitect(..., envir = envir)


# ============================================================
# Specification parsing
# ============================================================

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


parse_spec_template = function(template, tag = argitect.options("template_tag")) {
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
    first_letters = vapply(arguments, function(a) substr(a$arg_name[1], 1, 1), character(1))
    tb = table(first_letters)

    for(i in seq_along(arguments)) {
        a = arguments[[i]]
        if(nzchar(a$flag)) next  # already has an explicit short flag
        le = first_letters[i]
        if(tb[[le]] == 1) {
            a$flag = le
            a$arg_name = c(a$arg_name, le)
            arguments[[i]] = a
        }
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


is_negatable = function(a) {
    sv = a$spec_value
    sv$arg_type == "$" && sv$arg_mode == "l" && isTRUE(sv$negatable)
}


looks_like_option = function(tok) {
    if(grepl("^-", tok)) {
        if(grepl("^-[0-9.]+$", tok)) return(FALSE)  # negative number is a value
        return(TRUE)
    }
    FALSE
}


# resolve a long option name (exact -> negatable -> unique prefix)
resolve_long = function(name, arguments, lookup, ignore_case) {
    key = norm_key(name, ignore_case)

    idx = lookup$long[[key]]
    if(!is.null(idx)) return(list(index = idx, negate = FALSE))

    # negation --no-X / --noX
    for(prefix in c("no-", "no")) {
        if(startsWith(name, prefix)) {
            base = substr(name, nchar(prefix) + 1, nchar(name))
            bkey = norm_key(base, ignore_case)
            idx2 = lookup$long[[bkey]]
            if(!is.null(idx2) && is_negatable(arguments[[idx2]])) {
                return(list(index = idx2, negate = TRUE))
            }
        }
    }

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
        stop_wrap("Unknown option: '-", body, "'.")
    }

    # multi character: long name / abbreviation first
    key = norm_key(body, ignore_case)
    idx = lookup$long[[key]]
    if(!is.null(idx)) return(list(index = idx, negate = FALSE, attached = NULL))
    cand = lookup$all_keys[startsWith(lookup$all_keys, key)]
    if(length(cand) == 1) return(list(index = lookup$long[[cand]], negate = FALSE, attached = NULL))

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
consume = function(arguments, idx, negate, inline, has_inline, argv, i, ignore_case) {
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
        a$value = convert_value(val, mode)
        return(i)

    } else if(type == "@") {
        force_min = if(isTRUE(sv$is_repeat) && is.numeric(sv$arg_range) && length(sv$arg_range) == 2) sv$arg_range[1] else 0
        g = gather_values(arguments, idx, first, argv, i, ignore_case, force_min = force_min)
        if(length(g$values)) {
            cv = convert_value(g$values, mode)
            a$value = c(a$value, cv)
        }
        return(g$i)

    } else if(type == "#") {
        # positional option should not appear; but tolerate as array-like
        g = gather_values(arguments, idx, first, argv, i, ignore_case)
        if(length(g$values)) {
            cv = convert_value(g$values, mode)
            a$value = c(a$value, cv)
        }
        return(g$i)

    } else if(type == "%") {
        g = gather_values(arguments, idx, first, argv, i, ignore_case)
        assign_hash(a, g$values)
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


# assign name=value pairs (and bare positional values) to a hash argument
assign_hash = function(a, vals) {
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
                if(length(subs) == 0) {
                    cur[[nm]] = convert_value(val, sv$arg_mode)
                } else {
                    stop_wrap("Unknown key '", nm, "' for hash option '", a$arg_name[1], "'.")
                }
            } else {
                m = if(is.na(sub$arg_mode)) sv$arg_mode else sub$arg_mode
                cv = convert_value(val, m)
                if(sub$arg_type == "@") {
                    cur[[sub$arg_name[1]]] = c(cur[[sub$arg_name[1]]], cv)
                } else {
                    cur[[sub$arg_name[1]]] = cv
                }
            }
        } else {
            # bare token: a logical sub-argument name, or a positional value
            sub = find_sub(subs, v)
            if(!is.null(sub)) {
                if(sub$arg_type == "$" && sub$arg_mode == "l") {
                    cur[[sub$arg_name[1]]] = TRUE
                } else {
                    stop_wrap("Key '", v, "' for hash option '", a$arg_name[1], "' requires a value.")
                }
            } else {
                bare = c(bare, v)
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
        for(ps in pos_idx) {
            sub = subs[[ps]]
            m = if(is.na(sub$arg_mode)) sv$arg_mode else sub$arg_mode
            rng = sub$arg_range
            single = is.numeric(rng) && rng[1] == 1 && rng[2] == 1
            if(k > length(bare)) break
            if(single) {
                cur[[sub$arg_name[1]]] = convert_value(bare[k], m)
                k = k + 1L
            } else {
                cur[[sub$arg_name[1]]] = convert_value(bare[k:length(bare)], m)
                k = length(bare) + 1L
            }
        }
        if(k <= length(bare)) {
            stop_wrap("Too many positional values for hash option '", a$arg_name[1], "'.")
        }
    }

    a$value = cur
}


# main argv parser
parse_argv = function(arguments, argv, config = NULL) {
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
            i = consume(arguments, m$index, m$negate, inline, has_inline, argv, i, ignore_case)

        } else if(grepl("^-", tok) && nchar(tok) > 1) {
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
                            i = consume(arguments, idx2, FALSE, attached, TRUE, argv, i, ignore_case)
                        } else {
                            i = consume(arguments, idx2, FALSE, "", FALSE, argv, i, ignore_case)
                        }
                        j = nchar(rest) + 1L
                    } else {
                        a2$value = TRUE
                        j = j + 1L
                    }
                }
            } else {
                if(!is.null(m$attached) && nzchar(m$attached)) {
                    i = consume(arguments, m$index, m$negate, m$attached, TRUE, argv, i, ignore_case)
                } else {
                    i = consume(arguments, m$index, m$negate, inline, has_inline, argv, i, ignore_case)
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
    idxs = which(vapply(arguments, function(a) identical(a$spec_value$arg_type, "#"), logical(1)))
    np = length(positional)

    if(length(idxs) == 0) {
        # no positional arguments declared: silently ignore leftovers
        # (matches GetoptLong, which does not check stray arguments)
        return(invisible(NULL))
    }

    k = 1L
    for(j in seq_along(idxs)) {
        a = arguments[[idxs[j]]]
        rng = a$spec_value$arg_range
        if(is.character(rng)) { mn = 1L; mx = 1L }
        else { mn = rng[1]; mx = rng[2] }

        remaining = np - k + 1L
        last = (j == length(idxs))

        if(last) {
            take = min(remaining, mx)
            if(take > 0) {
                a$value = convert_value(positional[k:(k + take - 1)], a$spec_value$arg_mode)
                k = k + take
            }
        } else {
            if(remaining < mn) {
                stop_wrap("Insufficient positional arguments for '", a$arg_name[1], "'.")
            }
            a$value = convert_value(positional[k:(k + mn - 1)], a$spec_value$arg_mode)
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

validate_all = function(arguments) {
    for(a in arguments) {
        if(a$arg_name[1] %in% DEFAULT_ARGUMENTS) next
        sv = a$spec_value
        type = sv$arg_type
        mode = sv$arg_mode

        if(type == "$" && mode == "l") {
            # logical arguments always have a value (FALSE or the negatable default)
            if(is.null(a$value)) a$value = a$default
            next
        }

        if(type == "%") {
            # merge default values for keys not specified on the command line
            if(!is.null(a$value) && a$has_default && is.list(a$default)) {
                old_nm = setdiff(names(a$default), names(a$value))
                if(length(old_nm)) {
                    a$value = c(a$value, a$default[old_nm])
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

    if(is.character(rng)) {
        allowed = rng
        alias = attr(rng, "alias")
        if(!is.null(alias)) allowed = c(allowed, names(alias))
        for(z in v) {
            if(!as.character(z) %in% allowed) {
                stop_wrap("Value '", z, "' is not allowed for '", name, "'. Allowed values: ",
                          paste(rng, collapse = ", "), ".")
            }
        }
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
