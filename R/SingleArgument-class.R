
# ============================================================
# A single argument (option) — specification, parsing, validation, help
# ============================================================

ALL_MODES = c("i", "f", "s", "l", "o")
ALL_TYPES = c("$", "#", "@", "%")
MAX_NV = 999999L
BRACKET = c("{" = "}", "(" = ")", "[" = "]", "<" = ">")
LEFT_BRACKET = names(BRACKET)
RIGHT_BRACKET = unname(BRACKET)

DEFAULT_ARGUMENTS = c("help", "version")

# mode / type explanations used in show() and errors
EXPLAIN_MODE = c("i" = "integer", "f" = "numeric", "s" = "character",
                 "l" = "logical", "o" = "extended_integer")
EXPLAIN_TYPE = c("$" = "scalar", "%" = "hash", "@" = "vector", "#" = "positional")


SingleArgument = setRefClass("SingleArgument",
    fields = list(
        "spec" = "character",        # original (space-stripped) specification
        "arg_name" = "character",    # first name (R variable name) plus aliases
        "spec_value" = "list",       # parsed requirements (arg_mode/arg_type/arg_range/arg_sub/negatable)
        "desc" = "character",        # description
        "flag" = "character",        # single-letter flag
        "has_default" = "logical",
        "default" = "ANY",
        "is_mandatory" = "logical",
        "value" = "ANY",
        "sub_opt" = "ANY"            # descriptions for hash sub-arguments
    )
)


SingleArgument$methods(
    initialize = function(spec, desc = "", envir = parent.frame(), ...) {
        obj = callSuper(...)

        pa = parse_spec(spec)
        obj$spec = pa$spec
        obj$arg_name = pa$arg_name
        obj$spec_value = pa$spec_value

        if(length(desc) && !grepl("^\\s*$", desc)) {
            obj$desc = desc
        } else {
            obj$desc = "No description is provided."
        }

        # single-letter flag: explicit one-letter alias, otherwise decided later
        i = which(nchar(obj$arg_name) == 1)
        if(length(i)) {
            obj$flag = obj$arg_name[i]
        } else {
            obj$flag = ""
        }

        obj$has_default = FALSE
        obj$is_mandatory = TRUE
        obj$default = NULL
        obj$value = NULL
        obj$sub_opt = NULL

        obj$validate_default(envir)

        return(obj)
    }
)


# look for an existing (non-function) variable with the argument name in envir;
# it becomes the default value.
SingleArgument$methods(
    validate_default = function(envir = parent.frame()) {
        var_nm = .self$arg_name[1]
        v = NULL
        found = FALSE

        if(exists(var_nm, envir = envir, inherits = FALSE)) {
            v = get(var_nm, envir = envir, inherits = FALSE)
            if(!is.function(v)) {
                found = TRUE
            }
        }

        if(found) {
            .self$has_default = TRUE
            .self$default = v
            .self$is_mandatory = FALSE
        } else {
            # logical arguments always have a default of FALSE
            if(.self$spec_value$arg_type == "$" && .self$spec_value$arg_mode == "l") {
                .self$has_default = TRUE
                .self$default = FALSE
                .self$is_mandatory = FALSE
            } else {
                .self$has_default = FALSE
                .self$default = NULL
                .self$is_mandatory = TRUE
            }
        }

        # for non-negatable logicals, the default is always reset to FALSE
        if(.self$spec_value$arg_type == "$" && .self$spec_value$arg_mode == "l" &&
           !isTRUE(.self$spec_value$negatable) && found) {
            .self$default = FALSE
        }

        invisible(NULL)
    }
)


# convert one or more character values to the typed value according to the mode
convert_value = function(x, mode) {
    if(mode == "s") {
        return(as.character(x))
    } else if(mode == "i") {
        y = suppressWarnings(as.integer(x))
        bad = is.na(y) | !grepl("^[+-]?[0-9]+$", trimws(x))
        if(any(bad)) {
            stop_wrap("Value '", x[which(bad)[1]], "' is not a valid integer.")
        }
        return(y)
    } else if(mode == "o") {
        y = vapply(x, function(z) {
            z = trimws(z)
            if(grepl("^0[xX][0-9a-fA-F]+$", z)) return(strtoi(substr(z, 3, nchar(z)), base = 16L))
            if(grepl("^0[bB][01]+$", z)) return(strtoi(substr(z, 3, nchar(z)), base = 2L))
            if(grepl("^0[0-7]+$", z)) return(strtoi(z, base = 8L))
            if(grepl("^[+-]?[0-9]+$", z)) return(as.integer(z))
            NA_integer_
        }, integer(1), USE.NAMES = FALSE)
        if(any(is.na(y))) {
            stop_wrap("Value '", x[which(is.na(y))[1]], "' is not a valid extended integer.")
        }
        return(y)
    } else if(mode == "f") {
        y = suppressWarnings(as.numeric(x))
        if(any(is.na(y))) {
            stop_wrap("Value '", x[which(is.na(y))[1]], "' is not a valid number.")
        }
        return(y)
    } else if(mode == "l") {
        y = rep(NA, length(x))
        for(k in seq_along(x)) {
            z = tolower(trimws(x[k]))
            if(z %in% c("true", "t", "yes", "y", "1", "on")) {
                y[k] = TRUE
            } else if(z %in% c("false", "f", "no", "n", "0", "off")) {
                y[k] = FALSE
            } else {
                stop_wrap("Value '", x[k], "' is not a valid logical.")
            }
        }
        return(y)
    }
    stop_wrap("Unknown mode: ", mode)
}


# validate a raw value (after conversion) against the specification:
# range / named allowed values for scalar, array and positional arguments
SingleArgument$methods(
    validate_value = function(v, mode = .self$spec_value$arg_mode, type = .self$spec_value$arg_type) {
        rng = .self$spec_value$arg_range

        if(type == "%") {
            return(v)
        }

        # named allowed values (enum)
        if(is.character(rng)) {
            allowed = rng
            alias = attr(rng, "alias")
            if(!is.null(alias)) {
                allowed = c(allowed, names(alias))
            }
            for(z in v) {
                if(!as.character(z) %in% allowed) {
                    stop_wrap("Value '", z, "' is not allowed. Allowed values: ",
                              paste(rng, collapse = ", "), ".")
                }
            }
            return(v)
        }

        # numeric range
        if(!is.null(rng) && is.numeric(rng) && length(rng) == 2) {
            n = length(v)
            if(type == "$") {
                if(n > 1) stop_wrap("Only a single value is allowed for scalar argument.")
            } else {
                if(n < rng[1]) {
                    stop_wrap("Insufficient number of values: expected at least ", rng[1],
                              ", got ", n, ".")
                }
                if(n > rng[2]) {
                    stop_wrap("Too many values: expected at most ", rng[2], ", got ", n, ".")
                }
            }
        }
        v
    }
)


SingleArgument$methods(
    export = function(envir = parent.frame()) {
        assign(.self$arg_name[1], .self$value, envir = envir)
        invisible(NULL)
    }
)


SingleArgument$methods(
    show = function() {
        x = .self
        cat("Spec:", x$spec, "\n")
        cat("Name:", x$arg_name[1])
        if(length(x$arg_name) > 1) cat(" (aliases:", paste(x$arg_name[-1], collapse = ", "), ")")
        if(nzchar(x$flag)) cat(" (flag:", x$flag, ")")
        cat("\n")
        sv = x$spec_value
        cat("Type:", sv$arg_type, " (", EXPLAIN_TYPE[sv$arg_type], ")\n", sep = "")
        cat("Mode:", sv$arg_mode, " (", EXPLAIN_MODE[sv$arg_mode], ")\n", sep = "")
        if(sv$arg_type == "%") {
            if(length(sv$arg_sub) == 0) {
                cat("Range: unlimited\n")
            } else {
                for(sub in sv$arg_sub) {
                    cat("  sub-name:", sub$arg_name[1], "\n")
                    cat("  sub-mode:", ifelse(is.na(sub$arg_mode), "inherit", sub$arg_mode), "\n")
                    cat("  sub-type:", sub$arg_type, "\n")
                    cat("  ------\n")
                }
            }
        } else {
            cat("Range: ", paste(sv$arg_range, collapse = " - "), "\n", sep = "")
        }
        if(x$has_default) {
            cat("Has default? yes\n"); print(x$default)
        } else {
            cat("Has default? no\n")
        }
    }
)


# ============================================================
# Specification grammar
# ============================================================

# Parse one argument specification string, e.g. "number=i@{2,4}" or "verbose!"
parse_spec = function(spec) {
    spec = gsub("\\s+", "", spec)
    if(nchar(spec) == 0) stop_wrap("Argument specification should not be empty.")

    negatable = FALSE

    if(grepl("=", spec, fixed = TRUE)) {
        pos = regexpr("=", spec, fixed = TRUE)[1]
        name_part = substr(spec, 1, pos - 1)
        value_part = substr(spec, pos + 1, nchar(spec))
    } else {
        if(grepl("!$", spec)) {
            name_part = sub("!$", "", spec)
            negatable = TRUE
        } else {
            name_part = spec
            warning_wrap("Logical specification should end with '!'. ", spec, " -> ", paste0(spec, "!"))
        }
        value_part = "l"
    }

    arg_name = parse_names(name_part)

    sv = parse_value_part(value_part, spec, allow_hash = TRUE)
    sv$negatable = negatable

    list(spec = spec, arg_name = arg_name, spec_value = sv)
}


# split and validate the name list "name1|name2|..."
parse_names = function(name_part) {
    if(nchar(name_part) == 0) stop_wrap("Argument name should not be empty.")
    nm = strsplit(name_part, "|", fixed = TRUE)[[1]]

    for(i in seq_along(nm)) {
        if(i == 1) {
            if(!grepl("^[A-Za-z_][A-Za-z0-9._]*$", nm[i])) {
                stop_wrap("Argument name '", nm[i], "' should be a valid R variable name.")
            }
        } else {
            if(!grepl("^[A-Za-z0-9._-]+$", nm[i])) {
                stop_wrap("Alias '", nm[i], "' is in wrong format.")
            }
        }
    }
    nm
}


# parse the value part after "="
parse_value_part = function(value_part, spec, allow_hash = TRUE) {
    nc = nchar(value_part)

    # logical scalar: "", "l"
    if(value_part == "" || value_part == "l") {
        return(list(arg_mode = "l", arg_type = "$", arg_range = c(1L, 1L),
                    arg_sub = list(), negatable = FALSE))
    }

    ch1 = substr(value_part, 1, 1)

    # mode-first forms: i, f, s, o, then optionally a type
    if(ch1 %in% c("i", "f", "s", "o")) {
        if(nc == 1) {
            return(list(arg_mode = ch1, arg_type = "$", arg_range = c(1L, 1L),
                        arg_sub = list(), negatable = FALSE))
        }
        tail = substr(value_part, 2, nc)
        res = parse_typed(tail, arg_mode = ch1, spec = spec, allow_hash = allow_hash)
        return(res)
    }

    # type-first forms: # @ % (mode defaults to "s"), only valid with bracket
    if(ch1 %in% c("#", "@", "%")) {
        if(nc == 1) {
            stop_wrap("A bare type '", ch1, "' needs a mode or a bracketed range. In: ", spec)
        }
        res = parse_typed(value_part, arg_mode = NA_character_, spec = spec, allow_hash = allow_hash)
        if(is.na(res$arg_mode)) res$arg_mode = "s"
        return(res)
    }

    stop_wrap("Wrong specification: ", spec)
}


# parse the tail after an optional mode, which must start with a type char
parse_typed = function(tail, arg_mode, spec, allow_hash = TRUE) {
    nc = nchar(tail)
    ch = substr(tail, 1, 1)

    res = list(arg_mode = arg_mode, arg_type = NA_character_,
               arg_range = NULL, arg_sub = list(), negatable = FALSE,
               is_repeat = FALSE)

    # GetoptLong repeat shorthand: mode{min,max}, e.g. i{2,4} -> a vector
    if(ch %in% LEFT_BRACKET) {
        res$arg_type = "@"
        res$is_repeat = TRUE
        close = substr(tail, nc, nc)
        if(BRACKET[[ch]] != close) {
            stop_wrap("Bracket is not closed in: ", spec)
        }
        inner = substr(tail, 2, nc - 1)
        res$arg_range = parse_range(inner, "@", spec, allow_names = FALSE)
        return(res)
    }

    if(!(ch %in% c("$", "#", "@", "%"))) {
        stop_wrap("A type character (#, @, %) is required before a bracket. In: ", spec)
    }

    if(ch == "%" && !allow_hash) {
        stop_wrap("'%' (hash) is not allowed in sub-arguments. In: ", spec)
    }

    res$arg_type = ch

    if(nc == 1) {
        # no bracket
        if(ch == "$") res$arg_range = c(1L, 1L)
        else if(ch == "#") res$arg_range = c(1L, 1L)
        else if(ch == "@") res$arg_range = c(1L, MAX_NV)
        else if(ch == "%") res$arg_sub = list()
        return(res)
    }

    open = substr(tail, 2, 2)
    close = substr(tail, nc, nc)
    if(!(open %in% LEFT_BRACKET) || BRACKET[[open]] != close) {
        stop_wrap("Bracket is not closed in: ", spec)
    }
    inner = substr(tail, 3, nc - 1)

    if(ch == "$") {
        stop_wrap("'$' is a scalar type, no range should be specified. In: ", spec)
    } else if(ch %in% c("#", "@")) {
        res$arg_range = parse_range(inner, ch, spec)
    } else if(ch == "%") {
        res$arg_sub = parse_subs(inner, arg_mode, spec)
        nm = vapply(res$arg_sub, function(s) s$arg_name[1], character(1))
        if(length(nm)) {
            alias = do.call(c, lapply(res$arg_sub, function(s) {
                if(length(s$arg_name) > 1) {
                    structure(rep(s$arg_name[1], length(s$arg_name) - 1), names = s$arg_name[-1])
                } else character(0)
            }))
            res$arg_range = nm
            if(length(alias)) attr(res$arg_range, "alias") = alias
        }
    }

    res
}


# parse a numeric range "{min,max}" or a list of allowed names "{foo, bar|baz}"
parse_range = function(inner, type, spec, allow_names = TRUE) {
    inner = gsub("\\s+", "", inner)

    if(inner == "") {
        return(c(1L, MAX_NV))
    }

    # numeric range forms: "3", "3,", ",3", "2,4", ","
    if(grepl("^[0-9]*(,[0-9]*)?$", inner)) {
        if(inner == ",") {
            return(c(1L, MAX_NV))
        }
        if(grepl(",", inner, fixed = TRUE)) {
            pos = regexpr(",", inner, fixed = TRUE)[1]
            a = substr(inner, 1, pos - 1)
            b = substr(inner, pos + 1, nchar(inner))
            lo = if(nchar(a)) as.integer(a) else 1L
            hi = if(nchar(b)) as.integer(b) else MAX_NV
            if(lo > hi) { tmp = lo; lo = hi; hi = tmp }
            return(c(lo, hi))
        } else {
            n = as.integer(inner)
            return(c(n, n))
        }
    }

    if(!allow_names) {
        stop_wrap("Only numeric ranges are allowed in this form. In: ", spec)
    }

    # otherwise a list of allowed names
    values = strsplit(inner, ",", fixed = TRUE)[[1]]
    if(any(!grepl("^[A-Za-z0-9._-]+(\\|[A-Za-z0-9._-]+)*$", values))) {
        stop_wrap("Wrong range specification: ", spec)
    }
    nl = strsplit(values, "|", fixed = TRUE)
    alias = do.call(c, lapply(nl, function(x) {
        if(length(x) > 1) structure(rep(x[1], length(x) - 1), names = x[-1]) else character(0)
    }))
    rng = vapply(nl, function(x) x[1], character(1))
    if(length(alias)) attr(rng, "alias") = alias
    rng
}


# parse the inner content of a hash "{}" into sub-argument specs
parse_subs = function(inner, arg_mode, spec) {
    arg_sub = list()

    if(gsub("\\s+", "", inner) == "") {
        return(arg_sub)
    }

    # split by commas that are not inside brackets
    pieces = split_by_top_level_commas(inner)
    pieces = pieces[pieces != ""]

    for(v in pieces) {
        arg_sub[[length(arg_sub) + 1]] = parse_sub_spec(v, spec)
    }

    # inherit parent mode where the sub argument leaves it unspecified
    for(i in seq_along(arg_sub)) {
        if(is.na(arg_sub[[i]]$arg_mode)) {
            arg_sub[[i]]$arg_mode = if(is.na(arg_mode)) "s" else arg_mode
        }
    }

    arg_sub
}


# split a string by commas that are not nested inside brackets
split_by_top_level_commas = function(x) {
    # protect bracket contents by replacing their commas temporarily
    # find every bracket pair and mask commas inside it
    chars = strsplit(x, "", fixed = TRUE)[[1]]
    n = length(chars)
    depth = 0L
    out = integer(n)
    for(i in seq_len(n)) {
        c = chars[i]
        if(c %in% LEFT_BRACKET) depth = depth + 1L
        else if(c %in% RIGHT_BRACKET) depth = depth - 1L
        out[i] = depth
    }
    comma = which(chars == "," & out == 0L)
    if(!length(comma)) return(x)
    s = c(1L, comma + 1L)
    e = c(comma - 1L, n)
    vapply(seq_along(s), function(i) {
        paste(chars[s[i]:e[i]], collapse = "")
    }, character(1))
}


# parse one sub-argument of a hash, e.g. "foo=i@[2]" or "bar!"
parse_sub_spec = function(s, spec) {
    s = gsub("\\s+", "", s)
    negatable = FALSE

    if(grepl("=", s, fixed = TRUE)) {
        pos = regexpr("=", s, fixed = TRUE)[1]
        name_part = substr(s, 1, pos - 1)
        value_part = substr(s, pos + 1, nchar(s))
    } else {
        if(grepl("!$", s)) {
            name_part = sub("!$", "", s)
            negatable = TRUE
        } else {
            name_part = s
        }
        value_part = NA_character_
    }

    arg_name = parse_names(name_part)

    if(is.na(value_part) || value_part == "") {
        if(negatable) {
            return(list(arg_name = arg_name, arg_mode = "l", arg_type = "$",
                        arg_range = c(1L, 1L), negatable = TRUE))
        }
        return(list(arg_name = arg_name, arg_mode = NA_character_, arg_type = "$",
                    arg_range = c(1L, 1L), negatable = FALSE))
    }

    # logical sub-argument: "foo=l"
    if(value_part == "l") {
        return(list(arg_name = arg_name, arg_mode = "l", arg_type = "$",
                    arg_range = c(1L, 1L), negatable = negatable))
    }

    ch1 = substr(value_part, 1, 1)

    if(ch1 %in% c("i", "f", "s", "o")) {
        if(nchar(value_part) == 1) {
            return(list(arg_name = arg_name, arg_mode = ch1, arg_type = "$",
                        arg_range = c(1L, 1L), negatable = FALSE))
        }
        res = parse_typed(substr(value_part, 2, nchar(value_part)), arg_mode = ch1,
                          spec = spec, allow_hash = FALSE)
        res$negatable = negatable
        return(c(list(arg_name = arg_name), res))
    }

    if(ch1 %in% c("#", "@", "%")) {
        if(nchar(value_part) == 1) {
            stop_wrap("A bare type '", ch1, "' needs a mode or a bracketed range. In: ", spec)
        }
        res = parse_typed(value_part, arg_mode = NA_character_, spec = spec, allow_hash = FALSE)
        res$negatable = negatable
        return(c(list(arg_name = arg_name), res))
    }

    # bare bracket -> array with inherited mode, e.g. foo=(1,2)
    if(ch1 %in% LEFT_BRACKET) {
        close = substr(value_part, nchar(value_part), nchar(value_part))
        if(BRACKET[[ch1]] != close) stop_wrap("Bracket is not closed in: ", spec)
        inner = substr(value_part, 2, nchar(value_part) - 1)
        return(list(arg_name = arg_name, arg_mode = NA_character_, arg_type = "@",
                    arg_range = parse_range(inner, "@", spec), negatable = negatable))
    }

    stop_wrap("Wrong sub-argument specification: ", s)
}
