
# ============================================================
# A single argument (option) — specification, parsing, validation, help
# ============================================================

ALL_MODES = c("i", "f", "s", "l")
ALL_TYPES = c("$", "#", "@", "%")
MAX_NV = 999999L
BRACKET = c("{" = "}", "(" = ")", "[" = "]", "<" = ">")
LEFT_BRACKET = names(BRACKET)
RIGHT_BRACKET = unname(BRACKET)

DEFAULT_ARGUMENTS = c("help", "version")

# mode / type explanations used in show() and errors
EXPLAIN_MODE = c("i" = "integer", "f" = "numeric", "s" = "character",
                 "l" = "logical")
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
            obj$flag = obj$arg_name[i[1]]   # a single flag, not all one-letter aliases
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
            # a non-logical default for a logical argument is coerced (with a warning)
            if(.self$spec_value$arg_type == "$" && .self$spec_value$arg_mode == "l" &&
               !is.logical(v)) {
                warning_wrap("The default value of '", var_nm,
                             "' is not logical; it is coerced to logical with as.logical().")
                v = as.logical(v)
            }
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

        invisible(NULL)
    }
)


# convert one or more character values to the typed value according to the mode
# `name` is optional and is used to make conversion errors point at the argument
convert_value = function(x, mode, name = NULL) {
    err_suffix = if(is.null(name)) "" else paste0(" for option '", name, "'")
    if(mode == "s") {
        return(as.character(x))
    } else if(mode == "i") {
        # integer mode also accepts extended integer notation:
        # 0x/0X hex, 0b/0B binary, 0/0o/0O[0-7]+ octal, otherwise decimal
        y = vapply(x, function(z) {
            z = trimws(z)
            if(grepl("^0[xX][0-9a-fA-F]+$", z)) return(strtoi(substr(z, 3, nchar(z)), base = 16L))
            if(grepl("^0[bB][01]+$", z)) return(strtoi(substr(z, 3, nchar(z)), base = 2L))
            if(grepl("^0[0-7]+$", z)) return(strtoi(z, base = 8L))
            if(grepl("^0[oO][0-7]+$", z)) return(strtoi(substr(z, 3, nchar(z)), base = 8L))
            if(grepl("^[+-]?[0-9]+$", z)) return(as.integer(z))
            NA_integer_
        }, integer(1), USE.NAMES = FALSE)
        if(any(is.na(y))) {
            stop_wrap("Value '", x[which(is.na(y))[1]], "' is not a valid integer", err_suffix, ".")
        }
        return(y)
    } else if(mode == "f") {
        y = suppressWarnings(as.numeric(x))
        if(any(is.na(y))) {
            stop_wrap("Value '", x[which(is.na(y))[1]], "' is not a valid number", err_suffix, ".")
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
                stop_wrap("Value '", x[k], "' is not a valid logical", err_suffix, ".")
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
    plain_logical = FALSE

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
            plain_logical = TRUE
        }
        value_part = "l"
    }

    # validate the name before emitting any warning
    arg_name = parse_names(name_part)

    if(plain_logical) {
        warning_wrap("Logical specification should end with '!'. ", spec, " -> ", paste0(spec, "!"))
    }

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
            if(grepl("^[0-9]+$", nm[i])) {
                stop_wrap("Alias '", nm[i], "' cannot be entirely numeric.")
            }
            if(!grepl("^[A-Za-z0-9._-]+$", nm[i])) {
                stop_wrap("Alias '", nm[i], "' is in wrong format. Only letters, digits, '.', '_' and '-' are allowed.")
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

    # mode-first forms: i, f, s, then optionally a type
    if(ch1 %in% c("i", "f", "s")) {
        if(nc == 1) {
            return(list(arg_mode = ch1, arg_type = "$", arg_range = c(1L, 1L),
                        arg_sub = list(), negatable = FALSE))
        }
        return(parse_typed(substr(value_part, 2, nc), arg_mode = ch1,
                           spec = spec, allow_hash = allow_hash))
    }

    # type-first forms: # @ % (mode defaults to "s")
    if(ch1 %in% c("#", "@", "%")) {
        if(nc == 1) {
            if(ch1 == "%") {
                warning_wrap("Bare '%' is treated as an untyped list. ", spec, " -> ", paste0("s", spec))
                return(list(arg_mode = "s", arg_type = "%", arg_range = NULL,
                            arg_sub = list(), negatable = FALSE))
            }
            stop_wrap("A bare type '", ch1, "' needs a mode or a bracketed range. In: ", spec)
        }
        res = parse_typed(value_part, arg_mode = NA_character_, spec = spec, allow_hash = allow_hash)
        if(is.na(res$arg_mode)) res$arg_mode = "s"
        return(res)
    }

    # bare bracket forms: {...} / (...) / [...] / <...>
    if(ch1 %in% LEFT_BRACKET) {
        close = substr(value_part, nc, nc)
        if(BRACKET[[ch1]] != close) stop_wrap("Bracket is not closed in: ", spec)
        inner = substr(value_part, 2, nc - 1)

        if(gsub("\\s+", "", inner) == "") {
            warning_wrap("Empty '", ch1, close, "' is treated as an untyped list. ", spec, " -> %{}")
            return(list(arg_mode = "s", arg_type = "%", arg_range = NULL,
                        arg_sub = list(), negatable = FALSE))
        }

        pieces = split_by_top_level_commas(inner)
        pieces = pieces[pieces != ""]
        if(!any(grepl("=", pieces, fixed = TRUE))) {
            stop_wrap("A bare '", ch1, "...", close, "' is ambiguous; use '%' explicitly for a list. In: ", spec)
        }

        warning_wrap("Bare '", ch1, "...", close, "' is treated as a list; use '%' explicitly. ",
                     spec, " -> ", paste0("%", value_part))
        hs = parse_hash_subs(NA_character_, inner, spec)
        return(list(arg_mode = "s", arg_type = "%", arg_range = hs$arg_range,
                    arg_sub = hs$arg_sub, negatable = FALSE))
    }

    stop_wrap("Wrong specification: ", spec)
}


# parse the sub-arguments of a hash and build the key list (with aliases)
parse_hash_subs = function(arg_mode, inner, spec) {
    # a trailing comma means "open": the declared sub-arguments are required,
    # but additional names are also accepted. This is only allowed when the
    # hash mode is determined (explicit parent mode).
    open = grepl(",$", gsub("\\s+", "", inner))
    if(open && is.na(arg_mode)) {
        stop_wrap("A trailing comma is not allowed when the hash mode is not determined. In: ", spec)
    }
    arg_sub = parse_subs(inner, arg_mode, spec)
    nm = vapply(arg_sub, function(s) s$arg_name[1], character(1))
    rng = NULL
    if(length(nm)) {
        alias = do.call(c, lapply(arg_sub, function(s) {
            if(length(s$arg_name) > 1) {
                structure(rep(s$arg_name[1], length(s$arg_name) - 1), names = s$arg_name[-1])
            } else character(0)
        }))
        rng = nm
        if(length(alias)) attr(rng, "alias") = alias
    }
    if(open && !is.null(rng)) attr(rng, "open") = TRUE
    list(arg_sub = arg_sub, arg_range = rng)
}


# parse the tail after an optional mode, which must start with a type char
parse_typed = function(tail, arg_mode, spec, allow_hash = TRUE) {
    nc = nchar(tail)
    ch = substr(tail, 1, 1)

    res = list(arg_mode = arg_mode, arg_type = NA_character_,
               arg_range = NULL, arg_sub = list(), negatable = FALSE)

    # mode{...} without a type character is ambiguous
    if(ch %in% LEFT_BRACKET) {
        stop_wrap("A type character (#, @, %) is required before a bracket; use i@{...} or i#{...}. In: ", spec)
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
        if(isTRUE(attr(res$arg_range, "open")) && is.na(arg_mode)) {
            stop_wrap("A trailing comma is not allowed when the mode is not determined. In: ", spec)
        }
    } else if(ch == "%") {
        if(is.na(arg_mode) && gsub("\\s+", "", inner) == "") {
            stop_wrap("'%{}' is not allowed; use '%' for an untyped list or a typed form. In: ", spec)
        }
        hs = parse_hash_subs(arg_mode, inner, spec)
        res$arg_sub = hs$arg_sub
        res$arg_range = hs$arg_range
    }

    res
}


# parse a numeric range "{min,max}" or a list of fixed keys "{foo, bar|baz}"
parse_range = function(inner, type, spec) {
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

    # otherwise a list of fixed keys; a trailing comma means "open"
    open = grepl(",$", inner)
    if(open) inner = sub(",$", "", inner)
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
    if(open) attr(rng, "open") = TRUE
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

    # inherit parent mode where the sub argument leaves it unspecified;
    # with an implicit parent mode every sub argument must specify its type;
    # an explicit sub mode always overrides the parent mode
    parent_explicit = !is.na(arg_mode)
    for(i in seq_along(arg_sub)) {
        sub_mode = arg_sub[[i]]$arg_mode
        if(is.na(sub_mode)) {
            if(parent_explicit) {
                arg_sub[[i]]$arg_mode = arg_mode
            } else {
                stop_wrap("Sub-argument '", arg_sub[[i]]$arg_name[1],
                          "' must specify its type. In: ", spec)
            }
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
        if(s[i] > e[i]) return("")
        paste(chars[s[i]:e[i]], collapse = "")
    }, character(1))
}


# parse one sub-argument of a hash, e.g. "foo=i@[2]" or "bar!"
# a sub-argument of a hash cannot be a fixed-key named vector: its key is
# already the hash key, so only a numeric count range is meaningful
check_sub_range = function(res, spec) {
    if(res$arg_type %in% c("@", "#") && is.character(res$arg_range)) {
        stop_wrap("A fixed key set is not allowed for a sub-argument of a hash; ",
                  "only a count range is allowed here. In: ", spec)
    }
    res
}

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

    if(ch1 %in% c("i", "f", "s")) {
        if(nchar(value_part) == 1) {
            return(list(arg_name = arg_name, arg_mode = ch1, arg_type = "$",
                        arg_range = c(1L, 1L), negatable = FALSE))
        }
        res = parse_typed(substr(value_part, 2, nchar(value_part)), arg_mode = ch1,
                          spec = spec, allow_hash = FALSE)
        res = check_sub_range(res, spec)
        res$negatable = negatable
        return(c(list(arg_name = arg_name), res))
    }

    if(ch1 %in% c("#", "@", "%")) {
        if(nchar(value_part) == 1 && ch1 == "%") {
            stop_wrap("'%' (hash) is not allowed in sub-arguments. In: ", spec)
        }
        res = parse_typed(value_part, arg_mode = NA_character_, spec = spec, allow_hash = FALSE)
        res = check_sub_range(res, spec)
        res$negatable = negatable
        return(c(list(arg_name = arg_name), res))
    }

    # bare bracket -> array with inherited mode, e.g. foo=(1,2)
    if(ch1 %in% LEFT_BRACKET) {
        close = substr(value_part, nchar(value_part), nchar(value_part))
        if(BRACKET[[ch1]] != close) stop_wrap("Bracket is not closed in: ", spec)
        inner = substr(value_part, 2, nchar(value_part) - 1)
        res = list(arg_name = arg_name, arg_mode = NA_character_, arg_type = "@",
                   arg_range = parse_range(inner, "@", spec), negatable = negatable)
        return(check_sub_range(res, spec))
    }

    stop_wrap("Wrong sub-argument specification: ", s)
}
