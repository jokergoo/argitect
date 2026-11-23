
# ============================================================
# Text helpers (cli-backed colors, glue-backed descriptions)
# ============================================================

red    = function(...) col_red(paste0(...))
green  = function(...) col_green(paste0(...))
yellow = function(...) col_yellow(paste0(...))
blue   = function(...) col_blue(paste0(...))
cyan   = function(...) col_cyan(paste0(...))
silver = function(...) col_silver(paste0(...))
bold   = function(...) style_bold(paste0(...))


# render a description through glue so that "{{expr}}" interpolation and richer
# inline text are available in description strings; the cli helpers are exposed
# when an expression is present
render_desc = function(x, .envir = NULL) {
    if(length(x) == 0) return(x)
    caller = parent.frame()
    vapply(x, function(s) {
        if(is.na(s) || !nzchar(s)) return(s)
        if(!grepl("{{", s, fixed = TRUE)) return(s)
        env = if(is.null(.envir)) glue_env(caller) else .envir
        as.character(glue::glue(s, .open = "{{", .close = "}}", .envir = env))
    }, character(1), USE.NAMES = FALSE)
}


# an evaluation environment for glue that also exposes the cli functions, so
# that descriptions can use e.g. {{ cli_h2("Options") }} without a namespace.
# cli's printing helpers are wrapped so that they return their formatted text
# instead of writing to the console (which makes them usable inside glue).
CLI_DISPLAY_FUNS = c(
    "cli_h1", "cli_h2", "cli_h3",
    "cli_ul", "cli_ol", "cli_dl", "cli_bullets", "cli_bullets_raw",
    "cli_text", "cli_verbatim", "cli_rule", "cli_code", "cli_blockquote",
    "cli_alert", "cli_alert_success", "cli_alert_info", "cli_alert_warning",
    "cli_alert_danger", "cli_alert_light", "cli_div", "cli_end"
)

# wrap a cli printing helper so that it returns the rendered text (without the
# surrounding blank lines that cli adds for block elements such as headings)
capture_cli_fun = function(f) {
    force(f)
    function(...) {
        txt = paste(cli_fmt(f(...)), collapse = "\n")
        txt = sub("^\\n+", "", txt)
        txt = sub("\\n+$", "", txt)
        txt
    }
}

glue_env = function(caller = parent.frame()) {
    env = new.env(parent = caller)
    cli_ns = asNamespace("cli")
    for(nm in getNamespaceExports("cli")) {
        if(!exists(nm, envir = caller, inherits = TRUE)) {
            assign(nm, get(nm, envir = cli_ns), envir = env)
        }
    }
    for(nm in CLI_DISPLAY_FUNS) {
        if(exists(nm, envir = cli_ns)) {
            assign(nm, capture_cli_fun(get(nm, envir = cli_ns)), envir = env)
        }
    }
    env
}


stop_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

warning_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    warning(yellow(x), call. = FALSE)
}

message_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x)
}


# wrap a block of plain text to a given width
# prefix is prepended to every line (used for indentation of wrapped text)
format_text = function(text, prefix = "  ", width = argitect.options("help_width")) {
    if(length(text) == 0) return("")
    text = as.character(text)
    text = render_desc(text)
    text = gsub("^\\s+", "", text)
    if(is.null(prefix)) {
        text = strwrap(text, width = 0.9 * width)
        text = paste(text, collapse = "\n")
    } else {
        text = strwrap(text, width = max(10, 0.9 * (width - nchar(prefix) - 2)))
        text = paste(prefix, "  ", text, sep = "", collapse = "\n")
    }
    text
}


# find code blocks matching a pattern such as "@{CODE}" or "<CODE>"
find_code = function(m, text) {
    if(length(text) != 1) {
        stop_wrap("text must be length of 1.")
    }

    m2 = gsub("CODE", ".+?", m, fixed = TRUE)

    reg = gregexpr(m2, text)[[1]]
    v1 = character(0)
    if(reg[1] > -1) {
        v1 = sapply(seq_along(reg), function(i) {
            substr(text, as.numeric(reg)[i], as.numeric(reg)[i] + attr(reg, "match.length")[i] - 1)
        })
    }
    edge = strsplit(m, "CODE", fixed = TRUE)[[1]]
    v2 = gsub(paste0("^", edge[1], "|", edge[2], "$"), "", v1)

    return(list(template = v1, code = v2))
}


# signal that a script sourced by source_script() should stop quietly
# (used when --help / --version is handled outside a real command line)
stop_script = function() {
    stop(structure(list(message = "", call = NULL),
        class = c("argitect_exit", "condition")))
}
