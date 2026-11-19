
stop_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x, call. = FALSE)
}

warning_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    warning(x, call. = FALSE)
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


# simple color helpers (no dependency on crayon)
red = function(x) x
green = function(x) x
