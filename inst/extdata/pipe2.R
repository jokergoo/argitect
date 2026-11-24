library(argitect)
argitect(
    "input=s#", "input"
)
if(input %in% c("-", "stdin")) {
    cat(readLines(file("stdin")), sep = "\n")
} else {
    cat(readLines(input), sep = "\n")
}