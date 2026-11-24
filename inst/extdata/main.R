#!/usr/local/bin/Rscript

library(argitect)

sub_commands(
	system.file("extdata", "sub1.R", package = "argitect"), "This is the description of sub command 1, which will be a long long long text.",
    system.file("extdata", "sub2.R", package = "argitect"), "This is the description of sub command 2, which will be a long long long text."
)

