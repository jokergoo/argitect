# Path of the Rscript executable. A bare "Rscript" is rejected by recent R
# checks ("'Rscript' should not be used without a path"), so always use the
# full path from R.home("bin"); on Windows it may live in a "x64" subdirectory
# and carry an ".exe" suffix.
rscript_bin = function() {
    bin = R.home("bin")
    guess = c(file.path(bin, "Rscript"),
              file.path(bin, "Rscript.exe"),
              file.path(bin, "x64", "Rscript.exe"),
              file.path(bin, "x64", "Rscript"))
    for(p in guess) {
        if(file.exists(p)) return(p)
    }
    file.path(bin, "Rscript")
}
