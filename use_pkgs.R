# use_pkgs
# ensure all packages are installed and loaded into the library

# list packages

pkgs <- c("my_packages")

use_pkgs <- function(pkgs, quietly = TRUE) {
     pkgs <- unique(pkgs)
     to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
     if (length(to_install)) install.packages(to_install, quiet = quietly)
     ok <- sapply(pkgs, require, character.only = TRUE, quietly = quietly)
     if (!all(ok)) stop("Could not load: ", paste(pkgs[!ok], collapse = ", "))
     invisible(ok)
}