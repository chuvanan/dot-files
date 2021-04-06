#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(GetoptLong))
GetoptLong.options(help_style = "two-column")

argv = new.env()
argv$VERSION = "0.1.0"
argv$ncpus = 1L

GetoptLong(
    "packages=s@", "Packages to install.",
    "deps!", "Whether to install dependencies.",
    "ncpus=i", "Number of processes to use for a parallel install.",
    envir = argv
)

pkgs = argv$packages
chk = vapply(pkgs, FUN = file.exists, FUN.VALUE = logical(1))
valid_pkgs = pkgs[chk]

if (length(valid_pkgs) == length(pkgs)) {
    install.packages(pkgs = valid_pkgs, repos = NULL, type = "source", Ncpus = argv$ncpus)
} else if (length(valid_pkgs) < length(pkgs)) {
    invalid_pkgs = pkgs[!chk]
    warning(sprintf("Package(s) does not exist: %s", paste0(invalid_pkgs, collapse = ", ")), call. = FALSE)
    install.packages(pkgs = valid_pkgs, repos = NULL, type = "source", Ncpus = argv$ncpus)
}
