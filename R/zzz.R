.First.lib <- function (libname, pkgname, where) {

    require("OLIN") || stop("Bioconductor-package OLIN needed")

    if (interactive()){ require("tcltk") || stop("R-package tcltk needed")}

    if (interactive()){ require("tkWidgets") || stop("Bioconductor-package tkWidgets needed")}
}




