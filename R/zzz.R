.First.lib <- function (libname, pkgname, where) 
{
    require("OLIN") || stop("Bioconductor-package OLIN needed")

    if (interactive()){ require("tcltk",quietly=TRUE) || stop("R-package locfit needed")}
    else { require("tcltk",quietly=TRUE)}
    
    require("tkWidgets") || stop("Bioconductor-package tkWidgets needed")
}



