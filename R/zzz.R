
.onLoad <- function(libname, pkgname) {
    path <- Sys.which("espeak")
    if (path != "") {
        options("director.espeakPath"=path)
    } else {
        options("director.espeakPath"=NULL)
    }
}
