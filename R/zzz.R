.onLoad <- function(libname, pkgname) {

    library.dynam("RTMGreifensee", pkgname, libname)

}

.onUnload <- function(libpath) {

    library.dynam.unload("RTMGreifensee", libpath)

}
