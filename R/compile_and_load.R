#' @useDynLib RTMGreifensee
NULL

# Compilation of the model is automatically done during the package installation.
# Loading the shared libraries is automatically done when the package is loaded.
# The following functions just display how compilation and loading could be done manually.

# ==============================================================================
# > Compile Model
# ==============================================================================
compile_model <- function() {

    lib_path <- "your_lib_path"
    shlib    <- paste0(lib_path, "/RTMGreifensee", .Platform$dynlib.ext)

    src_path <- "your_source_path"
    model    <- paste0(src_path, "/model.f95")
    routines <- paste0(src_path, "/setup_routines.f95")
    tran1d   <- paste0(src_path, "/tran1d.f95")
    tableau  <- paste0(src_path, "/solve_tableau.f95")
    dgesv    <- paste0(src_path, "/dgesv.f")

    command <- paste0(
        "R CMD SHLIB -c -o ", shlib, " ",
        model, " ",
        routines, " ",
        tran1d, " ",
        tableau, " ",
        dgesv
    )
    
    system(command)

}


# ==============================================================================
# > (Un)Load Model
# ==============================================================================
load_model <- function() {

    lib_path <- "your_lib_path"
    shlib    <- paste0(lib_path, "/RTMGreifensee", .Platform$dynlib.ext)

    if (is.loaded("derivs")) {
        dyn.unload(shlib)
    }

    dyn.load(shlib)

}

unload_model <- function() {

    lib_path <- "your_lib_path"
    shlib    <- paste0(lib_path, "/RTMGreifensee", .Platform$dynlib.ext)

    dyn.unload(shlib)

}
