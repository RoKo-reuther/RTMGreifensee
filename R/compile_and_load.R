# THIS IS AUTOMATICALLY DONE BY PACKAGE INSTALLER and .onLoad
# BUT IT CAN SHOW HOW TO COMPILE AND LOAD A MODEL MANUALLY

# ==============================================================================
# > Compile Model
# ==============================================================================
compile_model <- function() {

    # !!! make it OS agnostic !!!

    prefix <- paste0(system.file("fortran", package = "RTMGreifensee"), "/")

    shlib  <- paste0(prefix, "RTMGreifensee", .Platform$dynlib.ext)

    model    <- paste0(prefix, "model.f95")
    routines <- paste0(prefix, "setup_routines.f95")
    tran1d   <- paste0(prefix, "tran1d.f95")
    tableau  <- paste0(prefix, "solve_tableau.f95")
    dgesv    <- paste0(prefix, "dgesv.f")

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

    shlib <- system.file(paste0("fortran/RTMGreifensee", .Platform$dynlib.ext), package = "RTMGreifensee")

    if (is.loaded("derivs")) {
        dyn.unload(shlib)
    }

    dyn.load(shlib)

}

unload_model <- function() {

    shlib <- system.file(paste0("fortran/RTMGreifensee", .Platform$dynlib.ext), package = "RTMGreifensee")

    dyn.unload(shlib)

}