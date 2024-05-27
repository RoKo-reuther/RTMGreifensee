print_fitting_df <- function() {

    for (p in names(model_parameters$parms)) {
        cat(
            "dplyr::add_row(name = '", p, "',\t\t",
            "initial = ", model_parameters$parms[p], ",\t\t",
            "lower = ", model_parameters$parms[p] * 0.5, ",\t\t",
            "upper = ", model_parameters$parms[p] * 2, ",\t\t",
            "active = ", FALSE, ") |>",
            "\n",
            sep = ""
        )
    }

    for (p in names(model_parameters$forcings)) {
        cat(
            "dplyr::add_row(name = '", p, "',\t\t",
            "initial = ", model_parameters$forcings[p], ",\t\t",
            "lower = ", model_parameters$forcings[p] * 0.5, ",\t\t",
            "upper = ", model_parameters$forcings[p] * 2, ",\t\t",
            "active = ", FALSE, ") |>",
            "\n",
            sep = ""
        )
    }

}
