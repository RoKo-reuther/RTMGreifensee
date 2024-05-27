# =============================================================================
# > Greifensee Approximation Functions
# =============================================================================
get_approxfun <- function(species) {

    data <- subset(RTMGreifensee::greifensee_data, name == species)
    return(approxfun(data$depth, data$value, rule = 2))

}

get_greifensee_approxfuncs <- function() {

    species   <- unique(RTMGreifensee::greifensee_data$name)

    functions <- lapply(species, get_approxfun)

    names(functions) <- species

    return(functions)

}


# =============================================================================
# > Target Function
# =============================================================================
target_diff <- function(species, std, data_approx) {

    model_result  <- subset(std$profiles, name == species, "value", drop = TRUE)
    measured_data <- data_approx[[species]](std$original$xmid)

    return(sum((model_result - measured_data)^2))

}

target_func <- function(par, parnames, data_approx, active_targets) {

    model_parameters <- do.call("set_parameters", as.list(setNames(par, parnames)))

    std <- tryCatch(
        solve_steady(model_parameters),
        error = function(e) {
            return(NULL)
        }
    )

    if (is.null(std) || !attributes(std$original)$steady) {
        return(1e20)
    }

    diff <- sapply(active_targets, target_diff,  std = std, data_approx = data_approx)

    return(sum(diff))

}


# =============================================================================
# > Fitting Function
# =============================================================================
fit_greifensee <- function(fitting_dataframe, active_targets, target_func) {

    # get approximation functions for measured data
    data_approx <- get_greifensee_approxfuncs()

    # get parnames
    parnames <- subset(fitting_dataframe, active)$name

    # get initial parameter values
    par_initial <- subset(fitting_dataframe, active)$initial

    # get lower boundaries
    lower <- subset(fitting_dataframe, active)$lower

    # get upper boundaries
    upper <- subset(fitting_dataframe, active)$upper

    fitting_result <- optim(
        par = par_initial,
        fn = target_func,
        parnames = parnames,
        data_approx = data_approx,
        active_targets = active_targets,
        method = "L-BFGS-B",
        lower = lower,
        upper = upper,
        control = list()
    )

    model_parameters <- do.call("set_parameters", as.list(setNames(fitting_result$par, parnames)))

    std <- solve_steady(model_parameters)

    return(list(
        fit = fitting_result,
        std = std,
        plot = plot_fit(std, active_targets)
    ))

}


# =============================================================================
# > Plot Fit
# =============================================================================
plot_fit <- function(std, active_targets) {

    all_targets <- c("TOT_NO3", "TOT_NH4", "TOT_SO4", "TOT_H2S", "TOT_H2PO4", "pH", "TOT_H", "Ca2", "Fe2", "Mn2")

    df <- rbind(std$profiles, RTMGreifensee::reference_state$profiles, RTMGreifensee::greifensee_data)

    plots <- list()

    for (i in seq_along(all_targets)) {

        target <- all_targets[i]

        plots[[i]] <- plot_std_profile_echart(
            df = df,
            entity = target,
            main = ifelse(target %in% active_targets, paste(target, "(target)"), paste(target, "(no target)"))
        )
    }

    final_plot <- do.call(echarts4r::e_arrange, plots)

    return(final_plot)

}



# =============================================================================
# > Fitting Dataframe
# =============================================================================
#' Fitting Dataframe
#'
#' A input dataframe to {fit_greifensee} containing parameter names,
#' initial values, lower and upper boundaries as well as an inicator if
#' parameter should be fitted.
#'
#' @export
fitting_dataframe <- data.frame(
    name    = character(),
    initial = double(),
    lower   = double(),
    upper   = double(),
    active  = logical()
) |>
dplyr::add_row(name = 'k_OM_deg_a',             initial = 0.2,                  lower = 0.1,                    upper = 0.4,                    active = TRUE) |>
dplyr::add_row(name = 'k_OM_deg_b',             initial = 0.022,                lower = 0.011,                  upper = 0.044,                  active = TRUE) |>
dplyr::add_row(name = 'f_OM_b',                 initial = 0.1509434,            lower = 0.0754717,              upper = 0.3018868,              active = FALSE) |>
dplyr::add_row(name = 'f_OM_c',                 initial = 0.009433962,          lower = 0.004716981,            upper = 0.01886792,             active = FALSE) |>
dplyr::add_row(name = 'lambda',                 initial = 0.6,                  lower = 0.3,                    upper = 1.2,                    active = FALSE) |>
dplyr::add_row(name = 'K_O2',                   initial = 0.02,                 lower = 0.01,                   upper = 0.04,                   active = TRUE) |>
dplyr::add_row(name = 'K_NO3',                  initial = 0.002,                lower = 0.001,                  upper = 0.004,                  active = TRUE) |>
dplyr::add_row(name = 'K_MnO2',                 initial = 40,                   lower = 20,                     upper = 80,                     active = TRUE) |>
dplyr::add_row(name = 'K_FeOH3',                initial = 5000,                 lower = 2500,                   upper = 10000,                  active = TRUE) |>
dplyr::add_row(name = 'K_SO4',                  initial = 1.6,                  lower = 0.8,                    upper = 3.2,                    active = TRUE) |>
dplyr::add_row(name = 'k_H2S_ox_by_O2',         initial = 160,                  lower = 80,                     upper = 320,                    active = FALSE) |>
dplyr::add_row(name = 'k_SO4_red_by_CH4',       initial = 50,                   lower = 25,                     upper = 100,                    active = FALSE) |>
dplyr::add_row(name = 'k_FeOH3_red_by_H2S',     initial = 100,                  lower = 50,                     upper = 200,                    active = FALSE) |>
dplyr::add_row(name = 'k_FeOH3_formation',      initial = 35000,                lower = 17500,                  upper = 70000,                  active = FALSE) |>
dplyr::add_row(name = 'K_PO4',                  initial = 1e-05,                lower = 5e-06,                  upper = 2e-05,                  active = FALSE) |>
dplyr::add_row(name = 'k_Fe2_ox_by_MnO2',       initial = 3000,                 lower = 1500,                   upper = 6000,                   active = FALSE) |>
dplyr::add_row(name = 'k_NH4_ox_by_O2',         initial = 5000,                 lower = 2500,                   upper = 10000,                  active = FALSE) |>
dplyr::add_row(name = 'k_H2S_ox_by_MnO2',       initial = 20,                   lower = 10,                     upper = 40,                     active = FALSE) |>
dplyr::add_row(name = 'k_FeS_ox_by_O2',         initial = 320,                  lower = 160,                    upper = 640,                    active = FALSE) |>
dplyr::add_row(name = 'k_CH4_ox_by_O2',         initial = 1e+07,                lower = 5e+06,                  upper = 2e+07,                  active = FALSE) |>
dplyr::add_row(name = 'k_precip_FeCO3',         initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'k_diss_FeCO3',           initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'Ksp_FeCO3',              initial = 3.981072e-05,         lower = 1.990536e-05,           upper = 7.962143e-05,           active = FALSE) |>
dplyr::add_row(name = 'k_precip_FeS',           initial = 0.7,                  lower = 0.35,                   upper = 1.4,                    active = FALSE) |>
dplyr::add_row(name = 'k_diss_FeS',             initial = 0.01,                 lower = 0.005,                  upper = 0.02,                   active = FALSE) |>
dplyr::add_row(name = 'Ksp_FeS',                initial = 0.1216186,            lower = 0.0608093,              upper = 0.2432372,              active = FALSE) |>
dplyr::add_row(name = 'k_precip_vivianite',     initial = 0.011,                lower = 0.0055,                 upper = 0.022,                  active = FALSE) |>
dplyr::add_row(name = 'k_diss_vivianite',       initial = 0.0053,               lower = 0.00265,                upper = 0.0106,                 active = FALSE) |>
dplyr::add_row(name = 'Ksp_vivianite',          initial = 1e-21,                lower = 5e-22,                  upper = 2e-21,                  active = FALSE) |>
dplyr::add_row(name = 'k_precip_CaCO3',         initial = 0.11,                 lower = 0.055,                  upper = 0.22,                   active = FALSE) |>
dplyr::add_row(name = 'k_diss_CaCO3',           initial = 0.125,                lower = 0.0625,                 upper = 0.25,                   active = FALSE) |>
dplyr::add_row(name = 'Ksp_CaCO3',              initial = 0.005463664,          lower = 0.002731832,            upper = 0.01092733,             active = FALSE) |>
dplyr::add_row(name = 'k_precip_apatite',       initial = 1.4e-06,              lower = 7e-07,                  upper = 2.8e-06,                active = FALSE) |>
dplyr::add_row(name = 'k_diss_apatite',         initial = 0.037,                lower = 0.0185,                 upper = 0.074,                  active = FALSE) |>
dplyr::add_row(name = 'Ksp_apatite',            initial = 6e-20,                lower = 3e-20,                  upper = 1.2e-19,                active = FALSE) |>
dplyr::add_row(name = 'k_precip_MnCO3',         initial = 270,                  lower = 135,                    upper = 540,                    active = FALSE) |>
dplyr::add_row(name = 'k_diss_MnCO3',           initial = 0.8,                  lower = 0.4,                    upper = 1.6,                    active = FALSE) |>
dplyr::add_row(name = 'Ksp_MnCO3',              initial = 0.003162278,          lower = 0.001581139,            upper = 0.006324555,            active = FALSE) |>
dplyr::add_row(name = 'rsed',                   initial = 0.0039,               lower = 0.00195,                upper = 0.0078,                 active = FALSE) |>
dplyr::add_row(name = 'Db_0',                   initial = 1e-05,                lower = 5e-06,                  upper = 2e-05,                  active = TRUE) |>
dplyr::add_row(name = 'Db_inf',                 initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'Db_L',                   initial = 0.1,                  lower = 0.05,                   upper = 0.2,                    active = FALSE) |>
dplyr::add_row(name = 'Db_xatt',                initial = 0.08,                 lower = 0.04,                   upper = 0.16,                   active = FALSE) |>
dplyr::add_row(name = 'O2_up',                  initial = 0.2,                  lower = 0.1,                    upper = 0.4,                    active = FALSE) |>
dplyr::add_row(name = 'CH4_up',                 initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'N2_up',                  initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'Mn2_up',                 initial = 0.26,                 lower = 0.13,                   upper = 0.52,                   active = FALSE) |>
dplyr::add_row(name = 'Fe2_up',                 initial = 5e-04,                lower = 0.00025,                upper = 0.001,                  active = FALSE) |>
dplyr::add_row(name = 'Fe2_down',               initial = 0.7,                  lower = 0.35,                   upper = 1.4,                    active = FALSE) |>
dplyr::add_row(name = 'Ca2_up',                 initial = 2.65,                 lower = 1.325,                  upper = 5.3,                    active = FALSE) |>
dplyr::add_row(name = 'OMa_up',                 initial = 0.7,                  lower = 0.35,                   upper = 1.4,                    active = TRUE) |>
dplyr::add_row(name = 'OMb_up',                 initial = 1.4,                  lower = 0.7,                    upper = 2.8,                    active = TRUE) |>
dplyr::add_row(name = 'MnO2_up',                initial = 0.05,                 lower = 0.025,                  upper = 0.1,                    active = FALSE) |>
dplyr::add_row(name = 'FeOH3_up',               initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'FeOH3_P_up',             initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'FeCO3_up',               initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'S0_up',                  initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'FeS_up',                 initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'vivianite_up',           initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'CaCO3_up',               initial = 1,                    lower = 0.5,                    upper = 2,                      active = FALSE) |>
dplyr::add_row(name = 'apatite_up',             initial = 0,                    lower = 0,                      upper = 0,                      active = FALSE) |>
dplyr::add_row(name = 'MnCO3_up',               initial = 0.1,                  lower = 0.05,                   upper = 0.2,                    active = FALSE) |>
dplyr::add_row(name = 'TOT_H_up',               initial = 4.8,                  lower = 2.4,                    upper = 9.6,                    active = FALSE) |>
dplyr::add_row(name = 'TOT_H2CO3_up',           initial = 5.47,                 lower = 2.735,                  upper = 10.94,                  active = FALSE) |>
dplyr::add_row(name = 'TOT_H2PO4_up',           initial = 0.015,                lower = 0.0075,                 upper = 0.03,                   active = FALSE) |>
dplyr::add_row(name = 'TOT_NO3_up',             initial = 0.05,                 lower = 0.025,                  upper = 0.1,                    active = FALSE) |>
dplyr::add_row(name = 'TOT_NH4_up',             initial = 0.173,                lower = 0.0865,                 upper = 0.346,                  active = FALSE) |>
dplyr::add_row(name = 'TOT_SO4_up',             initial = 0.1,                  lower = 0.05,                   upper = 0.2,                    active = FALSE) |>
dplyr::add_row(name = 'TOT_H2S_up',             initial = 6e-04,                lower = 3e-04,                  upper = 0.0012,                 active = FALSE)


# =============================================================================
# > Fitting Routine
# =============================================================================
active_targets <- c(
    "TOT_NO3"
    ,"TOT_NH4"
    ,"TOT_SO4"
    ,"TOT_H2S"
    ,"TOT_H2PO4"
    #"pH"
    #,"TOT_H"
    #,"Ca2"
    #,"Fe2"
    #,"Mn2"
)

new_fit <- fit_greifensee(fitting_dataframe, active_targets, target_func)
