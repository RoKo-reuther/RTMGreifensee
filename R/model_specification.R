# =============================================================================
# > Parameters
# =============================================================================
set_parameters <- function(...,
    #---------------------------------------------------------------------------
    # environmental
    #---------------------------------------------------------------------------
    salinity = 35,                                                   # psu
    temperature = 20,                                                # dgC
    pressure = 1.013253,                                             # bar

    #---------------------------------------------------------------------------
    # porosity
    #---------------------------------------------------------------------------
    por_0 = 0.9,
    por_inf = 0.7,
    por_xatt = 0.15,

    #---------------------------------------------------------------------------
    # bioturbation
    #---------------------------------------------------------------------------
    Db_0 = 1e-5,
    Db_inf = 0.0,
    Db_L = 0.1,
    Db_xatt = 0.08,

    #---------------------------------------------------------------------------
    # sedimentation rate
    #---------------------------------------------------------------------------
    rsed = 0.39 * 0.01,

    #---------------------------------------------------------------------------
    # reaction constants
    #---------------------------------------------------------------------------
    # OM degradation
    k_OM_deg_a = 0.2,                                                # yr-1
    k_OM_deg_b = 0.022,                                              # yr-1
    f_OM_b = 16/106,
    f_OM_c = 1/106,
    lambda = 0.6,
    K_O2 = 0.02,
    K_NO3 = 0.002,
    K_MnO2 = 40.0,
    K_FeOH3 = 5000,
    K_SO4 = 1.6,
    # H2S oxidation with O2
    k_H2S_ox_by_O2 = 1.6e2,                                          # m3_pw mol-1 yr-1
    # SO4 reduction coupled to AOM
    k_SO4_red_by_CH4 = 50.0,                                         # m3_pw mol-1 yr-1
    # FeOH3 / FeOH3_P reduction coupled to H2S oxidation
    k_FeOH3_red_by_H2S = 100.0,                                      # m3_pw mol-1 yr-1
    # FeOH3(_P) formation
    k_FeOH3_formation = 1.4e8 * 1e-3 * 1/4,                          # m3_pw mol-1 yr-1
    K_PO4 = 10^-5,                                                   # mol m-3_pw
    # Oxidation of Fe2 by MnO2
    k_Fe2_ox_by_MnO2 = 3e3,                                          # mol m-3_sf yr-1
    # Ammonium Oxidation
    k_NH4_ox_by_O2 = 5e3,                                            # mol m-3_sf yr-1
    # Sulphide Oxidation by MnO2
    k_H2S_ox_by_MnO2 = 20,                                           # mol m-3_sf yr-1
    # FeS Oxidation by O2
    k_FeS_ox_by_O2 = 3.2e2,                                          # mol m-3_sf yr-1
    # CH4 Oxidation by O2
    k_CH4_ox_by_O2 = 1e7,                                            # mol m-3_pw yr-1
    # Siderite precipitation / dissolution
    k_precip_FeCO3 = 0.0 * 1.8e2,                                    # mol m-3_sf yr-1
    k_diss_FeCO3 = 0.0 * 2.5e-1,                                     # yr -1
    Ksp_FeCO3 = 10^(-10.4) * 1000^2,                                 # mol2 m-6
    # FeS precipitation / dissolution
    k_precip_FeS = 0.7,                                              # mol m-3_sf yr-1
    k_diss_FeS = 0.01,                                               # yr-1
    Ksp_FeS = 10^(-3.915) * 1000,                                    # mol m-3
    # Vivianite precipitation / dissolution
    k_precip_vivianite = 1.1e-2,                                     # mol m-3_sf yr-1
    k_diss_vivianite = 5.3e-3,                                       # yr-1
    Ksp_vivianite = 1.0e-21,                                         # mol5 m-15
    # CaCO3 precipitation / dissolution
    k_precip_CaCO3 = 1.1e-1,                                         # mol m-3_sf yr-1
    k_diss_CaCO3 = 1.25e-1,                                          # yr-1
    Ksp_CaCO3 = 1.65 * 10^(-8.48) * 1000^2,                          # mol2 m-6
    # Apatite precipitation / dissolution
    k_precip_apatite = 1.4e-6,                                       # mol m-3_sf yr-1
    k_diss_apatite = 3.7e-2,                                         # yr-1
    Ksp_apatite = 6e-20,                                             # mol3 m-9
    # MnCO3 Precipitation / Dissolution
    k_precip_MnCO3 = 270,                                            # mol m-3_sf yr-1
    k_diss_MnCO3 = 0.8,                                              # yr-1
    Ksp_MnCO3 = 10^(-8.5) * 1000^2,                                  # mol2 m-6

    #---------------------------------------------------------------------------
    # Boundary Conditions
    #---------------------------------------------------------------------------
    O2_up        = 0.2,
    CH4_up       = 0.0,
    N2_up        = 0.0,
    Mn2_up       = 0.26,
    Fe2_up       = 5e-4,
    Fe2_down     = 0.7,
    Ca2_up       = 2.65,
    OMa_up       = 0.7,
    OMb_up       = 1.4,
    MnO2_up      = 0.05,
    FeOH3_up     = 0.0,
    FeOH3_P_up   = 0.0,
    FeCO3_up     = 0.0,
    S0_up        = 0.0,
    FeS_up       = 0.0,
    vivianite_up = 0.0,
    CaCO3_up     = 1.0,
    apatite_up   = 0.0,
    MnCO3_up     = 0.1,
    TOT_H_up     = 4.8,
    TOT_H2CO3_up = 5.47,
    TOT_H2PO4_up = 0.015,
    TOT_NO3_up   = 0.05,
    TOT_NH4_up   = 0.173,
    TOT_SO4_up   = 0.1,
    TOT_H2S_up   = 6e-4
) {

    #---------------------------------------------------------------------------
    # Parameters
    #---------------------------------------------------------------------------

    diffcoeff_species <- c(
        "O2", "CH4", "N2", "Mn", "Fe", "Ca", "HCO3", "HCO3", "H2PO4", "NO3", "NH4", "SO4", "H2S"
    )

    diffcoeffs <- marelac::diffcoeff(
      species = diffcoeff_species,
      S = salinity,
      t = temperature,
      P = pressure
    )

    s2yr <- 3600 * 24 * 365.25

    parms <- c(
        precipitation  = 0.0,
        speciation     = 0.0,
        por_0          = por_0,
        por_inf        = por_inf,
        por_xatt       = por_xatt,
        Dmol_O2        = diffcoeffs[[1]]  * s2yr,
        Dmol_CH4       = diffcoeffs[[2]]  * s2yr,
        Dmol_N2        = diffcoeffs[[3]]  * s2yr,
        Dmol_Mn2       = diffcoeffs[[4]]  * s2yr,
        Dmol_Fe2       = diffcoeffs[[5]]  * s2yr,
        Dmol_Ca2       = diffcoeffs[[6]]  * s2yr,
        Dmol_TOT_H     = diffcoeffs[[7]]  * s2yr,
        Dmol_TOT_H2CO3 = diffcoeffs[[8]]  * s2yr,
        Dmol_TOT_H2PO4 = diffcoeffs[[9]]  * s2yr,
        Dmol_TOT_NO3   = diffcoeffs[[10]] * s2yr,
        Dmol_TOT_NH4   = diffcoeffs[[11]] * s2yr,
        Dmol_TOT_SO4   = diffcoeffs[[12]] * s2yr,
        Dmol_TOT_H2S   = diffcoeffs[[13]] * s2yr,
        # OM degradation
        k_OM_deg_a = k_OM_deg_a,
        k_OM_deg_b = k_OM_deg_b,
        f_OM_b = f_OM_b,
        f_OM_c = f_OM_c,
        lambda = lambda,
        K_O2 = K_O2,
        K_NO3 = K_NO3,
        K_MnO2 = K_MnO2,
        K_FeOH3 = K_FeOH3,
        K_SO4 = K_SO4,
        # H2S oxidation with O2
        k_H2S_ox_by_O2 = k_H2S_ox_by_O2,
        # SO4 reduction coupled to AOM
        k_SO4_red_by_CH4 = k_SO4_red_by_CH4,
        # FeOH3 / FeOH3_P reduction coupled to H2S oxidation
        k_FeOH3_red_by_H2S = k_FeOH3_red_by_H2S,
        # FeOH3(_P) formation
        k_FeOH3_formation = k_FeOH3_formation,
        K_PO4 = K_PO4,
        # Oxidation of Fe2 by MnO2
        k_Fe2_ox_by_MnO2 = k_Fe2_ox_by_MnO2,
        # Ammonium Oxidation
        k_NH4_ox_by_O2 = k_NH4_ox_by_O2,
        # Sulphide Oxidation by MnO2
        k_H2S_ox_by_MnO2 = k_H2S_ox_by_MnO2,
        # FeS Oxidation by O2
        k_FeS_ox_by_O2 = k_FeS_ox_by_O2,
        # CH4 Oxidation by O2
        k_CH4_ox_by_O2 = k_CH4_ox_by_O2,
        # Siderite precipitation / dissolution
        k_precip_FeCO3 = k_precip_FeCO3,
        k_diss_FeCO3 = k_diss_FeCO3,
        Ksp_FeCO3 = Ksp_FeCO3,
        # FeS precipitation / dissolution
        k_precip_FeS = k_precip_FeS,
        k_diss_FeS = k_diss_FeS,
        Ksp_FeS = Ksp_FeS,
        # Vivianite precipitation / dissolution
        k_precip_vivianite = k_precip_vivianite,
        k_diss_vivianite = k_diss_vivianite,
        Ksp_vivianite = Ksp_vivianite,
        # CaCO3 precipitation / dissolution
        k_precip_CaCO3 = k_precip_CaCO3,
        k_diss_CaCO3 = k_diss_CaCO3,
        Ksp_CaCO3 = Ksp_CaCO3,
        # Apatite precipitation / dissolution
        k_precip_apatite = k_precip_apatite,
        k_diss_apatite = k_diss_apatite,
        Ksp_apatite = Ksp_apatite,
        # MnCO3 Precipitation / Dissolution
        k_precip_MnCO3 = k_precip_MnCO3,
        k_diss_MnCO3 = k_diss_MnCO3,
        Ksp_MnCO3 = Ksp_MnCO3
    )

    #---------------------------------------------------------------------------
    # Forcings
    #---------------------------------------------------------------------------
    forcing_data <- list(
        rsed         = matrix(c(0.0, rsed), ncol = 2),
        Db_0         = matrix(c(0.0, Db_0), ncol = 2),
        Db_inf       = matrix(c(0.0, Db_inf), ncol = 2),
        Db_L         = matrix(c(0.0, Db_L), ncol = 2),
        Db_xatt      = matrix(c(0.0, Db_xatt), ncol = 2),
        O2_up        = matrix(c(0.0, O2_up), ncol = 2),
        CH4_up       = matrix(c(0.0, CH4_up), ncol = 2),
        N2_up        = matrix(c(0.0, N2_up), ncol = 2),
        Mn2_up       = matrix(c(0.0, Mn2_up), ncol = 2),
        Fe2_up       = matrix(c(0.0, Fe2_up), ncol = 2),
        Fe2_down     = matrix(c(0.0, Fe2_down), ncol = 2),
        Ca2_up       = matrix(c(0.0, Ca2_up), ncol = 2),
        OMa_up       = matrix(c(0.0, OMa_up), ncol = 2),
        OMb_up       = matrix(c(0.0, OMb_up), ncol = 2),
        MnO2_up      = matrix(c(0.0, MnO2_up), ncol = 2),
        FeOH3_up     = matrix(c(0.0, FeOH3_up), ncol = 2),
        FeOH3_P_up   = matrix(c(0.0, FeOH3_P_up), ncol = 2),
        FeCO3_up     = matrix(c(0.0, FeCO3_up), ncol = 2),
        S0_up        = matrix(c(0.0, S0_up), ncol = 2),
        FeS_up       = matrix(c(0.0, FeS_up), ncol = 2),
        vivianite_up = matrix(c(0.0, vivianite_up), ncol = 2),
        CaCO3_up     = matrix(c(0.0, CaCO3_up), ncol = 2),
        apatite_up   = matrix(c(0.0, apatite_up), ncol = 2),
        MnCO3_up     = matrix(c(0.0, MnCO3_up), ncol = 2),
        TOT_H_up     = matrix(c(0.0, TOT_H_up), ncol = 2),
        TOT_H2CO3_up = matrix(c(0.0, TOT_H2CO3_up), ncol = 2),
        TOT_H2PO4_up = matrix(c(0.0, TOT_H2PO4_up), ncol = 2),
        TOT_NO3_up   = matrix(c(0.0, TOT_NO3_up), ncol = 2),
        TOT_NH4_up   = matrix(c(0.0, TOT_NH4_up), ncol = 2),
        TOT_SO4_up   = matrix(c(0.0, TOT_SO4_up), ncol = 2),
        TOT_H2S_up   = matrix(c(0.0, TOT_H2S_up), ncol = 2)
    )

    get_forcing_values_ss <- function(forcings, t) {
        values <- c()
        for (i in forcings) {
            #value <- approx(i, xout = t)$y
            value  <- i[2]
            values <- c(values, value)
        }
        names(values) <- names(forcings)
        return(values)
    }

    forcing_values_ss <- get_forcing_values_ss(forcings = forcing_data, t = 0.0)


    return(list(parms = parms, forcings = forcing_values_ss))

}


# =============================================================================
# > Grouped List of Model Metadata
# =============================================================================
#' Grouped List of Model Metadata
#'
#' A list containing model metadata.
#' The entries are as follows:
#'
#' @format A list containing:
#' \describe{
#'   \item{N_grid}{integer: number of grid layers in which the model is divided}
#'   \item{species}{a vector of the model-species names in the order they are taken and returned by the model}
#'   \item{tableau_species}{a vector of the species names in the order they are returned by the tableau solving routine}
#'   \item{reactions}{a vector of the reaction rate names in the order they are returned by the model function}
#'   \item{omegas}{a vector of saturation indices in the order they are returned by the model function}
#'   \item{environmental_parms}{a vector of names of environmental parameters or forcings; used for seperation in UI}
#'   \item{boundary_conditions}{a vector of names of boundary conditions; used for seperation in UI}
#'   \item{reaction_parms}{a vector of names of reaction parameters; used for seperation in UI}
#'   \item{environmental_parms}{a vector of names of boundary conditions; used for seperation in UI}
#' }
#' @export
model_metadata <- list(

    N_grid = 250,

    species = c(
        # Solutes
        "O2",
        "CH4",
        "N2",
        "Mn2",
        "Fe2",
        "Ca2",
        # Solids
        "OMa",
        "OMb",
        "MnO2",
        "FeOH3",
        "FeOH3_P",
        "FeCO3",
        "S0",
        "FeS",
        "vivianite",
        "CaCO3",
        "apatite",
        "MnCO3",
        # Lump Sums
        "TOT_H",
        "TOT_H2CO3",
        "TOT_H2PO4",
        "TOT_NO3",
        "TOT_NH4",
        "TOT_SO4",
        "TOT_H2S"
    ),

    tableau_species = c(
        "H",
        "OH",   
        "H2CO3",
        "HCO3",
        "CO3",
        "H3PO4",
        "H2PO4",
        "HPO4",
        "PO4",
        "HNO3",
        "NO3",
        "NH4",
        "NH3",
        "H2SO4",
        "HSO4",
        "SO4",
        "H2S",
        "HS",
        "S2" 
    ),

    reactions = c(
        "R_OM_deg_a_O2",
        "R_OM_deg_b_O2",
        "R_OM_deg_a_NO3",
        "R_OM_deg_b_NO3",
        "R_OM_deg_a_MnO2",
        "R_OM_deg_b_MnO2",
        "R_OM_deg_a_FeOH3",
        "R_OM_deg_b_FeOH3",
        "R_OM_deg_a_SO4",
        "R_OM_deg_b_SO4",
        "R_OM_deg_a_CH4",
        "R_OM_deg_b_CH4",
        "R_H2S_ox_by_O2",
        "R_SO4_red_by_CH4",
        "R_FeOH3_red_by_H2S",
        "R_FeOH3_formation",
        "R_Fe2_ox_by_MnO2",
        "R_NH4_ox_by_O2",
        "R_H2S_ox_by_MnO2",
        "R_FeS_ox_by_O2",
        "R_CH4_ox_by_O2",
        "R_precip_FeCO3",
        "R_diss_FeCO3",
        "R_precip_FeS",
        "R_diss_FeS",
        "R_precip_vivianite",
        "R_diss_vivianite",
        "R_precip_CaCO3",
        "R_diss_CaCO3",
        "R_precip_apatite",
        "R_diss_apatite",
        "R_precip_MnCO3",
        "R_diss_MnCO3"
    ),

    omegas = c(
        "omega_FeCO3",
        "omega_CaCO3",
        "omega_FeS",
        "omega_vivianite",
        "omega_apatite",
        "omega_MnCO3"
    ),

    environmental_parms = c(
        "por_0",
        "por_inf",
        "por_xatt",
        "Db_0", 
        "Db_inf",
        "Db_L",
        "Db_xatt",
        "rsed"
    ),

    boundary_conditions = c(
        "O2_up",
        "CH4_up",
        "N2_up",
        "Mn2_up",
        "Fe2_up",
        "Fe2_down",
        "Ca2_up",
        "OMa_up",
        "OMb_up",
        "MnO2_up",
        "FeOH3_up",
        "FeOH3_P_up",
        "FeCO3_up",
        "S0_up",
        "FeS_up",
        "vivianite_up",
        "CaCO3_up",
        "apatite_up",
        "MnCO3_up",
        "TOT_H_up",
        "TOT_H2CO3_up",
        "TOT_H2PO4_up",
        "TOT_NO3_up",
        "TOT_NH4_up",
        "TOT_SO4_up",
        "TOT_H2S_up"
    ),

    reaction_parms = c(
        "k_OM_deg_a",
        "k_OM_deg_b",
        "f_OM_b",
        "f_OM_c",
        "lambda",
        "K_O2",
        "K_NO3",
        "K_MnO2",
        "K_FeOH3",
        "K_SO4",
        "k_H2S_ox_by_O2",
        "k_SO4_red_by_CH4",
        "k_FeOH3_red_by_H2S",
        "k_FeOH3_formation",
        "K_PO4",
        "k_Fe2_ox_by_MnO2",
        "k_NH4_ox_by_O2",
        "k_H2S_ox_by_MnO2",
        "k_FeS_ox_by_O2",
        "k_CH4_ox_by_O2",
        "k_precip_FeCO3",
        "k_diss_FeCO3",
        "Ksp_FeCO3",
        "k_precip_FeS",
        "k_diss_FeS",
        "Ksp_FeS",
        "k_precip_vivianite",
        "k_diss_vivianite",
        "Ksp_vivianite",
        "k_precip_CaCO3",
        "k_diss_CaCO3",
        "Ksp_CaCO3",
        "k_precip_apatite",
        "k_diss_apatite",
        "Ksp_apatite",
        "k_precip_MnCO3",
        "k_diss_MnCO3",
        "Ksp_MnCO3"
    )
)


# =============================================================================
# > Get the Elemental Composition of Model Species
# =============================================================================
get_elemental_composition <- function(model_parameters) {

    f_OM_a <- 1
    f_OM_b <- model_parameters$parms[["f_OM_b"]]
    f_OM_c <- model_parameters$parms[["f_OM_c"]]
    lambda <- model_parameters$parms[["lambda"]]

    C <- data.frame(species = character(), stoic = double()) |>
        dplyr::add_row(species = "TOT_H2CO3", stoic = 1) |>
        dplyr::add_row(species = "CH4", stoic = 1) |>
        dplyr::add_row(species = "OMa", stoic = f_OM_a) |>
        dplyr::add_row(species = "OMb", stoic = f_OM_a) |>
        dplyr::add_row(species = "FeCO3", stoic = 1) |>
        dplyr::add_row(species = "CaCO3", stoic = 1) |>
        dplyr::add_row(species = "MnCO3", stoic = 1)

    N <- data.frame(species = character(), stoic = double()) |>
        dplyr::add_row(species = "TOT_NO3", stoic = 1) |>
        dplyr::add_row(species = "TOT_NH4", stoic = 1) |>
        dplyr::add_row(species = "N2", stoic = 2) |>
        dplyr::add_row(species = "OMa", stoic = f_OM_b) |>
        dplyr::add_row(species = "OMb", stoic = f_OM_b)

    P <- data.frame(species = character(), stoic = double()) |>
        dplyr::add_row(species = "TOT_H2PO4", stoic = 1) |>
        dplyr::add_row(species = "OMa", stoic = f_OM_c) |>
        dplyr::add_row(species = "OMb", stoic = f_OM_c) |>
        dplyr::add_row(species = "FeOH3_P", stoic = lambda) |>
        dplyr::add_row(species = "vivianite", stoic = 2) |>
        dplyr::add_row(species = "apatite", stoic = 2)

    S <- data.frame(species = character(), stoic = double()) |>
        dplyr::add_row(species = "TOT_SO4", stoic = 1) |>
        dplyr::add_row(species = "TOT_H2S", stoic = 1) |>
        dplyr::add_row(species = "S0", stoic = 1) |>
        dplyr::add_row(species = "FeS", stoic = 1)

    Fe <- data.frame(species = character(), stoic = double()) |>
        dplyr::add_row(species = "Fe2", stoic = 1) |>
        dplyr::add_row(species = "FeOH3", stoic = 1) |>
        dplyr::add_row(species = "FeOH3_P", stoic = 1) |>
        dplyr::add_row(species = "FeCO3", stoic = 1) |>
        dplyr::add_row(species = "FeS", stoic = 1) |>
        dplyr::add_row(species = "vivianite", stoic = 3)

    Mn <- data.frame(species = character(), stoic = double()) |>
        dplyr::add_row(species = "Mn2", stoic = 1) |>
        dplyr::add_row(species = "MnO2", stoic = 1) |>
        dplyr::add_row(species = "MnCO3", stoic = 1)

    Ca <- data.frame(species = character(), stoic = double()) |>
        dplyr::add_row(species = "Ca2", stoic = 1) |>
        dplyr::add_row(species = "CaCO3", stoic = 1) |>
        dplyr::add_row(species = "apatite", stoic = 3)

    elemental_composition <- list(
        C = C,
        N = N,
        P = P,
        S = S,
        Fe = Fe,
        Mn = Mn,
        Ca = Ca
    )

    return(elemental_composition)

}
