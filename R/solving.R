# ==============================================================================
# > Solve Steady State
# ==============================================================================
solve_steady <- function(model_parameters, model_metadata = RTMGreifensee::model_metadata, tag = "std", initial = NULL) {

    N_grid          <- model_metadata$N_grid
    species         <- model_metadata$species
    tableau_species <- model_metadata$tableau_species
    reactions       <- model_metadata$reactions
    omegas          <- model_metadata$omegas

    parms           <- model_parameters$parms
    forcings        <- model_parameters$forcings

    outnames <- c(
        # Grid properties
        rep("xmid", N_grid),
        rep("xint", N_grid + 1),
        rep("por", N_grid),
        rep("svf", N_grid),
        rep("Db", N_grid + 1),
        # Speciation
        rep(tableau_species, each = N_grid),
        # pH
        rep("pH", N_grid),
        # success of tableau solving
        rep("success", N_grid),
        # Transport dC
        rep(paste0("tran_", species), each = N_grid),
        # Transport fluxes at boundaries
        paste0("flux_up_", species),
        paste0("flux_down_", species),
        # sumR
        rep(paste0("reac_", species), each = N_grid),
        # reaction rates
        rep(reactions, each = N_grid),
        # omegas
        rep(omegas, each = N_grid)
    )

    nout <- length(outnames)

    std <- NULL

    if (!is.null(initial)) {

        cat("Try from reference ...\n")

        # switch precipitation and speciation on
        parms["precipitation"] <- 1.0
        parms["speciation"]    <- 1.0

        std <- rootSolve::steady.1D(
            y        = as.vector(initial),
            time     = 0.0,
            parms    = parms,
            names    = species,
            method   = "stode",
            dimens   = N_grid,
            nout     = nout,
            outnames = outnames,
            dllname  = "RTMGreifensee",
            func     = "derivs",
            initfunc = "initmod",
            initforc = "forcing_functions",
            forcings = forcings,
            positive = TRUE
        )

    }
    
    if (is.null(std) || !attributes(std)$steady) {

        cat("Try from start ...\n")

        initial <- rep(1e-4, N_grid * length(species))

        # switch precipitation and speciation off
        parms["precipitation"] <- 0.0
        parms["speciation"]    <- 0.0

        std <- rootSolve::steady.1D(
            y        = initial,
            time     = 0.0,
            parms    = parms,
            names    = species,
            method   = "stodes",
            dimens   = N_grid,
            nout     = nout,
            outnames = outnames,
            dllname  = "RTMGreifensee",
            func     = "derivs",
            initfunc = "initmod",
            initforc = "forcing_functions",
            forcings = forcings,
            positive = TRUE
        )

        # switch precipitation on
        parms["precipitation"] <- 1.0

        std <- rootSolve::steady.1D(
            y        = as.vector(std$y),
            time     = 0.0,
            parms    = parms,
            names    = species,
            method   = "stode",
            dimens   = N_grid,
            nout     = nout,
            outnames = outnames,
            dllname  = "RTMGreifensee",
            func     = "derivs",
            initfunc = "initmod",
            initforc = "forcing_functions",
            forcings = forcings,
            positive = TRUE
        )

        
        # switch speciation on
        parms["speciation"] <- 1.0

        std <- rootSolve::steady.1D(
            y        = as.vector(std$y),
            time     = 0.0,
            parms    = parms,
            names    = species,
            method   = "stode",
            dimens   = N_grid,
            nout     = nout,
            outnames = outnames,
            dllname  = "RTMGreifensee",
            func     = "derivs",
            initfunc = "initmod",
            initforc = "forcing_functions",
            forcings = forcings,
            positive = TRUE
        )

    }

    if (attributes(std)$steady) {

        cat("Done!\n")

        dfs <- get_std_dataframes(std, tag)

        mass_balance_species_based <- massbalance_species_based(std, species, tag)
        mass_balance_element_based <- massbalance_element_based(std, mass_balance_species_based, model_parameters, tag)

        integrated_reaction_rates  <- get_depth_integrated_reaction_rates(std, reactions, tag)

        # compose return list
        return(list(
            tag = tag,
            profiles = dfs$profiles,
            one_value_results = dfs$one_value_results,
            original = std,
            parameters = c(parms, forcings),
            mass_balances = list(species_based = mass_balance_species_based, element_based = mass_balance_element_based),
            integrated_reaction_rates = integrated_reaction_rates
        ))

    } else {

        cat("Failed ... returning NULL\n")

        return(NULL)

    }
}
