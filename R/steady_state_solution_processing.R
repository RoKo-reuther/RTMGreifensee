# ==============================================================================
# > Create Dataframes from Steady State Solution Object
# ==============================================================================
std_profile <- function(std_collection, entity) {

    value <- std_collection[[entity]]

    if (length(value) == length(std_collection[["xmid"]])) {
        depth <- std_collection[["xmid"]]
    } else {
        depth <- std_collection[["xint"]]
    }

    return(data.frame(depth = depth, value = value))
    
}

get_std_dataframes <- function(std, tag) {

    # get species concentrations as list of vectors
    species_concentration_list <- as.list(as.data.frame(std$y))
    # ... to expand result with species concentrations
    collection <- c(std, species_concentration_list)

    # remove "y", "xmid" and "xint" from collection
    collection_for_names      <- collection
    collection_for_names$y    <- NULL
    collection_for_names$xmid <- NULL
    collection_for_names$xint <- NULL

    # from the collection create a long table (depth, value, name) for profiles
    profiles <- data.frame(depth = NULL, value = NULL, name = NULL, tag = NULL)
    # ... and a table (value, name) for one-value results (so far only fluxes)
    one_value_results <- data.frame(value = NULL, name = NULL, tag = NULL)

    for (entity in names(collection_for_names)) {

        if (length(collection[[entity]]) > 1) {
            value_depth <- std_profile(collection, entity)
            
            new_data <- data.frame(
                depth = value_depth$depth,
                value = value_depth$value,
                name  = entity,
                tag   = tag
            )

            profiles <- rbind(profiles, new_data)

        } else {
            new_data <- data.frame(
                value = collection[[entity]],
                name  = entity,
                tag   = tag
            )

            one_value_results <- rbind(one_value_results, new_data)
        }

    }

    return(list(
        profiles = profiles,
        one_value_results = one_value_results
    ))

}


# =============================================================================
# > Species based Mass Balances (from original solution)
# =============================================================================
massbalance_species_based_single_species <- function(std, species) {

    flux_up    <- std[[paste0("flux_up_", species)]]
    flux_down  <- std[[paste0("flux_down_", species)]]
    netto_flux <- flux_up - flux_down

    reac_species <- std[[paste0("reac_", species)]]
    sumR_integrated <- sum(reac_species * diff(std$xint))

    bilanz <- netto_flux + sumR_integrated

    return(data.frame(
      species = species,
      flux_up = flux_up,
      flux_down = flux_down,
      netto_flux = netto_flux,
      sumR_integrated = sumR_integrated,
      bilanz = bilanz
    ))
}

massbalance_species_based <- function(std, species_vector, tag) {
  
    species_based_mass_balance <- data.frame(
        species = NULL,
        flux_up = NULL,
        flux_down = NULL,
        netto_flux = NULL,
        sumR_integrated = NULL,
        bilanz = NULL
    )
    
    for (species in species_vector) {
      species_based_mass_balance <- rbind(
        species_based_mass_balance,
        massbalance_species_based_single_species(std, species)
      )
    }

    species_based_mass_balance$tag <- tag
    
    return(species_based_mass_balance)
}


# =============================================================================
# > Element based Mass Balances (from original solution)
# =============================================================================
massbalance_element_based <- function(std, mass_balance_species_based, model_parameters, tag) {
  
    elemental_composition <- get_elemental_composition(model_parameters)

    elements <- names(elemental_composition)

    overview <- data.frame(
        element = character(),
        netto_flux = double(),
        sumR_integrated = double()
    )

    detailed <- list()

    for (element in elements) {

        composition <- elemental_composition[[element]]

        detailed_for_an_element <- data.frame(
            species = character(),
            flux_up = double(),
            flux_down = double(),
            sumR_integrated = double()
        )

        # for every species in which an element is present ...
        for (i in seq_len(nrow(composition))) {

            current_species <- composition$species[i]
            stoic <- composition$stoic[i]

            sumR_integrated <- subset(mass_balance_species_based, species == current_species)$sumR_integrated * stoic
            flux_up         <- subset(mass_balance_species_based, species == current_species)$flux_up * stoic
            flux_down       <- subset(mass_balance_species_based, species == current_species)$flux_down * stoic

            detailed_for_an_element <- rbind(
                detailed_for_an_element,
                data.frame(
                    species = current_species,
                    flux_up = flux_up,
                    flux_down = flux_down,
                    sumR_integrated = sumR_integrated
                )
          )
        }

        detailed_for_an_element$tag <- tag

        detailed[[element]] <- detailed_for_an_element

        net_flux <- sum(detailed_for_an_element[["flux_up"]]) - sum(detailed_for_an_element[["flux_down"]])
        net_rate <- sum(detailed_for_an_element[["sumR_integrated"]])

        overview <- rbind(
            overview,
            data.frame(
              element = element,
              netto_flux = net_flux,
              sumR_integrated = net_rate
            )
        )

    }

    overview$tag <- tag
  
    return(list(
        overview = overview,
        detailed = detailed
    ))
    
}
