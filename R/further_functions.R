# =============================================================================
# > Combine std-profile dataframes + further data
# =============================================================================
get_collective_profiles_dataframe <- function(std_list, further_data = NULL) {

    # "std_list" is a list of steady state results returned by "solve_steady"
    # "further_data" here is Greifensee measurements (a data frame with columns: depth, value, name, tag)

    data <- data.frame()

    # combine steady-state dataframes
    for (i in seq_along(std_list)) {

        data <- rbind(data, std_list[[i]][["profiles"]])

    }

    # add "further_data"
    data <- rbind(data, further_data)

    return(data)

}


# =============================================================================
# > Combine mass-balance dataframes
# =============================================================================
get_collective_species_based_massbalance <- function(std_list) {

    data <- data.frame()

    # combine massbalance dataframes
    for (i in seq_along(std_list)) {

        data <- rbind(data, std_list[[i]][["mass_balances"]][["species_based"]])

    }

    data$species <- as.factor(data$species)
    data$tag     <- as.factor(data$tag)

    return(data)

}

get_collective_element_based_overview_massbalance <- function(std_list) {

    data <- data.frame()

    # combine massbalance dataframes
    for (i in seq_along(std_list)) {

        data <- rbind(data, std_list[[i]][["mass_balances"]][["element_based"]][["overview"]])

    }

    data$element <- as.factor(data$element)
    data$tag     <- as.factor(data$tag)

    return(data)

}

get_collective_element_based_detailed_massbalance <- function(std_list) {

    data <- list()

    elements <- names(std_list[[1]][["mass_balances"]][["element_based"]][["detailed"]])

    get_element_dataframe <- function(std, element) {

        data <- std[["mass_balances"]][["element_based"]][["detailed"]][[element]]

        data$species <- as.factor(data$species)
        data$tag     <- as.factor(data$tag)

        return(data)
    
    }

    for (element in elements) {

        data[[element]] <- do.call("rbind", lapply(std_list, get_element_dataframe, element = element))

    }

    return(data)

}


# =============================================================================
# > Combine integrated_reaction_rates data-frames
# =============================================================================
get_collective_integrated_reaction_rates <- function(std_list) {

    data <- data.frame()

    # combine dataframes
    for (i in seq_along(std_list)) {

        data <- rbind(data, std_list[[i]][["integrated_reaction_rates"]])

    }

    data$name <- as.factor(data$name)
    data$tag  <- as.factor(data$tag)

    return(data)

}


# =============================================================================
# > Change tag
# =============================================================================
change_tag <- function(std_or_sublist, new_tag) {

    for (i in seq_along(std_or_sublist)) {

        if (names(std_or_sublist)[i] == "tag") {

            # change tag entry
            std_or_sublist[[i]] <- new_tag

        } else if (is.data.frame(std_or_sublist[[i]])) {

            # change tag column in data.frames
            std_or_sublist[[i]][["tag"]] <- new_tag

        } else if (is.list(std_or_sublist[[i]])) {

            # for further nedsted lists call function recursively
            std_or_sublist[[i]] <- change_tag(std_or_sublist[[i]], new_tag)

        }

    }

    return(std_or_sublist)

}
