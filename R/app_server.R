# =============================================================================
# > RTMGreifenseeApp-Server
# =============================================================================
app_server <- function(input, output, session) {

    #--------------------------------------------------------------------------
    # Initialize
    #--------------------------------------------------------------------------
    rVs <- shiny::reactiveValues()

    rVs$std_list <- list(ref = RTMGreifensee::reference_state)

    rVs$collective_dataframe <- get_collective_profiles_dataframe(list(ref = RTMGreifensee::reference_state), RTMGreifensee::greifensee_data)

    rVs$species_based_mass_balance          <- get_collective_species_based_massbalance(shiny::isolate(rVs$std_list))
    rVs$element_based_mass_balance_overview <- get_collective_element_based_overview_massbalance(shiny::isolate(rVs$std_list))
    rVs$element_based_mass_balance_detailed <- get_collective_element_based_detailed_massbalance(shiny::isolate(rVs$std_list))
    rVs$integrated_reaction_rates           <- get_collective_integrated_reaction_rates(shiny::isolate(rVs$std_list))

        #--------------------------------------------------------------------------
    # Helpers
    #--------------------------------------------------------------------------
    # create an parameter input-element
    create_parameter_input <- function(parameter) {
        numericInput(parameter, label = parameter, value = RTMGreifensee::reference_state$parameters[parameter])
    }


    #--------------------------------------------------------------------------
    # Parameter Panel Setup
    #--------------------------------------------------------------------------
    output$reaction_parms <- shiny::renderUI({

        output_list <- lapply(RTMGreifensee::model_metadata$reaction_parms, create_parameter_input)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        shiny::tagList(output_list)

    })

    output$boundary_conditions <- shiny::renderUI({

        output_list <- lapply(RTMGreifensee::model_metadata$boundary_conditions, create_parameter_input)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        shiny::tagList(output_list)

    })

    output$environmental_parms <- shiny::renderUI({

        output_list <- lapply(RTMGreifensee::model_metadata$environmental_parms, create_parameter_input)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        shiny::tagList(output_list)

    })


    #--------------------------------------------------------------------------
    # Steady-State Calculation
    #--------------------------------------------------------------------------
    shiny::observe({

        shiny::withProgress(message = "Calculating new scenario ...", {

        parms_list <- list()

        for (parameter in c(model_metadata$environmental_parms, model_metadata$reaction_parms, model_metadata$boundary_conditions)) {
            parms_list[[parameter]] <- input[[parameter]]
        }

        model_parameters <- do.call("set_parameters", parms_list)

        shiny::setProgress(0.1)
    
        std <- solve_steady(model_parameters, tag = paste0("@", input$new_tag), initial = RTMGreifensee::reference_state$original$y)

        if (!is.null(std)) {

            shiny::setProgress(0.8)
    
            rVs$std_list[[paste0("@", input$new_tag)]] <- std

            shiny::updateSelectInput(session, "active_tags", choices = names(rVs$std_list), selected = input$active_tags)

            shiny::setProgress(1.0)
            
        } else {

            shiny::setProgress(1.0)
            shiny::showNotification("Solving failed for this parameter-set.", type = "error")

        }

        })
    
    }) |> shiny::bindEvent(input$goButton)


    #--------------------------------------------------------------------------
    # Active Scenario Selection
    #--------------------------------------------------------------------------
    shiny::observe({

        rVs$collective_dataframe <- get_collective_profiles_dataframe(rVs$std_list[input$active_tags], RTMGreifensee::greifensee_data)
        rVs$species_based_mass_balance <- get_collective_species_based_massbalance(rVs$std_list[input$active_tags])
        rVs$element_based_mass_balance_overview <- get_collective_element_based_overview_massbalance(rVs$std_list[input$active_tags])
        rVs$element_based_mass_balance_detailed <- get_collective_element_based_detailed_massbalance(rVs$std_list[input$active_tags])
        rVs$integrated_reaction_rates           <- get_collective_integrated_reaction_rates(rVs$std_list)

    }) |> shiny::bindEvent(input$selectScenariosButton)


    #--------------------------------------------------------------------------
    # Download list of steady state scenarios
    #--------------------------------------------------------------------------
    output$std_list_download <- shiny::downloadHandler(
        filename = "std_list.rds",
        content = function(file) {
          saveRDS(rVs$std_list, file)
        }
      )


    #--------------------------------------------------------------------------
    # Upload list of steady state scenarios
    #--------------------------------------------------------------------------
    shiny::observe({
        rVs$std_list <- readRDS(input$std_list_input$datapath)
        shiny::updateSelectInput(session, "active_tags", choices = names(rVs$std_list), selected = input$active_tags)
    }) |> shiny::bindEvent(input$std_list_input)


    #--------------------------------------------------------------------------
    # Plots: Species Profiles
    #--------------------------------------------------------------------------
    # render species profiles
    # remove "TOT_H" .. name it alkalinity
    for (species in model_metadata$species[! model_metadata$species %in% c("TOT_H")]) { local({

        local_species <- species

        output[[paste0("profile_", local_species)]] <- echarts4r::renderEcharts4r(
            plot_std_profile_echart(
                rVs$collective_dataframe,
                local_species,
                xlab = "concentration (mol m-3_phase)",
                ylab = "depth (m)",
                main = local_species
            )
        )

        output[["profile_TOT_H"]] <- echarts4r::renderEcharts4r(
            plot_std_profile_echart(
                rVs$collective_dataframe,
                "TOT_H",
                xlab = "concentration (mol m-3_phase)",
                ylab = "depth (m)",
                main = "alkalinity"
            )
        )

    })}


    #--------------------------------------------------------------------------
    # Plots: Speciation Profiles
    #--------------------------------------------------------------------------
    # render species profiles
    # remove "H" .. plot pH instead
    for (species in model_metadata$tableau_species[! model_metadata$tableau_species %in% c("H")]) { local({

        local_species <- species

        output[[paste0("profile_", local_species)]] <- echarts4r::renderEcharts4r(
            plot_std_profile_echart(
                rVs$collective_dataframe,
                local_species,
                xlab = "concentration (mol m-3_phase)",
                ylab = "depth (m)",
                main = local_species
            )
        )

        # pH plot
        output[["profile_H"]] <- echarts4r::renderEcharts4r(
            plot_std_profile_echart(
                rVs$collective_dataframe,
                "pH",
                xlab = "pH",
                ylab = "depth (m)",
                main = "pH"
            )
        )

    })}


    #--------------------------------------------------------------------------
    # Plots: Reaction Rate Profiles
    #--------------------------------------------------------------------------
    # render profiles
    for (element in model_metadata$reactions) { local({

        local_element <- element

        output[[paste0("profile_", local_element)]] <- echarts4r::renderEcharts4r(
            plot_std_profile_echart(
                rVs$collective_dataframe,
                local_element,
                xlab = "reaction rate (mol m-3_phase yr-1)",
                ylab = "depth (m)",
                main = local_element
            )
        )

    })}


    #--------------------------------------------------------------------------
    # Plots: Saturation Profiles
    #--------------------------------------------------------------------------
    # render profiles
    for (element in model_metadata$omegas) { local({

        local_element <- element

        output[[paste0("profile_", local_element)]] <- echarts4r::renderEcharts4r(
            plot_std_profile_echart(
                rVs$collective_dataframe,
                local_element,
                xlab = "reaction rate (mol m-3_phase yr-1)",
                ylab = "depth (m)",
                main = local_element
            )
        )

    })}


    #--------------------------------------------------------------------------
    # Mass Balances > Species based
    #--------------------------------------------------------------------------
    output$tbl_species_based_mass_balance <- DT::renderDT(
        DT::datatable(
            rVs$species_based_mass_balance,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$species_based_mass_balance), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "netto_flux", "sumR_integrated", "bilanz"))
    )

    #--------------------------------------------------------------------------
    # Mass Balances > Element based overview
    #--------------------------------------------------------------------------
    output$tbl_element_based_mass_balance_overview <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_overview,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_overview), dom = "t")
        ) |>
        DT::formatSignif(c("netto_flux", "sumR_integrated"))
    )

    #--------------------------------------------------------------------------
    # Mass Balances > Element based detailed
    #--------------------------------------------------------------------------
    output$tbl_element_based_mass_balance_C <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_detailed$C,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_detailed$C), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "sumR_integrated"))
    )

    output$tbl_element_based_mass_balance_N <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_detailed$N,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_detailed$N), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "sumR_integrated"))
    )

    output$tbl_element_based_mass_balance_P <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_detailed$P,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_detailed$P), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "sumR_integrated"))
    )

    output$tbl_element_based_mass_balance_S <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_detailed$S,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_detailed$S), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "sumR_integrated"))
    )

    output$tbl_element_based_mass_balance_Fe <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_detailed$Fe,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_detailed$Fe), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "sumR_integrated"))
    )

    output$tbl_element_based_mass_balance_Mn <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_detailed$Mn,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_detailed$Mn), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "sumR_integrated"))
    )

    output$tbl_element_based_mass_balance_Ca <- DT::renderDT(
        DT::datatable(
            rVs$element_based_mass_balance_detailed$Ca,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$element_based_mass_balance_detailed$Ca), dom = "t")
        ) |>
        DT::formatSignif(c("flux_up", "flux_down", "sumR_integrated"))
    )


    #--------------------------------------------------------------------------
    # Depth Integrated Reaction Rates
    #--------------------------------------------------------------------------
    output$tbl_integrated_reaction_rates <- DT::renderDT(
        DT::datatable(
            rVs$integrated_reaction_rates,
            rownames = FALSE,
            filter  = "top",
            fillContainer = TRUE,
            class = "hover",
            options = list(pageLength = nrow(rVs$integrated_reaction_rates), dom = "t")
        ) |>
        DT::formatSignif(c("value"))
    )

}