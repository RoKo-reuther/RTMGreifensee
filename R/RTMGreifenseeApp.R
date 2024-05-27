#' @import shiny

RTMGreifenseeApp <- function() {

# =============================================================================
# > UI
# =============================================================================
ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(disable = TRUE),

    shinydashboard::dashboardSidebar(
        shiny::tags$head(shiny::tags$style(shiny::HTML('
            .content {
                height: 100vh;
                scrollbar-width: none;
                padding: 0px 15px 0px 15px;
            }

            .tab-content {
                height: 89vh;
                overflow-y: scroll;
            }

            .nav-tabs-custom {
                margin-bottom: 0px;
                margin-right: -15px;
                margin-left: -15px;
            }

            h3 {
                font-size: 15px;
                font-weight: 800;
            }

            hr {
                border: 2px solid #999999d1;
            }

            .main-sidebar {
                padding-top: 0px;
            }

            .sidebar {
                padding-bottom: 0px;
            }

            .scrollable_container_in_sidebar {
                height: calc(93vh - 40px);
                overflow-y: scroll;
                scrollbar-width: none;
                margin-top: 20px;
                margin-bottom: 20px;
            }

            section.sidebar .shiny-input-container {
                padding: 0px 15px 0px 15px;
                width: 85%;
            }

            .form-group {
                margin-bottom: 0px;
            }

            label {
                margin-bottom: -7px;
                margin-left: 4px;
                padding-top: 10px;
            }

            section.sidebar .form-control{
                background-color: #222c33;
                border-color: #9a9d9f;
                border-width: 2px;
                border-radius: 4px;
                font-size: 15px;
                font-weight: 600;
                color: #ecf1f4;
            }

            .input-group-btn {
                padding-top: 0px;
            }

            .shiny-file-input-progress {display: none;}
        '))),
        div(style = "height: 7vh; background-color: #3d8cbb;",
            shiny::tags$style(shiny::HTML('
                h4 {
                    padding-left: 27px;
                    padding-top: 7px;
                    margin-top: 0px;
                    font-weight: 600;;
                }
                .col-sm-6 {
                    padding-left: 10px;
                    padding-right: 10px;
                }
                section.sidebar .shiny-input-container {
                    margin: auto;
                    padding: 0px;
                }
                #new_tag {
                    border-width: 0px;
                }
            ')),
            h4("Add a new Scenario ..."),
            column(textInput("new_tag", label = NULL, value = "std", width = "100%"), width  = 6),
            column(actionButton("goButton", "Go!", width  ="100%", style = "margin: auto; color: #3c8dbc; background-color: #222d32; border: 0px; font-size: 15px; font-weight: 600;"), width = 6)
        ),
        div(class = "scrollable_container_in_sidebar",
            shiny::tags$style(shiny::HTML('
                .scrollable_container_in_sidebar h4 {
                    font-size: 22px; font-weight: 600; padding: 0px 15px 0px 15px; margin-top: 0px; margin-bottom: 0px;
                }
            ')),
            h4("Reaction Parameters"),
            uiOutput("reaction_parms"),
            hr(),
            h4("Boundary Conditions"),
            uiOutput("boundary_conditions"),
            hr(),
            h4("Environmental Parameters"),
            uiOutput("environmental_parms")
        )
    ),

    shinydashboard::dashboardBody(
        fluidRow(
            column(
                fluidRow(
                    column("Scenarios:", width = 2, style = "font-weight: 600; font-size: 15px; top: 50%; transform: translateY(-50%); text-align: right;"),
                    column(selectInput("active_tags", NULL, c("ref"), c("ref"), multiple = TRUE, selectize = TRUE, width = "100%"), width = 8),
                    column(actionButton("selectScenariosButton", label = "select", width = "100%"), width = 2),
                    style = "height: 34px; width: 100%; margin: 0; position: absolute; top: 50%; transform: translateY(-50%);"
                ),
                width = 5,
                style = "position: relative; height: 100%; padding: 0px; z-index: 1"
            ),
            column(width = 2),
            column(
                fluidRow(
                    column("Up-/Download a list of Scenarios:", width = 5, style = "font-weight: 600; font-size: 15px; top: 50%; transform: translateY(-50%); text-align: right;"),
                    column(fileInput("std_list_input", label = NULL, buttonLabel = "Upload", multiple = FALSE, accept = c(".rds"), width = "100%"), width = 4),
                    column(downloadButton("std_list_download", "Download", width = "100%"), width = 3),
                    style = "height: 34px; width: 100%; margin: 0; position: absolute; top: 50%; transform: translateY(-50%);"
                ),
                width = 5,
                style = "position: relative; height: 100%; padding: 0px; z-index: 1"
            ),
            style = "height: 7vh;" #padding-top: 2vh;
        ),
        shinydashboard::tabBox(
            width = 12,
            tabPanel(
                title = "Concentrations",
                uiOutput("species_profiles"),
                uiOutput("tableau_species_profiles")
            ),
            tabPanel(
                title = "Reation Rates",
                uiOutput("reaction_rate_profiles")
            ),
            tabPanel(
                title = "Saturation",
                uiOutput("saturation_profiles")
            ),
            tabPanel(
                title = "Mass Balances",
                shinydashboard::box(
                    title = "Species based",
                    width = 12,
                    DT::DTOutput("tbl_species_based_mass_balance", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: Overview",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_overview", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: C",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_C", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: N",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_N", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: P",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_P", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: S",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_S", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: Fe",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_Fe", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: Mn",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_Mn", height = 500)
                ),
                shinydashboard::box(
                    title = "Element based: Ca",
                    width = 6,
                    DT::DTOutput("tbl_element_based_mass_balance_Ca", height = 500)
                )
            )
        )
    )
)


# =============================================================================
# > SERVER
# =============================================================================
server <- function(input, output, session) {

    #--------------------------------------------------------------------------
    # Initialize
    #--------------------------------------------------------------------------
    rVs <- reactiveValues()

    rVs$std_list <- list(ref = RTMGreifensee::reference_state)

    rVs$collective_dataframe <- get_collective_profiles_dataframe(list(ref = RTMGreifensee::reference_state), RTMGreifensee::greifensee_data)

    rVs$species_based_mass_balance          <- get_collective_species_based_massbalance(isolate(rVs$std_list))
    rVs$element_based_mass_balance_overview <- get_collective_element_based_overview_massbalance(isolate(rVs$std_list))
    rVs$element_based_mass_balance_detailed <- get_collective_element_based_detailed_massbalance(isolate(rVs$std_list))


    #--------------------------------------------------------------------------
    # Helpers
    #--------------------------------------------------------------------------
    # create an parameter input-element
    create_parameter_input <- function(parameter) {
        numericInput(parameter, label = parameter, value = RTMGreifensee::reference_state$parameters[parameter])
    }

    # create profile-plot ouput element
    plot_profile_output <- function(element) {
        shinydashboard::box(
            echarts4r::echarts4rOutput(paste0("profile_", element)),
            width = 4
        )
    }


    #--------------------------------------------------------------------------
    # Parameter Panel Setup
    #--------------------------------------------------------------------------
    output$reaction_parms <- renderUI({

        output_list <- lapply(model_metadata$reaction_parms, create_parameter_input)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        tagList(output_list)

    })

    output$boundary_conditions <- renderUI({

        output_list <- lapply(model_metadata$boundary_conditions, create_parameter_input)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        tagList(output_list)

    })

    output$environmental_parms <- renderUI({

        output_list <- lapply(model_metadata$environmental_parms, create_parameter_input)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        tagList(output_list)

    })


    #--------------------------------------------------------------------------
    # Steady-State Calculation
    #--------------------------------------------------------------------------
    observe({

        withProgress(message = "Calculating new scenario ...", {

        parms_list <- list()

        for (parameter in c(model_metadata$environmental_parms, model_metadata$reaction_parms, model_metadata$boundary_conditions)) {
            parms_list[[parameter]] <- input[[parameter]]
        }

        model_parameters <- do.call("set_parameters", parms_list)

        setProgress(0.1)
    
        std <- solve_steady(model_parameters, tag = paste0("@", input$new_tag), initial = RTMGreifensee::reference_state$original$y)

        if (!is.null(std)) {

            setProgress(0.8)
    
            rVs$std_list[[paste0("@", input$new_tag)]] <- std

            updateSelectInput(session, "active_tags", choices = names(rVs$std_list), selected = input$active_tags)

            setProgress(1.0)
            
        } else {

            setProgress(1.0)
            showNotification("Solving failed for this parameter-set.", type = "error")

        }

        })
    
    }) |> bindEvent(input$goButton)


    #--------------------------------------------------------------------------
    # Active Scenario Selection
    #--------------------------------------------------------------------------
    observe({

        rVs$collective_dataframe <- get_collective_profiles_dataframe(rVs$std_list[input$active_tags], RTMGreifensee::greifensee_data)
        rVs$species_based_mass_balance <- get_collective_species_based_massbalance(rVs$std_list[input$active_tags])
        rVs$element_based_mass_balance_overview <- get_collective_element_based_overview_massbalance(rVs$std_list[input$active_tags])
        rVs$element_based_mass_balance_detailed <- get_collective_element_based_detailed_massbalance(rVs$std_list[input$active_tags])

    }) |> bindEvent(input$selectScenariosButton)


    #--------------------------------------------------------------------------
    # Download list of steady state scenarios
    #--------------------------------------------------------------------------
    output$std_list_download <- downloadHandler(
        filename = "std_list.rds",
        content = function(file) {
          saveRDS(rVs$std_list, file)
        }
      )


    #--------------------------------------------------------------------------
    # Upload list of steady state scenarios
    #--------------------------------------------------------------------------
    observe({
        rVs$std_list <- readRDS(input$std_list_input$datapath)
        updateSelectInput(session, "active_tags", choices = names(rVs$std_list), selected = input$active_tags)
    }) |> bindEvent(input$std_list_input)


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

    # output species profiles
    output$species_profiles <- renderUI({
        # create output elements
        plot_output_list <- lapply(model_metadata$species, plot_profile_output)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        tagList(plot_output_list)
    })

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

    # output species profiles
    output$tableau_species_profiles <- renderUI({
        # create output elements
        plot_output_list <- lapply(model_metadata$tableau_species, plot_profile_output)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        tagList(plot_output_list)

    })


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

    # output profiles
    output$reaction_rate_profiles <- renderUI({
        # create output elements
        plot_output_list <- lapply(model_metadata$reactions, plot_profile_output)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        tagList(plot_output_list)

    })


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

    # output profiles
    output$saturation_profiles <- renderUI({
        # create output elements
        plot_output_list <- lapply(model_metadata$omegas, plot_profile_output)
        # convert the list to a tagList - this is necessary for the list of items to display properly.
        tagList(plot_output_list)

    })


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

}


# =============================================================================
# > RUN
# =============================================================================
#shinyApp(ui, server, options = list(port = 4166, launch.browser = FALSE))
shinyApp(ui, server)

}
