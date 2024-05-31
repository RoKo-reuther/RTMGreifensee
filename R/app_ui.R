# =============================================================================
# > RTMGreifenseeApp-UI
# =============================================================================
app_ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(disable = TRUE),

    shinydashboard::dashboardSidebar(
        tags$head(tags$style(HTML('
            .content {
                height: 100vh;
                scrollbar-width: none;
                padding: 0px 15px 0px 15px;
            }

            .tab-content {
                height: 89vh;
                overflow-y: auto;
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

            #tbl_integrated_reaction_rates {
                margin: auto;
            }
        '))),
        div(style = "height: 7vh; background-color: #3d8cbb;",
            tags$style(HTML('
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
            tags$style(HTML('
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
                title = "Saturation",
                uiOutput("saturation_profiles")
            ),
            tabPanel(
                title = "Reation Rate Profiles",
                uiOutput("reaction_rate_profiles")
            ),
            tabPanel(
                title = "Depth Integrated Reation Rates",
                DT::DTOutput("tbl_integrated_reaction_rates", height = "87vh", width = "800px")
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