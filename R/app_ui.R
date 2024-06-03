#' @include model_specification.R

# =============================================================================
# > RTMGreifenseeApp-UI
# =============================================================================

#------------------------------------------------------------------------------
# Sidebar
#------------------------------------------------------------------------------
# create an parameter input-element
create_parameter_input <- function(parameter) {
    shiny::numericInput(parameter, label = parameter, value = 0.0)
}

new_scenario_control <- bslib::card(
    fill = FALSE,
    class = "border-2",
    bslib::card_body(
        class = "border-0",
        shiny::p("Add new Scenario"),
        bslib::layout_column_wrap(
            width = 1/2,
            shiny::textInput("new_tag", label = NULL, value = "std", width = "100%"),
            shiny::actionButton("goButton", "Go", width  ="100%")
        )
    )
)

parameters_navset <- bslib::navset_card_underline(
    title = new_scenario_control,
    id = "navset_sidebar",
    height = "calc(100vh - 48.88px)",
    bslib::nav_panel(
        title = "Reaction Parameters",
        shiny::tagList(lapply(model_metadata$reaction_parms, create_parameter_input))
        #shiny::uiOutput("reaction_parms")
    ),
    bslib::nav_panel(
        title = "Boundary Conditions",
        shiny::tagList(lapply(model_metadata$boundary_conditions, create_parameter_input))
        #shiny::uiOutput("boundary_conditions")
    ),
    bslib::nav_panel(
        title = "Grid & Transport",
        shiny::tagList(lapply(model_metadata$environmental_parms, create_parameter_input))
        #shiny::uiOutput("environmental_parms")
    )
)

dashboard_sidebar <- bslib::sidebar(
    open = "always",
    padding = "0px",
    parameters_navset
)


#------------------------------------------------------------------------------
# Panel I: Scenario List Handling
#------------------------------------------------------------------------------
select_active_scenarios <- bslib::card(
    fill = TRUE,
    bslib::card_header("Select active Scenarios"),
    bslib::layout_columns(
        col_widths = c(9, 3),
        shiny::selectInput("active_tags", NULL, c("ref"), c("ref"), multiple = TRUE, selectize = TRUE, width = "100%"),
        shiny::actionButton("selectScenariosButton", label = "select", width = "100%")
    )
)

im_and_export_std_list <- bslib::card(
    fill = TRUE,
    bslib::card_header("Up-/Download Scenario List"),
    bslib::layout_columns(
        col_widths = c(9, 3),
        shiny::fileInput("std_list_input", label = NULL, buttonLabel = "Upload", multiple = FALSE, accept = c(".rds"), width = "100%"),
        shiny::downloadButton("std_list_download", "Download", width = "100%")
    )
)

select_parameters <- bslib::card(
    fill = TRUE,
    bslib::card_header("Select Parameter-Set"),
    bslib::layout_columns(
        col_widths = c(9, 3),
        shiny::selectInput("parameter_set", NULL, c("ref"), "ref", multiple = FALSE, selectize = TRUE, width = "100%"),
        shiny::actionButton("selectParameterSet", label = "select", width = "100%")
    )
)

panel_scenarios <- bslib::nav_panel(
    title = "Scenarios",
    select_active_scenarios,
    im_and_export_std_list,
    select_parameters
)


#------------------------------------------------------------------------------
# Function: Profile-Plot-Card
#------------------------------------------------------------------------------
plot_profile_output <- function(element) {
    bslib::card(
        fill = FALSE,
        echarts4r::echarts4rOutput(paste0("profile_", element))
    )
}


#------------------------------------------------------------------------------
# Panel II: Concentrations
#------------------------------------------------------------------------------
panel_concentrations <- bslib::nav_panel(
    title = "Concentrations",
    bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        !!!lapply(model_metadata$species, plot_profile_output),
        !!!lapply(model_metadata$tableau_species, plot_profile_output)
    )
)


#------------------------------------------------------------------------------
# Panel III: Saturation
#------------------------------------------------------------------------------
panel_saturation <- bslib::nav_panel(
    title = "Saturation",
    bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        !!!lapply(model_metadata$omegas, plot_profile_output)
    )
)


#------------------------------------------------------------------------------
# Panel IV: Reaction Rate Profiles
#------------------------------------------------------------------------------
panel_reaction_rate_profiles <- bslib::nav_panel(
    title = "Reaction Rate Profiles",
    bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        !!!lapply(model_metadata$reactions, plot_profile_output)
    )
)


#------------------------------------------------------------------------------
# Panel V: Depth Integrated Reaction Rates
#------------------------------------------------------------------------------
panel_integrated_reaction_rates <- bslib::nav_panel(
    title = "Depth Integrated Reaction Rates",
    DT::DTOutput("tbl_integrated_reaction_rates")
)


#------------------------------------------------------------------------------
# Panel VI: Mass Balances
#------------------------------------------------------------------------------
tbl_mb_species_based <- bslib::card(
    fill = FALSE,
    bslib::card_header("Species based"),
    DT::DTOutput("tbl_species_based_mass_balance", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_overview <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: Overview"),
    DT::DTOutput("tbl_element_based_mass_balance_overview", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_C <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: C"),
    DT::DTOutput("tbl_element_based_mass_balance_C", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_N <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: N"),
    DT::DTOutput("tbl_element_based_mass_balance_N", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_P <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: P"),
    DT::DTOutput("tbl_element_based_mass_balance_P", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_S <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: S"),
    DT::DTOutput("tbl_element_based_mass_balance_S", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_Fe <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: Fe"),
    DT::DTOutput("tbl_element_based_mass_balance_Fe", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_Mn <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: Mn"),
    DT::DTOutput("tbl_element_based_mass_balance_Mn", height = "calc(100vh - 150px)", fill = FALSE)
)

tbl_mb_element_based_Ca <- bslib::card(
    fill = FALSE,
    bslib::card_header("Element based: Ca"),
    DT::DTOutput("tbl_element_based_mass_balance_Ca", height = "calc(100vh - 150px)", fill = FALSE)
)

panel_mass_balances <- bslib::nav_panel(
    title = "Mass Balances",
    tbl_mb_species_based,
    bslib::layout_column_wrap(
        width = 1/2,
        fill = FALSE,
        tbl_mb_element_based_overview,
        tbl_mb_element_based_C,
        tbl_mb_element_based_N,
        tbl_mb_element_based_P,
        tbl_mb_element_based_S,
        tbl_mb_element_based_Fe,
        tbl_mb_element_based_Mn,
        tbl_mb_element_based_Ca
    )
)


#------------------------------------------------------------------------------
# Whole UI
#------------------------------------------------------------------------------
#.shiny-file-input-progress {display: none;}
app_ui <- bslib::page_navbar(
    theme = bslib::bs_theme(preset = "lux", secondary = "#343b41", version = 5),
    title = NULL,
    sidebar = dashboard_sidebar,
    header = shiny::tags$style(shiny::HTML(
        '
        .navbar-static-top {padding-top: 3px; padding-bottom: 0px;}
        #navset_sidebar .nav-item {margin: auto;}
        '
    )),
    bslib::nav_spacer(),
    panel_scenarios,
    panel_concentrations,
    panel_saturation,
    panel_reaction_rate_profiles,
    panel_integrated_reaction_rates,
    panel_mass_balances,
    bslib::nav_spacer()
)
