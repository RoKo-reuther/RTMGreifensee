

# =============================================================================
# > RUN THE APP
# =============================================================================

#' @title
#' Shiny application to analyze steady-state behaviour
#' 
#' @description
#' Shiny application to analyze steady-state behaviour of a reactive-transport
#' sediment model.
#' 
#' @details
#' The app initializes with measured data from Greifensee in [`greifensee_data`][greifensee_data] 
#' as well as fitted steady-state profiles with the tag `ref`.
#' In the sidebar parameters can be altered, a new tag can be given and the new result will 
#' be added to a steay-state-scenarios list.
#' This list can be downloaded for further analysis of the scenarios.
#' The UI allows an upload of a stedy-state-scenarios list, which was downloaded prior.
#' 
#' @export
RTMGreifenseeApp <- function() {

    shiny::shinyApp(ui = app_ui, server = app_server)

}
