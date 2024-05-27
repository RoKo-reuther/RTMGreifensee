#' Measured Concentration Profiles(Greifensee)
#'
#' A dataset containing concentration profiles of pH, alkalinity, nitrate, ammonia,
#' sulfate, sulfide, Fe(II), Mn(II), Ca(II) + Mg(II) and phosphate,
#' measured in Greifensee or calculated based on measurements.
#' Data was digitalized from printed figures.
#'
#' @format A data frame with 410 rows and 4 columns:
#' \describe{
#'   \item{depth}{measuring depth in m}
#'   \item{value}{concentration in mol m-3 porewater or pH unit}
#'   \item{name}{name of measured concentration}
#'   \item{tag}{'ref_data', used as identifier when combined with other data as a long-table}
#' }
#' 
#' @source Wersin, P., Höhener, P., Giovanoli, R., & Stumm, W. (1991). Early diagenetic influences on iron transformations in a freshwater lake sediment. Chemical Geology, 90(3-4), 233-252.
#' @docType data
#' @keywords datasets
#' @name greifensee_data
#' @usage data(greifensee_data)
NULL


#' Steady State Result fitted to 'greifensee_data'
#'
#' Steady State Result fitted to 'greifensee_data'
#'
#' @format A list returned by 'solve_steady'.
#' @docType data
#' @keywords datasets
#' @name reference_state
#' @usage data(reference_state)
NULL