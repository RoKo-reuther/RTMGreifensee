# =============================================================================
# > Add up Ca2 & Mg2
# =============================================================================

Ca2 <- read.csv("data-raw/GreifenseeProfiles/Ca2.csv")
Mg2 <- read.csv("data-raw/GreifenseeProfiles/Mg2.csv")

Ca2_approxfun <- approxfun(Ca2$depth_cm, Ca2$C_umolL, rule = 2)
Mg2_approxfun <- approxfun(Mg2$depth_cm, Mg2$C_umolL, rule = 2)

x <- seq(min(c(Ca2$depth_cm, Mg2$depth_cm)), max(c(Ca2$depth_cm, Mg2$depth_cm)), by = 1)
Ca2_approximated <- Ca2_approxfun(x)
Mg2_approximated <- Mg2_approxfun(x)

#curve(Ca2_approxfun, -10, 50)
#points(Ca2$depth_cm, Ca2$C_umolL)
#points(x, Ca2_approximated, col = "red")

#curve(Mg2_approxfun, -10, 50)
#points(Mg2$depth_cm, Mg2$C_umolL)
#points(x, Mg2_approximated, col = "red")

Ca2Mg2 <- data.frame(
    C_umol   = Ca2_approximated + Mg2_approximated,
    depth_cm = x
)

#plot(Ca2Mg2$depth_cm, Ca2Mg2$C_umol, type = "l", ylim = c(0, 3500))
#points(Ca2$depth_cm, Ca2$C_umolL, type = "l")
#points(Mg2$depth_cm, Mg2$C_umolL, type = "l")

write.csv(Ca2Mg2, "data-raw/GreifenseeProfiles/Ca2Mg2.csv", quote = FALSE, row.names = FALSE)


# =============================================================================
# > Combine Greifensee Data to list & data.frame
# =============================================================================
adjust_colnames <- function(data) {
  colnames(data)[1] <- "value"
  colnames(data)[2] <- "depth"
  return(data)
}

cm_to_m_greifensee <- function(data) {
  data$depth <- data$depth * 1e-2
  return(data)
}

umolL_to_molm3_greifensee <- function(data){
  data$value <- data$value * 1e-3
  return(data)
}

# all depths in cm; all concentrations in umol L-1
greifensee_data_list = list(
  pH = read.csv("data-raw/GreifenseeProfiles/pH.csv") |> adjust_colnames() |> cm_to_m_greifensee(),
  TOT_H = read.csv("data-raw/GreifenseeProfiles/ALK.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  TOT_NO3 = read.csv("data-raw/GreifenseeProfiles/NO3.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  TOT_NH4 = read.csv("data-raw/GreifenseeProfiles/NH3.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  TOT_SO4 = read.csv("data-raw/GreifenseeProfiles/SO4.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  TOT_H2S = read.csv("data-raw/GreifenseeProfiles/H2S.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  Fe2 = read.csv("data-raw/GreifenseeProfiles/Fe2.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  Mn2 = read.csv("data-raw/GreifenseeProfiles/Mn2.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  TOT_H2PO4 = read.csv("data-raw/GreifenseeProfiles/PO4.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee(),
  Ca2 = read.csv("data-raw/GreifenseeProfiles/Ca2Mg2.csv") |> adjust_colnames() |> cm_to_m_greifensee() |> umolL_to_molm3_greifensee()
)

#usethis::use_data(greifensee_data_list, overwrite = TRUE)

# from the list create a long table: depth, value, name, tag
greifensee_data <- data.frame(depth = NULL, value = NULL, name = NULL, tag = NULL)

for (name in names(greifensee_data_list)) {
  new_data <- data.frame(
    depth = greifensee_data_list[[name]]$depth,
    value = greifensee_data_list[[name]]$value,
    name = name,
    tag = "ref_data"
  )
  greifensee_data <- rbind(greifensee_data, new_data)
}

usethis::use_data(greifensee_data, overwrite = TRUE)

