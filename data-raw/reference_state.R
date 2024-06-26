#-------------------------------------------------------------------------------
# Save Reference State
#-------------------------------------------------------------------------------
model_parameters <- RTMGreifensee:::set_parameters()

reference_state <- RTMGreifensee:::solve_steady(model_parameters, tag = "ref")

usethis::use_data(reference_state, overwrite = TRUE)
