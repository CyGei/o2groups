#' Extract Scenario
#'
#' This function takes a list of scenarios and extracts the relevant parameters from each scenario to create a dataframe.
#'
#' @param scenarios A list of scenarios.
#'
#' @return A dataframe with the extracted scenario parameters.
#'
#' @keywords internal

extract_scenario <- function(scenarios){
  # Create an empty dataframe
  scenario_df <- data.frame(scenario = character(),
                            group = character(),
                            size = character(),
                            delta = character(),
                            r = character(),
                            intro_n = character(),
                            stringsAsFactors = FALSE)

  # Iterate over each scenario
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]
    scenario_name <- scenario$scenario
    scenario_param <- scenario$param
    # Extract scenario parameters
    n_groups <- scenario$n_groups
    size <- scenario$size
    name <- scenario$name
    delta <- scenario$delta
    intro_n <- scenario$intro_n
    r <- scenario$r0

    # Iterate over each group in the scenario
    for (j in seq_along(name)) {
      group <- name[j]

      # Add a row to the dataframe for the group in the scenario
      scenario_df <- rbind(scenario_df,
                           data.frame(scenario = scenario_name,
                                      param = scenario_param,
                                      group = group,
                                      size = paste(size[j], collapse = "/"),
                                      delta = paste(delta[j], collapse = "/"),
                                      r = paste(r[j], collapse = "/"),
                                      intro_n = intro_n[j],
                                      stringsAsFactors = FALSE))
    }
  }

return(scenario_df)
}
