#' Process Simulation Results
#'
#' This function processes the results of a simulation by calculating delta estimates for each group and simulation based on the provided peak coefficient.
#'
#' @param simulation_data A data frame containing the simulation results, with columns for simulation ID, group, and other relevant variables.
#' @param peak_coeff A numeric value representing the peak coefficient for calculating the maximum time range for delta estimation.
#' @param size A vector of integers representing the sizes of each group.
#' @param name A vector of characters representing the names of each group.
#'
#' @return A data frame containing the delta estimates for each group and simulation.
#'
#' @keywords internal


process_simulation <- function(simulation_data, peak_coeff, size, name) {
  sim <- split(simulation_data, simulation_data$simulation)

  n_groups <- length(name)
  n_sims <- length(sim)
  obs <- n_groups * n_sims

  delta_results <- data.frame(
    simulation = integer(obs),
    group = character(obs),
    est = numeric(obs),
    lower_ci = numeric(obs),
    upper_ci = numeric(obs),
    successes = integer(obs),
    trials = integer(obs),
    stringsAsFactors = FALSE
  )

  row_indices <- seq(1, obs, by = n_groups)

  for (i in seq_along(sim)) {

    data <- sim[[i]]
    peaks <- o2groups:::get_peak(data)

    d_sim <- data.frame(
      simulation = i,
      group = name,
      est = rep(NA, n_groups),
      lower_ci = rep(NA, n_groups),
      upper_ci = rep(NA, n_groups),
      successes = rep(NA, n_groups),
      trials = rep(NA, n_groups),
      row.names = name
    )

    for (group in names(peaks)) {
      d_group <- o2groups::early_delta(
        data = data,
        min_t = 0,
        max_t = peak_coeff * peaks[[group]],
        size = size,
        name = name
      )

      #check that early_delta returned an estimate for the group
      if (group %in% rownames(d_group)) {
        d_sim[group, colnames(d_group)] <- d_group[group, ]
      }
    }

    # Insert d_sim into delta_results at the appropriate rows
    delta_results[row_indices[i]:(row_indices[i] + n_groups - 1), ] <- d_sim

  }

  return(delta_results)
}



