# get_stats -----------------------------------------------------------------------------------

#' Calculate statistics for each group over time
#'
#' This function calculates statistics for each group over time, including the number of susceptible and infected individuals, force of infection (FOI), infection rate, infection probability, and new cases.
#' @param data A dataframe from simulate_groups().
#' @param t The time point.
#' @param group_n The number of groups.
#' @param name A character vector of length group_n specifying the names of the groups.
#' @param size A numeric vector of length group_n specifying the size of each group.
#' @param groupFOI A numeric vector of length group_n specifying the FOI that each group receives at time t.
#' @param dt A numeric value specifying the time interval between timesteps.
#' @return A data frame containing the calculated statistics for each group at each time point.
#'
#' @keywords internal


get_stats <- function(data , t, group_n, name, size, groupFOI, dt){

  stats <- data.frame(time = t,
                      group = name,
                      size = size)

#  Calculate the number of infected individuals for each group
  stats$n_infected <- sapply(name, function(group) {
    sum(data$group == group)
  })
  stats$n_susceptible <- size - stats$n_infected
  stats$prop_susceptible <- stats$n_susceptible / size
  stats$foi <- groupFOI
  stats$infection_rate <- stats$foi / size
  stats$infection_prob <- 1 - exp(-stats$infection_rate * dt)
  stats$new_cases <- stats::rbinom(
    n = group_n,
    size = stats$n_susceptible,
    prob = stats$infection_prob
  )

  return(stats)

}

# sample_sources ------------------------------------------------------------------------------

#' Samples infectors based on their Force of Infection
#'
#' This functions samples infector IDs based on their FOI targeting the group of interest
#'
#' @param group The current group of interest
#' @param n_cases The number of new cases to be generated in that group at time t
#' @param data The data/linelist at time t
#' @param indivFOI The individual FOI for all potential infectors targeting the group of interest (will be used as weight for sampling)
#' @return A list of source IDs
#' @keywords internal

sample_sources <- function(group, n_cases, data, indivFOI) {

  # Infector IDs
  sources <- data$id

  # Get the foi values for the infectors in the group
  source_foi <- indivFOI[, group]

  # Normalise the foi values
  source_prob <- source_foi / sum(source_foi)

  # Sample the infectors with replacement based on their foi
  sampled_ids <- sample(sources, size = n_cases, prob = source_prob, replace = TRUE)
  # Return the sampled IDs
  return(sampled_ids)
}


# generate_new_cases --------------------------------------------------------------------------

#' Generate new cases at time t
#'
#' @param data The current linelist at time t
#' @param t The current iteration of the timestep
#' @param sources The `sources` object returned by sample_sources
#' @return A dataframe / linelist of new cases
#' @keywords internal

generate_new_cases <- function(data, t, sources) {

  # Get the number of new cases for each group
  n_new_cases <- lengths(sources)

  # Generate the IDs of new cases
  case_ids <- simulacr::draw_labels(sum(n_new_cases))

  # Get the source IDs/groups for the new cases
  source <- unlist(sources, use.names = FALSE)

  source_group <- data$group[match(source, data$id)]

  # Create a new dataframe with the new cases
  new_cases <- data.frame(
    #name of sources list refers to the destination group
    group = rep(names(sources), n_new_cases),
    id = case_ids,
    source = source,
    source_group = source_group,
    date_infection = rep(t, sum(n_new_cases)),
    row.names = NULL
    )

  return(new_cases)
}


# simulate_groups_furrr -----------------------------------------------------------------------

#' simulate_groups_furrr
#'
#' Simulate multiple outbreaks in parallel. A furrr wrapper for `simulate_groups`.
#'
#' @param n_simulations The number of simulations to run.
#' @param n_cores The number of cores to use in parallel. Default is as.integer(future::availableCores() - 1).
#' @param duration An integer value representing the duration of the simulation.
#' @param group_n An integer value representing the number of groups.
#' @param size A vector of integers representing the sizes of each group.
#' @param name A vector of characters representing the names of each group.
#' @param delta A numeric vector representing the within-group transmission factor (assortativity coefficient) for each group.
#' @param intro_n A vector of integers representing the number of introductions for each group.
#' @param r0 A numeric vector representing the basic reproductive number (R0) for each group.
#' @param generation_time The probability mass function (pmf) of the generation time.
#' @param incubation_period Optional. A vector of integers that will be sampled with replacement.
#' @param dt The time step of the simulation (default = 1 day).
#' @param stack Logical. If TRUE, returns a stacked dataframe by simulation ID else returns a list of `simulate_groups()` outputs. Default is TRUE.
#'
#' @return A list containing data frames with information about the simulated groups and the statistics of the simulation & matrices inputs (Mcol/M0).
#' @export
#'
#' @examples
#' \dontrun{
#' library(furrr)
#' simulate_groups_furrr(
#'   n_simulations = 20,
#'   n_cores = 5,
#'   duration = 100,
#'   group_n = 3,
#'   size = c(100, 50, 200),
#'   name = c("A", "B", "C"),
#'   delta = c(1, 2, 5),
#'   intro_n = c(7, 5),
#'   r0 = c(2, 3, 4),
#'   generation_time = c(0.1, 0.3, 0.4, 0.2))
#' }
#'
#'


simulate_groups_furrr <- function(n_simulations,
                                  n_cores = as.integer(future::availableCores() - 1),
                                  duration,
                                  group_n,
                                  size,
                                  name,
                                  delta,
                                  intro_n,
                                  r0,
                                  generation_time,
                                  incubation_period = NULL,
                                  dt = 1,
                                  stack = TRUE) {
  # Set up parallel processing using furrr
  future::plan("future::multisession", workers = n_cores)

  # Run the simulations in parallel
  results <- furrr::future_map(1:n_simulations, ~ {
    simulate_groups(
      duration = duration,
      group_n = group_n,
      size = size,
      name = name,
      delta = delta,
      intro_n = intro_n,
      r0 = r0,
      generation_time = generation_time,
      incubation_period = incubation_period,
      dt = dt
    )
  },
  .options = furrr::furrr_options(seed = TRUE),
  verbose = FALSE
  )


  if (isTRUE(stack)) {
    #stack simulations
    data <- purrr::map_dfr(results, ~ .x$data, .id = "simulation")
    stats <- purrr::map_dfr(results, ~ .x$stats, .id = "simulation")

    return(list(data = data, stats = stats))

  } else {
    return(results)
  }
}
