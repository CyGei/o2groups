#' simulate_groups
#'
#' Simulate the spread of an infectious disease among multiple groups
#'
#' @param duration An integer value representing the duration of the simulation
#' @param group_n An integer value representing the number of groups
#' @param size A vector of integers representing the sizes of each group
#' @param name A vector of characters representing the names of each group
#' @param gamma A numeric vector representing the within-group transmission factor (assortativity coefficient) for each group
#' @param intro_n A vector of n introductions respective to each group.
#' @param r0 A numeric vector representing the basic reproductive number (R0) for each group
#' @param generation_time The probability mass function (pmf) of the generation time (sums to 1).
#' @param incubation_period Optional. A vector of integers that will be sampled with replacement.
#' @param dt The time step of the simulation (default = 1 day)
#' @param quietly Logical. Whether to print progress or not. Default is TRUE (doesn't print progress).
#'
#' @return The transmission tree (data.frame) informing who infected whom and when.
#'
#' @export
#'
#' @examples
#' simulate_groups(
#'   duration = 100,
#'   group_n = 3,
#'   size = c(100, 50, 200),
#'   name = c("A", "B", "C"),
#'   gamma = c(1, 2, 5),
#'   intro_n = c(0, 7, 5),
#'   r0 = c(2, 3, 4),
#'   generation_time = c(0.1, 0.3, 0.4, 0.2)
#' )
#'
simulate_groups <- function(duration = 100,
                            group_n,
                            size,
                            name,
                            gamma,
                            intro_n,
                            r0,
                            generation_time,
                            incubation_period = NULL,
                            dt = 1,
                            quietly = TRUE) {
  #check inputs
  if(duration < 1){
    stop("The duration of the simulation must be at least 1 day.")
  }
  if(group_n < 1){
    stop("The number of groups must be at least 1.")
  }
  if (length(size) != group_n) {
    stop("The length of 'size' must be equal to 'group_n'")
  }
  if(any(size < 1)){
    stop("The size of each group must be at least 1.")
  }
  if (length(name) != group_n) {
    stop("The length of 'name' must be equal to 'group_n'")
  }
  if (length(gamma) != group_n) {
    stop("The length of 'gamma' must be equal to 'group_n'")
  }
  if (length(r0) != group_n) {
    stop("The length of 'r0' must be equal to 'group_n'")
  }
  if (length(intro_n) != group_n) {
    stop("The length of 'intro_n' must be equal to 'group_n'")
  }
  if(sum(intro_n) < 1){
    stop("There must be at least one introduction in the simulation.")
  }
  if(any(size < intro_n)){
    stop("The size of each group must be greater than the number of introductions.")
  }



  # Extend the pmf for the whole duration
  generation_time <- c(generation_time, rep(0, duration))

  # Basic Reproduction Number Matrix
  M0 <- generate_M0(
    group_n = group_n,
    size = size,
    name = name,
    r0 = r0,
    gamma = gamma
  )


  # Introductions on day 0
  data <-
    data.frame(
      group = rep(name, intro_n),
      id = simulacr::draw_labels(sum(intro_n)),
      source = NA,
      source_group = NA,
      date_infection = 0
    )

  R0mat <- matrix(nrow = 0, ncol = group_n, dimnames = list(NULL, name))

  # Loop through days:
  for (t in 1:duration) {
    if (isFALSE(quietly)) {
      cat(paste0("...", t, " "))
    }

    # R0 matrix per infected for each group:
    if (group_n > 1) {
      R0mat_t <- M0[, data$group[data$date_infection == t - 1]]
      R0mat_t <- matrix(R0mat_t, ncol = group_n)
    } else {
      R0mat_t <-
        matrix(M0[, data$group[data$date_infection == t - 1]], ncol = 1)
    }
    colnames(R0mat_t) <- name
    rownames(R0mat_t) <- data$id[data$date_infection == t - 1]
    R0mat <- rbind(R0mat, R0mat_t)

    # FOI: (t+1) Vector index in R starts from 1 not 0.
    indivFOI <- generation_time[c((t + 1) - data$date_infection)] * R0mat
    groupFOI <- colSums(indivFOI)

    # Daily stats : Computes key statistics for each group
    stats_t <- get_stats(data, t, group_n, name, size, groupFOI, dt)

    if (sum(stats_t$new_cases) > 0) {
      # sampling source IDs based on their FOI
      sources <-
        mapply(
          sample_sources,
          group = stats_t$group,
          n_cases = stats_t$new_cases,
          MoreArgs = list(data = data, indivFOI = indivFOI),
          SIMPLIFY = FALSE
        )

      # Generating new cases
      cases <- generate_new_cases(
        data = data,
        t = t,
        sources = sources
      )

      data <- rbind(data, cases)
    }

  }

  # add onset date if incubation period added
  if (!is.null(incubation_period)) {
    incubation_sample <-
      sample(incubation_period,
        size = nrow(data),
        replace = TRUE
      )
    data$date_onset <- data$date_infection + incubation_sample
  }

  return(data)
}
