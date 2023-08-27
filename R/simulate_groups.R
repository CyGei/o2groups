#' simulate_groups
#'
#' Simulate the spread of an infectious disease among multiple groups
#'
#' @param duration An integer value representing the duration of the simulation
#' @param n_groups An integer value representing the number of groups
#' @param size A vector of integers representing the sizes of each group
#' @param name A vector of characters representing the names of each group
#' @param delta A numeric vector representing the within-group transmission factor (assortativity coefficient) for each group
#' @param intro_n A vector of n introductions respective to each group.
#' @param r0 A numeric vector representing the basic reproductive number (R0) for each group
#' @param generation_time The probability mass function (pmf) of the generation time (sums to 1).
#' @param incubation_period Optional. A vector of integers that will be sampled with replacement.
#' @param dt The time step of the simulation (default = 1 day)
#' @param quietly Logical. Whether to print progress or not. Default is TRUE (doesn't print progress).
#'
#' @return A list containing the individual linelist, the summary statistics per time step & input matrices (Mcol/M0).
#'
#' @export
#'
#' @examples
#' simulate_groups(
#'   duration = 100,
#'   n_groups = 3,
#'   size = c(100, 50, 200),
#'   name = c("A", "B", "C"),
#'   delta = c(1, 2, 5),
#'   intro_n = c(0, 7, 5),
#'   r0 = c(2, 3, 4),
#'   generation_time = c(0.1, 0.3, 0.4, 0.2))
#'
#'
simulate_groups <- function(duration = 100, #Duration of the simulation
                            n_groups, #An integer
                            size, #A vector of integers
                            name = NULL, #A vector of characters
                            delta = rep(1, n_groups), #A numeric vector: within-group transmission factor
                            intro_n = NULL, #A vector of n introduction respective to each group
                            r0, #A numeric vector
                            generation_time, #The pmf
                            incubation_period = NULL, #a vector of integers
                            dt = 1, # The time step of the simulation (1 day)
                            quietly = TRUE
) {
  #Extend the pmf for the whole duration
  generation_time <- c(generation_time, rep(0, duration))

  # Mixing Matrix
  M0 <- generate_M0(
    n_groups = n_groups,
    size = size,
    name = name,
    r0 = r0,
    delta = delta)


  # Introductions on day 0
  data <-
    data.frame(
      group = rep(name, intro_n),
      id = simulacr::draw_labels(sum(intro_n)),
      source = NA,
      source_group = NA,
      date_infection = 0
    )

  stats <- data.frame()
  R0mat <- matrix(ncol = n_groups)
  colnames(R0mat) <- name
  R0mat <- R0mat[-1, ]


  # Loop through days:
  for (t in 1:duration) {

    if(isFALSE(quietly)){
      cat(paste0("...", t, " "))
    }

    # R0 matrix per infected for each group:
    if (n_groups > 1) {
      R0mat_t <- t(M0[, data$group[data$date_infection == t-1]])
    } else {
      R0mat_t <-
        matrix(M0[, data$group[data$date_infection == t-1]], ncol = 1)
    }
    colnames(R0mat_t) <- name
    rownames(R0mat_t) <- data$id[data$date_infection == t-1]
    R0mat <- rbind(R0mat, R0mat_t)

    # FOI: (t+1) Vector index in R starts from 1 not 0.
    indivFOI <- generation_time[c((t+1) - data$date_infection)] * R0mat
    groupFOI <- colSums(indivFOI)

    #Daily stats : New cases generated based on group FOI
    stats_t <- get_stats(data, t, n_groups, name, size, groupFOI, dt)

    if (sum(stats_t$new_cases) > 0) {

      #sampling source IDs based on their FOI
      sources <-
        mapply(
          sample_sources,
          group = stats_t$group,
          n_cases = stats_t$new_cases,
          MoreArgs = list(data = data, indivFOI = indivFOI),
          SIMPLIFY = FALSE
        )

      # Generating new cases
      cases <- generate_new_cases(data = data,
                                  t = t,
                                  sources = sources)

      data <- rbind(data, cases)

    }

    stats <- rbind(stats, stats_t)
  }

  #add onset date if incubation period added
  if (!is.null(incubation_period)) {
    incubation_sample <-
      sample(incubation_period,
             size = nrow(data),
             replace = TRUE)
    data$date_onset <- data$date_infection + incubation_sample
  }

  return(list(
    data = data,
    stats = stats,
    M0 = M0
  ))

}
