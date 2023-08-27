#' Calculate Delta Estimates and Confidence Intervals in Early Phase.
#'
#' This function calculates the delta estimates and 95% confidence intervals
#' from mixing data based on binomial test.
#'
#' @param data The input data containing the observed infections (simulate_groups).
#' @param min_t The minimum time range for considering infections.
#' @param max_t The maximum time range for considering infections.
#' @param n_groups The number of groups in the mixing matrix.
#' @param size The total population size.
#' @param name The name of the groups.
#'
#' @return A matrix containing the delta estimates, lower confidence intervals, and upper confidence intervals.


Mcol_to_delta_ci <-
  function(data, min_t, max_t, n_groups, size, name) {

    mixing <- get_mixing(data, min_t = min_t, max_t = max_t)$result

    total_n <- tapply(mixing$n, mixing$source_group, sum)

    est <- vector(length = nrow(mixing))
    lower_ci <- vector(length = nrow(mixing))
    upper_ci <- vector(length = nrow(mixing))

    for (i in 1:nrow(mixing)) {
      binom_result <- stats::binom.test(mixing$n[i], total_n[mixing$source_group[i]])
      est[i] <- binom_result$estimate[[1]]
      lower_ci[i] <- binom_result$conf.int[1]
      upper_ci[i] <- binom_result$conf.int[2]
    }

    mix_binom <- cbind(mixing, est, lower_ci, upper_ci)

    #you might need to convert source_group to factor !!!!
    #to have the matrix in the right order            !!!!
    mix_binom <- mix_binom[order(mix_binom$source_group), ]

    M_low <- matrix(
      mix_binom$lower_ci,
      ncol = n_groups,
      nrow = n_groups,
      dimnames = list(LETTERS[1:n_groups], LETTERS[1:n_groups])
    )
    M_high <- matrix(
      mix_binom$upper_ci,
      ncol = n_groups,
      nrow = n_groups,
      dimnames = list(LETTERS[1:n_groups], LETTERS[1:n_groups])
    )

    M_est <- matrix(
      mix_binom$est,
      ncol = n_groups,
      nrow = n_groups,
      dimnames = list(LETTERS[1:n_groups], LETTERS[1:n_groups])
    )

    delta <- sapply(list(M_est, M_low, M_high), function(M) {
      Mcol_to_delta(
        n_groups = n_groups,
        size = size,
        name = name,
        Mcol_observed = M
      ) %>%
        round(., digits = 2)
    },
    simplify = TRUE, USE.NAMES = TRUE)

    colnames(delta) <- c("est", "low", "high")

    return(delta)
  }




# Mt_to_delta_ci ----------------------------------------------------------
#' Calculate Delta Estimates and Confidence Intervals from Mixing Matrix at any timepoint.
#'
#' This function calculates the delta estimates and 95% confidence intervals
#' from mixing data based on binomial test.
#'
#' @param data The input data containing the observed infections (simulate_groups).
#' @param min_t The minimum time range for considering infections.
#' @param max_t The maximum time range for considering infections.
#' @param n_groups The number of groups in the mixing matrix.
#' @param size The total population size.
#' @param name The name of the groups.
#' @param r0 The vector of r0 values.
#'
#' @return A matrix containing the delta estimates, lower confidence intervals, and upper confidence intervals.

Mt_to_delta_ci <- function(data, min_t, max_t, n_groups, size, name, r0) {

  mixing <- get_mixing(data, min_t = min_t, max_t = max_t)$result
  total_n <- tapply(mixing$n, mixing$source_group, sum)
  prop_sus <- get_prop_susceptibles(data, t = max_t, n_groups, size, name)


  est <- vector(length = nrow(mixing))
  lower_ci <- vector(length = nrow(mixing))
  upper_ci <- vector(length = nrow(mixing))

  for (i in 1:nrow(mixing)) {
    binom_result <- stats::binom.test(mixing$n[i], total_n[mixing$source_group[i]])
    est[i] <- binom_result$estimate[[1]]
    lower_ci[i] <- binom_result$conf.int[1]
    upper_ci[i] <- binom_result$conf.int[2]
  }

  mix_binom <- cbind(mixing, est, lower_ci, upper_ci)

  # You might need to convert source_group to factor
  # to have the matrix in the right order
  mix_binom$source_group <- factor(mix_binom$source_group, levels = name)
  mix_binom <- mix_binom[order(mix_binom$source_group), ]

  M <- lapply(c( "est","lower_ci","upper_ci"),
         function(col){
           matrix(
             mix_binom[[col]] * rep(prop_sus * r0, each = n_groups),
             ncol = n_groups,
             nrow = n_groups,
             dimnames = list(name, name)
           )
         })

  delta <- sapply(M, function(Mat) {
    Mt_to_delta(
      n_groups = n_groups,
      size = size,
      name = name,
      r0 = r0,
      prop_susceptible = prop_sus,
      Mt_observed = Mat
    ) %>%
      round(., digits = 2)
  },
  simplify = TRUE, USE.NAMES = TRUE)

  colnames(delta) <- c("est", "low", "high")

  return(delta)
}
