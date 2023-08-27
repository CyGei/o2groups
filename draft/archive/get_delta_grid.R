get_delta_grid <- function(delta_range,
                            coarse_precision,
                            fine_precision,
                            mixMat,
                            n_groups,
                            name,
                            size,
                            r0,
                            prop_susceptible) {

  calculate_difference <- function(delta) {
    generated <- generate_Mcolt(
      n_groups = n_groups,
      name = name,
      size = size,
      r0 = r0,
      prop_susceptible = prop_susceptible,
      delta = delta
    )
    diff_matrix <- abs(generated - mixMat)
    max_diff <- max(diff_matrix)
    return(max_diff)
  }

  # Coarse grid search
  coarse_delta_values <- seq(delta_range[1], delta_range[2], by = coarse_precision)
  coarse_combinations <- expand.grid(rep(list(coarse_delta_values), n_groups), stringsAsFactors = FALSE)
  coarse_min_diff <- Inf
  coarse_min_deltas <- NULL

  for (i in 1:nrow(coarse_combinations)) {
    coarse_deltas <- as.numeric(coarse_combinations[i, ])
    diff <- calculate_difference(delta = coarse_deltas)

    if (diff < coarse_min_diff) {
      coarse_min_diff <- diff
      coarse_min_deltas <- coarse_deltas
    }
  }

  # Fine grid search around the coarse minimum
  fine_delta_values <- seq(min(coarse_min_deltas) - coarse_precision, max(coarse_min_deltas) + coarse_precision, by = fine_precision)
  fine_combinations <- expand.grid(rep(list(fine_delta_values), n_groups), stringsAsFactors = FALSE)
  fine_min_diff <- Inf
  fine_min_deltas <- NULL

  for (j in 1:nrow(fine_combinations)) {
    fine_deltas <- as.numeric(fine_combinations[j, ])
    diff <- calculate_difference(delta = fine_deltas)

    if (diff < fine_min_diff) {
      fine_min_diff <- diff
      fine_min_deltas <- fine_deltas
    }
  }

  best_deltas <- fine_min_deltas
  return(best_deltas)
}


# TEST --------------------------------------------------------------------

delta_range <- c(1,20)
coarse_precision <- 1
fine_precision <- 0.1
n_groups = 2
size = round(rnorm(n_groups, mean = 10000, sd = 10))
name = LETTERS[1:n_groups]
r0 = round(rnorm(n_groups, mean = 5, sd = 0.5))
prop_susceptible =  truncnorm::rtruncnorm(n_groups, a = 0.1, b = 0.99, mean = 0.5, sd = 0.3)
delta = c(11,2.21)

mixMat <- generate_Mcolt(
  n_groups = n_groups,
  size = size,
  name = name,
  r0 = r0,
  prop_susceptible = prop_susceptible,
  delta = delta
)
mixMat

get_delta_grid(
  delta_range = delta_range,
  coarse_precision = coarse_precision,
  fine_precision = fine_precision,
  mixMat = mixMat,
  n_groups = n_groups,
  name = name,
  size = size,
  r0 = r0,
  prop_susceptible = prop_susceptible
)
