library(tidyverse)
library(o2groups)

n_groups <- 2
size <- c(10000, 10000)
name <- c("A", "B")
r0 <- c(3, 3)
prop_susceptible <- c(0.26, 0.62)
true_delta = c(10, 2)
Mcolt_observed <-
  generate_Mcolt(
    n_groups = n_groups,
    name = name,
    size = size,
    r0 = r0,
    prop_susceptible = prop_susceptible,
    delta = true_delta
  )


# Function to calculate the difference between the generated matrix and the observed matrix
calculate_difference <- function(delta,
                                 mixMat,
                                 n_groups,
                                 name,
                                 size,
                                 r0,
                                 prop_susceptible) {
  generated <-
    generate_Mcolt(
      n_groups = n_groups,
      name = name,
      size = size,
      r0 = r0,
      prop_susceptible = prop_susceptible,
      delta = delta
    )
  desired <-
    mixMat

  # Calculate the absolute difference between corresponding elements of the matrices
  diff_matrix <- abs(generated - desired)

  # Calculate the maximum absolute difference between any pair of corresponding elements
  max_diff <- max(diff_matrix)

  return(max_diff)
}
#
# ## Iterate through different values of delta for each group
# ## and find the ones that minimize the difference
# # Create a matrix to store the results
# results_matrix <- matrix(NA, nrow = 15, ncol = 15)
# best_delta <- rep(NA, n_groups)
# min_diff <- Inf
# # Loop through all possible combinations of delta
# for (delta_A in seq(1, 15, length.out = 15)) {
#   for (delta_B in seq(1, 15, length.out = 15)) {
#     delta <- c(delta_A, delta_B)
#
#     # Calculate the difference for the current combination of delta values
#     diff <- calculate_difference(delta, mixMat = Mcolt_observed,
#                                  n_groups = n_groups,
#                                  name = name,
#                                  size = size,
#                                  r0 = r0,
#                                  prop_susceptible = prop_susceptible)
#
#     # Store the difference in the results matrix
#     results_matrix[delta_A, delta_B] <- diff
#
#   }
# }
# # Plot the heatmap
# heatmap(results_matrix, xlab = "Delta B", ylab = "Delta A", main = "Difference Heatmap")






# FUNCTIONIZE -------------------------------------------------------------
get_delta_optim <- function(delta_range,
                            precision,
                            mixMat,
                            n_groups,
                            name,
                            size,
                            r0,
                            prop_susceptible) {

  # Generate the delta combinations using expand.grid
  delta_values <- seq(delta_range[1], delta_range[2], by = precision)
  delta_combinations <- expand.grid(rep(list(delta_values), n_groups), stringsAsFactors = FALSE)

  # Create a vector to store the results
  results <- vector("numeric", nrow(delta_combinations))

  # Loop through each delta combination
  for (i in 1:nrow(delta_combinations)) {
    delta <- as.numeric(delta_combinations[i, ])

    # Calculate the difference for the current combination of delta values
    diff <- calculate_difference(delta = delta,
                                 mixMat = mixMat,
                                 n_groups = n_groups,
                                 name = name,
                                 size = size,
                                 r0 = r0,
                                 prop_susceptible = prop_susceptible)

    # Store the difference in the results vector
    results[i] <- diff
  }

  # Find the index of the minimum difference
  min_index <- which.min(results)

  # Get the corresponding delta values
  best_delta <- delta_combinations[min_index, ]

  return(best_delta)
}

delta_range = c(2,20)
precision = 0.1
mixMat = Mcolt_observed


best_delta <- get_delta_optim(delta_range = delta_range,
                              precision = precision,
                              mixMat = Mcolt_observed,
                              n_groups = n_groups,
                              name = name,
                              size = size,
                              r0 = r0,
                              prop_susceptible = prop_susceptible)
#
# print(best_delta)
