get_delta_optim <- function(mixMat,
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

  result <- optim(
    par = rep(2, length = n_groups),
    fn = calculate_difference,
    method = "Nelder-Mead"
  )

  return(result$par)
}


# TEST --------------------------------------------------------------------

n_groups = 3
size = round(rnorm(n_groups, mean = 10000, sd = 10))
name = LETTERS[1:n_groups]
r0 = round(rnorm(n_groups, mean = 5, sd = 0.5))
prop_susceptible =  truncnorm::rtruncnorm(n_groups, a = 0.1, b = 0.99, mean = 0.5, sd = 0.3)
delta = round(truncnorm::rtruncnorm(n_groups, a = 1, mean = 4, sd = 4), digits = 3)

mixMat <- generate_Mcolt(
  n_groups = n_groups,
  size = size,
  name = name,
  r0 = r0,
  prop_susceptible = prop_susceptible,
  delta = delta
)


best_delta <- get_delta_optim(
  mixMat = mixMat,
  n_groups = n_groups,
  name = name,
  size = size,
  r0 = r0,
  prop_susceptible = prop_susceptible
)
best_delta
delta
