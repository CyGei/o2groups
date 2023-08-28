
# Description ---------------------------------------------------------------------------------
# This script generates scenarios random changes to the parameters (under various constraints)
# It saves the scenarios in 2 formats: list and dataframe.

# Function -------------------------------------------------------------------------------------
# Helper Function
generate_scenarios_helper <- function() {
  duration <- 100

  n_groups <- round(truncnorm::rtruncnorm(n = 1, a = 1.5, b = 8.1, mean = 2.5, sd = 3))

  size <- round(runif(n_groups, min = 20, max = 200))

  name <- LETTERS[1:n_groups]

  # see delta_density.R
  delta <- actuar::rinvexp(n_groups, rate = 1, scale = 2/1)

  # 1 random introduction + sampling
  intro_n <- integer(n_groups)
  intro_n[sample(n_groups, 1)] <- 1
  for (i in seq_along(n_groups)){
    intro_n[i] <- intro_n[i] + round(runif(1, min = 0, max = 0.1 * size[i]))
  }

  r0_mean <- runif(1, min = 1, max = 5)
  # r0 <- round(rnorm(n_groups, mean = r0_mean, sd = 1), 2)
  r0_variation <- runif(n_groups, min = 0.7, max = 1.3)
  r0 <- round(r0_mean * r0_variation, 2)

  GT_mean <- truncnorm::rtruncnorm(1, a = 1, mean = 4, sd = 3)
  GT_sd <- truncnorm::rtruncnorm(1, a = 1, mean = 1.5, sd = 2)
  generation_time <- simulacr::make_disc_gamma(mean = GT_mean, sd = GT_sd)

  INCUB_mean <- truncnorm::rtruncnorm(1, a = 1, mean = 4, sd = 3)
  INCUB_sd <- truncnorm::rtruncnorm(1, a = 1, mean = 1.5, sd = 2)
  incubation_period <- simulacr::make_disc_gamma(mean = INCUB_mean, sd = INCUB_sd)

  scenario <- list(
    scenario = NULL,
    duration = duration,
    n_groups = n_groups,
    size = size,
    name = name,
    delta = delta,
    intro_n = intro_n,
    r0 = r0,
    GT_mean =  GT_mean,
    GT_sd = GT_sd,
    INCUB_mean = INCUB_mean,
    INCUB_sd = INCUB_sd,
    generation_time = generation_time,
    incubation_period = incubation_period
  )

  return(scenario)
}

# Main function
generate_scenarios <- function(n, seed = 123){
  set.seed(seed)
  # Create a list to store all scenarios
  scenarios <- vector("list", n)
  pseudonyms <- noah::pseudonymize(seq_len(n))


  for (i in 1:n) {
    scenario <- generate_scenarios_helper()
    scenario$scenario <- pseudonyms[i]
    scenarios[[i]] <- scenario
  }
  return(scenarios)
}




# Function Call -------------------------------------------------------------------------------
#
# scenarios <- generate_scenarios(10)


# Save Output ----------------------------------------------------------------------
# saveRDS(scenarios, "analysis/simulation/data/scenarios.rds")
