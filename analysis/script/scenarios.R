# List of scenarios considered in the analysis.
# Note: dissorative coeff = 1/assortative coeff

scenarios <- list(
  # Null Scenario : Neutral Mixing
  list(
    scenario = "Null",
    param = "Reference",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = rep(1, 3),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),

  # Base Scenario : Assortative Mixing, Neutral Mixing, Disassortative Mixing
  list(
    scenario = "Base",
    param = "Reference",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),

  # Group Size Variations: Large Groups, Small Groups, Different Sizes
  list(
    scenario = "Large Groups",
    param = "group_size",
    n_groups = 3,
    size = rep(1e4, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Small Groups",
    param = "group_size",
    n_groups = 3,
    size = rep(20, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Different Sizes 1",
    param = "group_size",
    n_groups = 3,
    size = c(100, 50, 100),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Different Sizes 2",
    param = "group_size",
    n_groups = 3,
    size = c(1e3, 50, 100),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),

  # Reproduction Number Variations: Low R0, High R0, Different R0
  list(
    scenario = "Low R0",
    param = "r0",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(1.5, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "High R0",
    param = "r0",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(3.5, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Different R0 1",
    param = "r0",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = c(3.5, 2, 3.5),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Different R0 2",
    param = "r0",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = c(1.5, 2, 3.5),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Different R0 3",
    param = "r0",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = c(3.5, 2, 1.5),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),

  # Generation Time Variations: Short Generation Time, Long Generation Time, Uncertain Generation Time.
  list(
    scenario = "Long Generation Time",
    param = "generation_time",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 10, sd = 1),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Short Generation Time",
    param = "generation_time",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 2, sd = 1),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Uncertain Generation Time",
    param = "generation_time",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = rep(1, 3),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 10),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),

  # Introduction Variations: Single Introduction, Different Introduction Sizes
  list(
    scenario = "Single Introduction Group A",
    param = "intro_group",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1],
    intro_n = 1,
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Single Introduction Group B",
    param = "intro_group",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[2],
    intro_n = 1,
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Single Introduction Group C",
    param = "intro_group",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[3],
    intro_n = 1,
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Multiple Introduction Sizes",
    param = "intro_n",
    n_groups = 3,
    size = rep(100, 3),
    name = LETTERS[1:3],
    delta = c(10, 1, 1 / 10),
    intro_group = LETTERS[1:3],
    intro_n = c(15, 1, 15),
    r0 = rep(2, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),

  # Group Number Variations:
  list(
    scenario = "Base & 7 neutral groups",
    param = "n_groups",
    n_groups = 10,
    size = rep(100, 10),
    name = LETTERS[1:10],
    delta = c(10, 1, 1 / 10, rep(1, 7)),
    intro_group = LETTERS[1:10],
    intro_n = rep(1, 10),
    r0 = rep(2, 10),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Base & 3 mixed groups",
    param = "n_groups",
    n_groups = 6,
    size = c(rep(100, 3), 50, 150, 200),
    name = LETTERS[1:6],
    delta = c(10, 1, 1 / 10, 1.5, 3, 1 / 1.7),
    intro_group = LETTERS[1:6],
    intro_n = rep(1, 6),
    r0 = rep(2, 6),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  )
)
saveRDS(scenarios, file = "analysis/data/scenarios.rds")
