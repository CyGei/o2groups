future::plan("multisession", workers = future::availableCores()[[1]] - 2)
# Set up parameters for different scenarios
scenarios <- list(
  list(
    scenario = "Null",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(1, 1),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Base",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Group Size v1",
    n_groups = 2,
    size = c(1e4, 1e4),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Group Size v2",
    n_groups = 2,
    size = c(1e4, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Group Size v3",
    n_groups = 2,
    size = c(100, 1e4),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "R0 v1",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(6, 6),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "R0 v2",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(6, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "R0 v3",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 6),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Generation Time v1",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 1, sd = 1),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Generation Time v2",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 10, sd = 1),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "Generation Time v3",
    n_groups = 2,
    size = c(100, 100),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(1, 1),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 10),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "n_groups v1",
    n_groups = 4,
    size = rep(100, 4),
    name = LETTERS[1:4],
    delta = c(10, 2, 1, 1) ,
    intro_group = LETTERS[1:4],
    intro_n = rep(1, 4),
    r0 = rep(3, 4),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  ),
  list(
    scenario = "n_groups v2",
    n_groups = 20,
    size = rep(100, 20),
    name = LETTERS[1:20],
    delta = c(10, 2, c(rep(1, 18))) ,
    intro_group = LETTERS[1:20],
    intro_n = rep(1, 20),
    r0 = rep(3, 20),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2),
    incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
  )
)

# Function to run simulations for a given scenario
source("R/run_sims.R")
run_scenario <- function(scenario) {
  set.seed(123)

  # Run simulations for the scenario
  s <- run_sims(
    n_simulations = 100,
    n_groups = scenario$n_groups,
    size = scenario$size,
    name = scenario$name,
    delta = scenario$delta,
    intro_group = scenario$intro_group,
    intro_n = scenario$intro_n,
    r0 = scenario$r0,
    generation_time = scenario$generation_time,
    incubation_period = scenario$incubation_period,
    peak_coeff = 0.8
  )

  return(s)
}

# Run simulations for all scenarios and store the results
runtime <- system.time({
  results <- lapply(scenarios, function(scenario) {
    s <- run_scenario(scenario)
    s$scenario <- scenario$scenario
    return(s)
  })
})

# saveRDS(scenarios, "draft/scenario_params.RData")
# saveRDS(results, "draft/scenario_runs.RData")
results <- readRDS("draft/scenario_runs.RData") %>% bind_rows()
scenarios <- readRDS("draft/scenario_params.RData")
source("draft/extract_scenario.R")
scenarios_df <- extract_scenario(scenarios)



# tranform delta ------------------------------------------------------------------------------
# A transformed coefficient of 1 indicates perfect assortativity.
# A transformed coefficient of -1 indicates perfect disassortativity.
# A transformed coefficient of 0 indicates no assortativity (neither assortative nor disassortative).
std_delta <- function(delta) {
  delta <- as.numeric(delta)
  d <- ifelse(is.finite(delta),
              (delta - 1) / (delta + 1),
              1.0)
  return(d)
}

log_delta <- function(delta) {
  delta <- as.numeric(delta)
  d <- log(delta)
  return(d)
}


results <-  merge(results,
                  scenarios_df, by = c("scenario", "group")) %>%
  mutate(
    across(
      .cols = c("delta", "est", "lower_ci", "upper_ci"),
      .names = "std_{.col}",
      .fns = std_delta
    ),
    across(
      .cols = c("delta", "est", "lower_ci", "upper_ci"),
      .names = "log_{.col}",
      .fns = log_delta
    ),
    delta = as.numeric(delta),
    raw_bias = delta - est,
    log_bias = log_delta - log_est,
    std_bias = std_delta - std_est,
    std_is_within_ci = ifelse(std_delta >= std_lower_ci &
                                std_delta <= std_upper_ci,
                              TRUE,
                              FALSE),
    std_delta_outside_0 = ifelse(std_delta != 0,
                                 TRUE,
                                 FALSE),
    std_est_outisde_0 = ifelse(std_lower_ci > 0 |
                                  std_upper_ci < 0,
                                TRUE,
                                FALSE)
  )

t.test(results$std_bias, mu = 0)$p.value


# Plots ---------------------------------------------------------------------------------------
# Plot mean estimates vs true value with CIs
# Plot coverage rate (prop_within_ci)
# Plot sensitivity and specificity
# Plot bias (absolute bias and directional bias)
# mean estimates ----------------------------------------------------------
results %>%
  drop_na(est) %>%
  group_by(scenario, group) %>%
  summarise(across(.cols = starts_with("std_"),
                   .fns = ~ mean(.))) %>%
  ggplot(aes(x = group, y = std_est, col = group)) +
  facet_wrap(~ scenario, scales = "free_x") +

  # estimate
  geom_point(position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = std_lower_ci, ymax = std_upper_ci),
    width = 0.2,
    position = position_dodge(0.9)
  ) +

  # true delta
  geom_point(
    aes(y = std_delta),
    color = "black",
    position = position_dodge(0.9),
    alpha = 0.7,
    show.legend = FALSE
  ) +

  #hline at 0
  geom_hline(aes(yintercept = 0), lty = "dotted") +

  labs(x = "Scenario", y = "Estimate") +
  scale_fill_discrete(name = "Group") +
  theme_minimal() +
  theme(legend.position = "none")



# bias ----------------------------------------------------------------------------------------

# Histogram of bias
ggplot(results, aes(x = std_bias, fill = scenario)) +
  geom_histogram(binwidth = 0.05) +
  theme_minimal()+
  theme(legend.position = "none")

ggplot(results,
       aes(
         x = group,
         y = std_bias,
         group = interaction(scenario, group)
       )) +
  facet_wrap( ~ scenario, scales = "free") +
  geom_violin() +
  # geom_point(aes(col = group),
  #            position = position_jitterdodge(jitter.width = 0.2,
  #                                            dodge.width = 0.75),
  #            alpha = 0.1) +
  ggforce::geom_sina(aes(col = group), alpha = 0.4) +

  geom_hline(yintercept = 0, lty = "dotted") +
  theme_minimal() +
  theme(legend.position = "none")



# coverage ----------------------------------------------------------------
results %>%
  group_by(scenario, group) %>%
  summarise(
    est = binom.test(x = sum(std_is_within_ci), n = n())$est[[1]],
    lower =  binom.test(x = sum(std_is_within_ci), n = n())$conf.int[[1]],
    upper = binom.test(x = sum(std_is_within_ci), n = n())$conf.int[[2]]
  ) %>%
  ggplot(aes(x = group, y = est,
             color = group)) +
  facet_wrap(~ scenario, scales = "free") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0.2) +
  geom_hline(yintercept = 0.95,
             linetype = "dashed",
             color = "red") +
  labs(x = "", y = "Proportion within CI") +
  scale_color_discrete(name = "Group") +
  theme_minimal() +
  theme(legend.position = "none")


# singificance --------------------------------------------------------------------------------

results %>%
  group_by(scenario, group) %>%
  summarise(
    est = binom.test(x = sum(std_est_outisde_0), n = n())$est[[1]],
    lower =  binom.test(x = sum(std_est_outisde_0), n = n())$conf.int[[1]],
    upper = binom.test(x = sum(std_est_outisde_0), n = n())$conf.int[[2]]
  ) %>%
  ggplot(aes(x = group, y = est,
             color = group)) +
  facet_wrap(~ scenario, scales = "free_x") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                width = 0.2) +
  labs(x = "", y = "proportion where significance is detected") +
  scale_color_discrete(name = "Group") +
  theme_minimal() +
  theme(legend.position = "none")


# roc -----------------------------------------------------------------------------------------
results %>%
  select(std_delta_outside_0, std_est_outisde_0) %>%
  summarise(
    true_positive = sum(std_delta_outside_0 == TRUE &
                          std_est_outisde_0 == TRUE),
    true_negative = sum(std_delta_outside_0 == FALSE &
                          std_est_outisde_0 == FALSE),
    TPR = true_positive / sum(std_delta_outside_0 == TRUE),
    TNR = true_negative / sum(std_delta_outside_0 == FALSE),
    PPV = true_positive / sum(std_est_outisde_0 == TRUE),
    NPV = true_negative / sum(std_est_outisde_0 == FALSE)
  )


