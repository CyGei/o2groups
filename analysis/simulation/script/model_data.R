library(tidyverse)
standardise <- function(x) {
  x <- as.numeric(x)
  d <- ifelse(is.finite(x),
              (x - 1) / (x + 1),
              1.0)
  return(d)
}

reverse_standardise <- function(d) {
  x <- (1 + d) / (1 - d)
  return(x)
}

# Scenarios ------------------------------------------------------------------
scenarios <- readRDS("analysis/simulation/data/scenarios.rds")

remove_obj <- function(scenario) {
  scenario$generation_time <- NULL
  scenario$incubation_period <- NULL
  return(scenario)
}

scenarios_df <- lapply(scenarios, remove_obj) %>%
  tibble(scenarios = .) %>%
  unnest_wider(col = scenarios) %>%
  group_by(scenario) %>%
  unnest(cols = c(name, size, delta, intro_n, r0)) %>%
  ungroup() %>%
  rename(group = name)


# Raw results -------------------------------------------------------------
results <- readRDS("analysis/simulation/data/results.rds")
results_df <- results %>%
  bind_rows()



# Model Df ---------------------------------------------------------------
model_df <-
  merge(results_df,
        scenarios_df,
        by = c("scenario", "group"),
        all.x = TRUE) %>%
  arrange(as.integer(scenario), as.integer(simulation)) %>%
  select(
    scenario,
    simulation,
    peak_coeff,
    group,
    n_groups,
    size,
    intro_n,
    r0,
    GT_mean,
    GT_sd,
    INCUB_mean,
    INCUB_sd,
    duration,
    est,
    lower_ci,
    upper_ci,
    successes,
    trials,
    delta
  ) %>%
  mutate(
    scenario = as.factor(scenario),
    simulation = as.factor(simulation),
    peak_coeff = as.factor(peak_coeff),
    group = as.factor(group),
    across(
      .cols = c("delta", "est", "lower_ci", "upper_ci"),
      .names = "{.col}",
      .fns = standardise
    ),
    bias = delta - est,
    is_within_ci = ifelse(delta >= lower_ci &
                            delta <= upper_ci,
                          TRUE,
                          FALSE),
    delta_outside_0 = ifelse(delta != 0,
                             TRUE,
                             FALSE),
    est_outisde_0 = ifelse(lower_ci > 0 | upper_ci < 0,
                           TRUE,
                           FALSE)
  )


saveRDS(model_df, "analysis/simulation/data/model_df.rds")




# VISU ----------------------------------------------------------------------------------------
model_df <- readRDS("analysis/simulation/data/model_df.rds")


model_df %>%
  ggplot(aes(x = size, y = bias, col = group))+
  facet_wrap(~scenario)+
  geom_point()+
  geom_hline(yintercept = 0)

model_df %>%
  ggplot(aes(x = size, y = bias, col = group))+
  facet_wrap(~scenario)+
  geom_point()+
  geom_hline(yintercept = 0)

model_df %>%
  ggplot(aes(x = delta, y = bias, col = group))+
  facet_wrap(~scenario)+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)


# MODELLING -------------------------------------------------------------------
model_df <- readRDS("analysis/simulation/data/model_df.rds")
library(lme4)
# Define the multilevel regression model
model <- lmer(bias ~  delta + delta_outside_0 + n_groups + size + intro_n + r0 + GT_mean + GT_sd  + (1 | scenario/simulation/peak_coeff), data = model_df)

# Fit the model
fit <- summary(model)

# View the model summary
print(fit)



library(randomForest)
# Convert factors to numeric for random forest
model_df$scenario <- as.numeric(model_df$scenario)
model_df$simulation <- as.numeric(model_df$simulation)
model_df$peak_coeff <- as.numeric(model_df$peak_coeff)
model_df$group <- as.numeric(model_df$group)

# Define the Random Forest model
rf_model <- randomForest(bias ~ delta + n_groups + size + intro_n + r0 + GT_mean + GT_sd, data = model_df)

# View the model summary
print(rf_model)
summary(rf_model)

# Feature importance plot
varImpPlot(rf_model, main = "Variable Importance Plot")


