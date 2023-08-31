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

normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
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
results_df <- readRDS("analysis/simulation/data/results.rds")



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
   # norm_bias = normalise(bias),
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


#saveRDS(model_df, "analysis/simulation/data/model_df.rds")




# VISU ----------------------------------------------------------------------------------------
#model_df <- readRDS("analysis/simulation/data/model_df.rds")

predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd")
plot_relationship <- function(predictor) {
  # Aggregate data by the dependent variable and calculate mean bias
  aggregated_data <- model_df %>%
    group_by(peak_coeff, .data[[predictor]]) %>%
    summarize(mean_bias = mean(bias))

  # Create a scatter plot
  ggplot(aggregated_data, aes(x = .data[[predictor]], y = mean_bias)) +
    geom_point(aes(col = peak_coeff)) +
    geom_hline(aes(yintercept = 0), col = "steelblue")+
    labs(x = predictor, y = "Mean Bias") +
    ggtitle(paste("Relationship between", predictor, "and Mean Bias"))
}

# plot all relationships using lapply
lapply(predictors, plot_relationship)
plot_relationship("r0")


library(corrplot)

correlation_matrix <- cor(model_df[, c("bias", predictors)])
corrplot(correlation_matrix, method = "color")




# Modelling -----------------------------------------------------------------------------------

predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd")
outcome <- "bias"
summary_df <- model_df %>%
  filter(as.numeric(scenario) <= 400) %>%
  group_by(scenario, simulation, peak_coeff) %>%
  summarise(across(all_of(c(predictors, outcome)), list(mean = ~mean(.), sd = ~sd(.)),
                   .names = "{.col}_{.fn}"))

# plot th relationship between bias and delta using density plots
ggplot(summary_df, aes(x = delta_mean, y = bias_mean)) +
  geom_point(aes(col = peak_coeff)) +
  geom_hline(aes(yintercept = 0), col = "steelblue")+
  labs(x = "Delta", y = "Bias") +
  ggtitle("Relationship between Delta and Bias")


ggplot(summary_df, aes(x = delta_mean, y = bias_mean)) +
  geom_density_2d(aes(fill = ..level..), alpha = 0.6) +
  geom_hline(yintercept = 0, col = "steelblue") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Delta", y = "Bias") +
  ggtitle("Relationship between Delta and Bias using Density Plots")


# Melting the summary_df for easier plotting
melted_summary <- summary_df %>%
  pivot_longer(cols = starts_with(predictors), names_to = "predictor", values_to = "value")

ggplot(melted_summary, aes(x = value, y = bias_mean )) +
  geom_violin() +
  facet_wrap(~predictor, scales = "free_x")+
  labs(x = "Predictor", y = "Bias") +
  ggtitle("Distribution of Bias by Predictor")


# Creating density distribution plots
ggplot(melted_summary, aes(x = value, y = ..density.., fill = predictor)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ predictor, scales = "free") +
  labs(x = "Value", y = "Density", fill = "Predictor") +
  ggtitle("Density Distribution of Bias by Predictor")



# MODELLING -------------------------------------------------------------------
#model_df <- readRDS("analysis/simulation/data/model_df.rds")
library(lme4)

model_formula <- "bias ~ size + delta + n_groups + (1 | scenario) + (1 | simulation)"
model <- lmer(model_formula, data = model_df)
model <- lmer(bias ~  delta  + n_groups + size + intro_n + r0 + GT_mean + GT_sd
              + (1 | scenario/simulation/peak_coeff),
              data = model_df)




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


