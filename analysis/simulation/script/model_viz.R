library(tidyverse)
library(here)
library(future)
library(furrr)
library(data.table)
plan(multisession, workers = future::availableCores()[[1]] - 2)

# Helpers -----------------------------------------------------------------
source(here("analysis/simulation/script/plot_helpers.R"))

# Data --------------------------------------------------------------------
scenarios_df <-
  fread(here("analysis/simulation/data/model", "scenarios_df.csv"))
# model_df is the most granular data on simulation level
model_df <-
  fread(here("analysis/simulation/data/model", "model_df.csv"))

# Calculate coverage, bias, and significance aggregated over simulations
outcomes <- model_df %>%
  group_by(scenario, peak_coeff, name) %>%
  summarise(
    coverage = sum(is_within_ci) / n(),
    bias = mean(bias),
    #significance = sum(significant_est) / n()
    significance = sum(significant_est == significant_delta) / n(), #significance should always be 1
    trials = mean(trials, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(scenarios_df, by = c("scenario", "name"))

predictors <-
  c("delta",
    "n_groups",
    "size",
    "intro_n",
    "r0",
    "GT_mean",
    "GT_sd",
    "INCUB_mean",
    "INCUB_sd",
    "trials")
metrics <- c("coverage", "bias", "significance")

# Pivot the dataframe into long format
outcomes_long <- outcomes %>%
  pivot_longer(cols = all_of(predictors),
               names_to = "predictor_name",
               values_to = "predictor_value") %>%
  pivot_longer(cols = all_of(metrics),
               names_to = "outcome_name",
               values_to = "outcome_value")

# PLOTS -------------------------------------------------------------------
#https://github.com/plotly/rasterly
outcomes_long %>%
  filter(predictor_name %in% c("delta", "size", "r0", "GT_mean", "trials")) %>%
  ggplot(aes(x = predictor_value , y = outcome_value)) +
  geom_point(alpha = 0.2) + #aes(col = as.factor(peak_coeff)),
  geom_smooth(se = FALSE)+
  facet_grid(outcome_name ~ predictor_name , scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom")


formula <- y~x
plot_scatter(model_df,
             x = "r0",
             y = "bias")+
  stat_smooth(method = "lm") +
  ggpubr::stat_regline_equation(
    aes(label =  paste(..eq.label.., "   ", ..adj.rr.label.., sep = "~")),
    col = "steelblue")

formula <- y ~ poly(x, 2, raw = TRUE)
plot_scatter(model_df,
             x = "size",
             y = "bias")+
  stat_smooth(method = "lm", formula = formula) +
  ggpubr::stat_regline_equation(
    aes(label =  paste(..eq.label.., "   ", ..adj.rr.label.., sep = "~")),
    formula = formula,
    col = "steelblue")


plot_heatmap(
  outcomes,
  "delta",
  "bias",
  x_breaks = seq(-1, 1, 0.1),
  y_breaks = seq(-2, 2, 0.1),
  bin = FALSE,
  min_alpha = 0.8
)




# Correlations ------------------------------------------------------------

corr <- outcomes %>%
  select(!c(scenario, name, peak_coeff)) %>%
  drop_na() %>%
  cor()

p.mat <- outcomes %>%
  select(!c(scenario, name, peak_coeff)) %>%
  drop_na() %>%
  ggcorrplot::cor_pmat()

ggcorrplot::ggcorrplot(
  corr = corr,
  type = "lower",
  lab = TRUE,
  hc.order = TRUE,
  p.mat = p.mat,
  method = "circle"
)

corrplot::corrplot(
  corr,
  type = "upper",
  order = "hclust",
  col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")
)
