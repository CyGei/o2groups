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
    significance = sum(significant_est) / n()
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
    "trials")



# PLOTS -------------------------------------------------------------------
plot_scatter(model_df,
             x = "r0",
             y = "bias",
             facet_vars = "peak_coeff")
plot_scatter(model_df,
             x = "size",
             y = "bias",
             facet_vars = "peak_coeff")

ggplot(outcomes, aes(x = r0, y = bias, fill = ..level..)) +
  stat_density_2d(geom = "polygon") +
  scale_fill_viridis_c() +
  labs(x = "r0", y = "Bias")
ggplot(outcomes, aes(x = size, y = bias, fill = ..level..)) +
  stat_density_2d(geom = "polygon") +
  scale_fill_viridis_c() +
  labs(x = "r0", y = "Bias")

ggplot(model_df, aes(x = r0, y = bias)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c() +
  theme_minimal()



plot_heatmap(
  outcomes,
  "delta",
  "bias",
  x_breaks = seq(-1, 1, 0.1),
  y_breaks = seq(-2, 2, 0.1),
  bin = FALSE
)

plot_heatmap(
  model_df,
  "delta",
  "bias",
  x_breaks = seq(-1, 1, 0.1),
  y_breaks = seq(-2, 2, 0.1),
  facet_vars = "peak_coeff",
  bin = FALSE)+
  geom_hline(aes(yintercept = 0), col = "grey", lty = "dashed")


plot_heatmap(outcomes,
          "r0",
          "bias",
          x_length_out = 20,
          y_breaks = seq(-1, 1, 0.1))

plot_heatmap(outcomes,
             "size",
             "bias",
             y_breaks = seq(-1, 1, 0.05),
             x_length_out = 100)

plot_heatmap(
  outcomes,
  "n_groups",
  "bias",
  x_length_out = max(outcomes$n_groups, na.rm = TRUE),
  y_breaks = seq(-1, 1, 0.1),
  bin = FALSE
)

plot_heatmap(
  outcomes,
  "delta",
  "significance",
  x_breaks = seq(-1, 1, 0.05),
  y_breaks = seq(0,1, 0.1),
  facet_vars = "peak_coeff",
  bin = TRUE,
  min_alpha = 0.6)

plot_scatter(outcomes,"delta","significance",facet_vars = "peak_coeff")


# dbplot::dbplot_raster(outcomes,
#                       coverage,
#                       significance,
#                       fill = mean(delta),
#                       resolution = 50)


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
