library(tidyverse)
library(here)
library(future)
library(furrr)
library(data.table)
plan(multisession, workers = future::availableCores()[[1]] - 2)

# Helpers -----------------------------------------------------------------
plot_relationship <- function(predictor) {
  # Aggregate data by the dependent variable and calculate mean bias
  aggregated_data <- model_df %>%
    group_by(peak_coeff, .data[[predictor]]) %>%
    summarise(mean_bias = mean(bias))

  # Create a scatter plot
  ggplot(aggregated_data, aes(x = .data[[predictor]], y = mean_bias)) +
    geom_point(aes(col = as.character(peak_coeff)), alpha = 0.6) +
    geom_hline(aes(yintercept = 0), col = "steelblue") +
    labs(x = predictor, y = "Mean Bias", col = "Peak Coeff") +
    ggtitle(paste("Relationship between", predictor, "and Mean Bias"))
}

plot_freq <- function(x,
                      y,
                      x_breaks = NULL,
                      y_breaks = NULL,
                      x_length_out = 10,
                      y_length_out = 10) {
  if (is.null(x_breaks)) {
    x_breaks <-
      seq(min(outcomes[[x]], na.rm = TRUE),
          max(outcomes[[x]], na.rm = TRUE),
          length.out = x_length_out)
  }
  if (is.null(y_breaks)) {
    y_breaks <-
      seq(min(outcomes[[y]], na.rm = TRUE),
          max(outcomes[[y]], na.rm = TRUE),
          length.out = y_length_out)
  }


  plot <- outcomes %>%
    select(x, y) %>%
    mutate(
      x_bin = cut(.data[[x]], breaks = x_breaks),
      y_bin = cut(.data[[y]], breaks = y_breaks)
    ) %>%
    drop_na() %>%
    count(x_bin, y_bin) %>%
    group_by(x_bin) %>%
    mutate(denom = sum(n),
           freq = n / denom) %>%
    ggplot(., aes(x = x_bin, y = y_bin)) +
    geom_tile(aes(fill = freq),
              col = "white") +
    geom_label(aes(
      x = x_bin,
      y = -1,
      label = scales::comma_format()(denom)
    ),
    vjust = -0.5) +
    scale_fill_viridis_c() +
    labs(x = paste(x, "Bin"),
         y = paste(y, "Bin"),
         fill = "Frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(plot)
}

# Data --------------------------------------------------------------------
scenarios_df <- fread(here("analysis/simulation/data/model", "scenarios_df.csv"))
# model_df is the most granular data on simulation level
model_df <- fread(here("analysis/simulation/data/model", "model_df.csv"))

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

predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd", "trials")



# PLOTS -------------------------------------------------------------------
plots <- predictors %>%
  set_names() %>%
  map(plot_relationship)

ggplot(outcomes, aes(x = r0, y = bias)) +
  geom_point() +
  theme_minimal()

ggplot(outcomes, aes(x = r0, y = bias, fill = ..level..)) +
  stat_density_2d(geom = "polygon") +
  scale_fill_viridis_c() +
  labs(x = "r0", y = "Bias")

ggplot(model_df, aes(x = r0, y = bias)) +
  geom_hex(bins = 50) +
  facet_wrap(~peak_coeff) +
  scale_fill_viridis_c() +
  theme_minimal()

plot_freq("delta", "bias", x_breaks = seq(-1,1, 0.1), y_breaks = seq(-2, 2, 0.1))
plot_freq("r0", "bias",x_length_out = 20, y_breaks = seq(-1, 1, 0.1))
plot_freq("size", "bias", y_breaks = seq(-1, 1, 0.05), x_length_out = 80)
plot_freq("n_groups", "bias", x_breaks = seq(1, 10, 1), y_breaks = seq(-1, 1, 0.1))
plot_freq("significance", "bias", y_breaks = seq(-1, 1, 0.1), x_length_out = 20)
plot_freq("coverage", "bias", x_breaks = seq(-1, 1, 0.1), y_breaks = seq(0.9, 1, 0.01))
dbplot::dbplot_raster(outcomes, coverage, significance, fill = mean(delta), resolution = 50)


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

corrplot::corrplot(corr,
  type = "upper", order = "hclust",
  col = RColorBrewer::brewer.pal(n = 8, name = "RdYlBu")
)
