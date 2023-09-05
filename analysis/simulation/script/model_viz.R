library(tidyverse)
library(here)
library(future)
library(furrr)#
library(data.table)
plan(multisession, workers = future::availableCores()[[1]] - 2)

# Helpers -----------------------------------------------------------------
#for model_df
plot_relationship <- function(predictor) {
  # Aggregate data by the dependent variable and calculate mean bias
  aggregated_data <- model_df %>%
    group_by(peak_coeff, .data[[predictor]]) %>%
    summarise(mean_bias = mean(bias))

  # Create a scatter plot
  ggplot(aggregated_data, aes(x = .data[[predictor]], y = mean_bias)) +
    geom_point(aes(col = as.character(peak_coeff)), alpha = 0.6) +
    geom_hline(aes(yintercept = 0), col = "steelblue")+
    labs(x = predictor, y = "Mean Bias", col = "Peak Coeff") +
    ggtitle(paste("Relationship between", predictor, "and Mean Bias"))
}


# Data --------------------------------------------------------------------
model_df <- fread(here("analysis/simulation/data/model", "model_df.csv"))

coverage <-  model_df %>%
  group_by(scenario, peak_coeff, name) %>%
  summarise(is_within_ci = sum(is_within_ci),
            n = n()) %>%
  mutate(coverage = is_within_ci / n) %>%
  select(scenario, peak_coeff, name, coverage)

bias <-
  model_df %>%
  group_by(scenario, peak_coeff, name) %>%
  summarise(bias = mean(bias)) %>%
  select(scenario, peak_coeff, name, bias)

significance <- model_df %>%
  group_by(scenario, peak_coeff, name) %>%
  summarise(significant_est = sum(significant_est),
            n = n()) %>%
  mutate(significance = significant_est / n) %>%
  select(scenario, peak_coeff, name, significance)

outcomes <- coverage %>%
  left_join(bias, by = c("scenario", "peak_coeff", "name")) %>%
  left_join(significance, by = c("scenario", "peak_coeff", "name")) %>%
  left_join(scenarios, by = c("scenario", "name")) %>%
  ungroup()


predictors <- c("delta", "n_groups", "size", "intro_n", "r0", "GT_mean", "GT_sd", "trials")



# PLOTS -------------------------------------------------------------------
plot_relationship("r0")
plot_relationship("size")
plots <- lapply(predictors, plot_relationship)


# Create a hexbin plot
ggplot(model_df, aes(x = r0, y = bias)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c() +
  theme_minimal()



ggplot(model_df, aes(x = r0, y = bias)) +
  geom_hex(bins = 50) +
  facet_wrap(~peak_coeff)+
  scale_fill_viridis_c() +
  theme_classic()


ggplot(outcomes, aes(x = delta , y = significance))+
  geom_hex(bins = 50) +
  facet_wrap(~peak_coeff)+
  scale_fill_viridis_c() +
  theme_classic()

outcomes[!complete.cases(outcomes), ]

model_df %>%
  filter(scenario == "filthy_xerus_20230903174722")

# correlation between outcomes
corr<- outcomes %>%
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
  p.mat = p.mat
)

corrplot(corr, type="upper", order="hclust",
         col= brewer.pal(n=8, name="RdYlBu"))


