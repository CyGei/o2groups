# Description -------------------------------------------------------------

# The aim of this script is to retrieve Delta in a "real world" scenario.
#
# We will use the symptom onset dates from the o2groups simulations
# as inputs to outbreaker2 to reconstruct transmission chains.
# From the reconstructed chains we will estimate Ri and obtain the mixing %
# over time by groups.
# Early Ri will correspond to r0.

# To estimate delta we will at least need:
# population size & r0 -> early stage (Mcol_to_delta)
# population size & r0 & % of susceptible -> any time point (get_delta_optim)


library(tictoc)
library(tidyverse)
library(o2groups)

# Parameters --------------------------------------------------------------

duration = 100
n_groups = 2
size = c(100, 100) #c(300, 300)
name = c("A", "B")
delta = c(10, 2)
intro_group = c("A", "B")
intro_n = c(1, 1)
r0 = c(3,3)
generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)
incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)




# Simulation --------------------------------------------------------------
set.seed(123)
sim <-
  simulate_groups(
    # n_simulations = 10,
    # n_cores = n_cores,
    duration = duration,
    n_groups = n_groups,
    size = size,
    name = name,
    delta = delta,
    intro_group = intro_group,
    intro_n = intro_n,
    r0 = r0 ,
    generation_time = generation_time$d(1:30)
  )
data = sim$data
stats = sim$stats
sequences <- generate_sequences(data,
                                genome_length = 1000,
                                mutation_rate = 0.01)

data["date_onset"] <- data["date_infection"] + incubation_period$r(nrow(data))
data <- data %>% simulacr::get_si() %>% simulacr::get_gt() %>% get_Ri()



# Basic Plots -------------------------------------------------------------
pal = c("A" = "magenta", "B" = "orange")
gg_col <- list(
  scale_color_manual(values = pal),
  scale_fill_manual(values = pal)
)
gg_scale <- list(scale_x_continuous(breaks = seq(0,100,5)))


plot_tree(data, pal = pal)
plot_stats(stats)[[1]] + gg_col + scale_x_continuous(breaks = seq(0,100, 10), limits = c(0,45))
plot_stats(stats)[[2]] + gg_col  + scale_x_continuous(breaks = seq(0,100, 10), limits = c(0,55))



# Ri ----------------------------------------------------------------------

o2groups:::plot_Ri(
  o2groups:::Ri_data(data, name)$Ri_group,
  o2groups:::Ri_data(data, name)$Ri_facet
  )


# MIXING ------------------------------------------------------------------
source("R/plot_mix_window.R")
window_length = 2
plot_mix_window(mix_window_data =
                  mix_window_data(data, window_length),
                window_length)


# DELTA ------------------------------------------------------------------
# treshold at 80% susceptible
t <- 5
values <- get_prop_susceptibles(data, t, n_groups, size, name)
while (values[1] > 0.80 || values[2] > 0.80) {
  if (values[1] > 0.80) {
    t1 <- t
  }

  if (values[2] > 0.80) {
    t2 <- t
  }

  t <- t + 1
  values <- get_prop_susceptibles(data, t, n_groups, size, name)
}
t1;t2

get_prop_susceptibles(data, t1+1, n_groups, size, name)
get_prop_susceptibles(data, t2, n_groups, size, name)


## early phase:
early_delta(
  data = data,
  min_t = 0,
  max_t = t1+1,
  size = size,
  name = name
)["A",]

early_delta(
  data = data,
  min_t = 0,
  max_t = t2,
  size = size,
  name = name
)["B",]




timepoints <- 1:100
output <- matrix(NA, nrow = length(timepoints), ncol = n_groups)

for (tp in timepoints) {
  Mcolt_observed <- get_mixing(data, min_t = tp, max_t = tp)$Mcol

  prop_sus <- stats %>%
    filter(time == tp) %>%
    group_by(group) %>%
    summarise(prop_susceptible = mean(prop_susceptible)) %>%
    pull(prop_susceptible)

  delta_optim <- Mcolt_to_delta_optim(
    n_groups = n_groups,
    size = size,
    name = name,
    r0 = r0,
    prop_susceptible = prop_sus,
    Mcolt_observed = Mcolt_observed
  )

  output[tp, ] <- delta_optim

}
colnames(output) <- name

output %>%
  as_tibble() %>%
  mutate(timepoint = row_number()) %>%
  filter(timepoint <=50 ) %>%
  pivot_longer(cols = -timepoint, names_to = "group", values_to = "delta") %>%
  filter(delta <= 20) %>%
  ggplot(aes(x = timepoint, y = delta, col = group))+
  geom_point()+
  geom_line()+
  geom_hline(data = tibble(group = LETTERS[1:n_groups],
                           delta = delta),
             aes(yintercept = delta, col = group))+
  geom_hline(aes(yintercept = 1), col = "darkgray", lty = "dashed")+
  gg_scale+
  scale_y_continuous(breaks = seq(0,100,1))+
  theme_bw()

## SEE draft/CG_binom
