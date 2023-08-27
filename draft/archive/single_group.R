# Source all the R files
R_dir <- "./R/"
files <- list.files(R_dir, pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

tictoc::tic()
out <- simulate_groups_furrr(
  n_simulations = 100,
  n_cores = 5,
  duration = 100,
  n_groups = 1,
  size = 1000,
  name = "A",
  delta = 1,
  intro_group = "A",
  intro_n = 10,
  r0 = 4,
  generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)$d(1:30)
)
tictoc::toc()
data = out$data
stats = out$stats

# VIZ ---------------------------------------------------------------------
library(tidyverse)
pal = c("A" = "magenta")
gg_col <- list(
  scale_color_manual(values = pal),
  scale_fill_manual(values = pal)
)
gg_scale <- list(scale_x_continuous(breaks = seq(0,100,5)))


#plot_tree(data, pal = pal)
plot_stats(stats)[[1]] + gg_col + gg_scale
plot_stats(stats)[[2]] + gg_col  + gg_scale




# AR2R0 -------------------------------------------------------------------

ar <- stats %>%
  filter(time == max(time)) %>%
  group_by(simulation) %>%
  summarise(attack_rate = sum(n_infected) / size) %>%
  pull(attack_rate) %>% mean()
epitrix::AR2R0(ar)



# EpiEstim ----------------------------------------------------------------
library(EpiEstim)
incid <-
  incidence::incidence(data$date_infection, groups = data$simulation)
plot(incid)
Restim <- estimate_R(data$date_infection,
                     method = "parametric_si",
                     config = make_config(list(mean_si = 5,
                                               std_si = 2)))
plot(Restim)

