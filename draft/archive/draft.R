# Source all the R files 
R_dir <- "./R/"
files <- list.files(R_dir, pattern = "\\.R$", full.names = TRUE)
lapply(files, source)

library(tictoc)
tic()
set.seed(123)
out <-
  simulate_groups(
    duration = 100,
    n_groups = 2,
    size = c(10000, 10000),
    name = c("A", "B"),
    delta = c(10, 2),
    intro_group = c("A", "B"),
    intro_n = c(5, 5),
    r0 = c(3, 3),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)$d(1:30)
  )
toc()

data = out$data
stats = out$stats
inputs = out$inputs


# VIZ ---------------------------------------------------------------------
library(tidyverse)
pal = c("A" = "magenta", "B" = "orange")
gg_col <- list(
  scale_color_manual(values = pal),
  scale_fill_manual(values = pal)
)
gg_scale <- list(scale_x_continuous(breaks = seq(0,100,5)))


plot_tree(data, pal = pal)
plot_stats(stats)[[1]] + gg_col + gg_scale
plot_stats(stats)[[2]] + gg_col  + gg_scale


# MIXING ---------------------------------------------------------------------
mix_t <- map(.x = unique(data$date_infection),
             function(.x){
              mix <- data %>% get_mixing(., min_t = .x, max_t = .x)
              return(mix$result)
             }) %>% 
  bind_rows(., .id = "t") %>% 
  mutate(t = as.integer(t))
head(mix_t)

ggplot(mix_t) +
  aes(x = t,
      y = freq ,
      col = source_group) +
  facet_wrap(source_group ~ group)+
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = 20), lty = "dashed") +
  gg_col +
  gg_scale


# DELTA -------------------------------------------------------------------
Mcol <- get_mixing(data, max_t = 20)$Mcol
Mcol_to_delta(n_groups = 2,
              size = c(10000, 10000),
              name = c("A", "B"),
              Mcol = Mcol)
