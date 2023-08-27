library(tidyverse)
library(o2groups)
set.seed(123)
out <-
  simulate_groups_furrr(
    n_simulations = 100,
    n_cores = parallel::detectCores() - 2,
    duration = 100,
    n_groups = 4,
    size = c(1000, 1000, 1000, 1000),
    name = c("A", "B", "C", "D"),
    delta = c(10, 2, 1, 4),
    intro_group = c("A", "B", "C", "D"),
    intro_n = c(5, 5, 5, 5),
    r0 = c(3, 3, 2, 3.5),
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)$d(1:30),
    incubation_period = simulacr::make_disc_gamma(mean = 5, sd = 2)$r(1000)
  )

data = out$data
stats = out$stats

# STATS ---------------------------------------------------------------------
pal = c("A" = "magenta", "B" = "orange", "C" = "forestgreen", "D" = "steelblue")
gg_col <- list(
  scale_color_manual(values = pal),
  scale_fill_manual(values = pal)
)
gg_scale <- list(scale_x_continuous(breaks = seq(0,100,10)))


plot_tree(subset(data, simulation == 1), pal = pal)
plot_stats(stats)[[1]] + gg_col + gg_scale
plot_stats(stats)[[2]] + gg_col  + gg_scale



# Ri ----------------------------------------------------------------------
p_list <-
  map(as.integer(unique(data$simulation)),
      ~ plot_Ri(
        data = data[data$simulation == .x, ] ,
        name = LETTERS[1:4],
        r0 = c(3, 3, 2, 3.5)
      ))

facet_dat <- map(1:length(p_list),
    ~ p_list[[.x]]$patches[["plots"]][[1]][["data"]]
    ) %>%
  bind_rows(., .id = "simulation")

group_dat <- map(1:length(p_list),
                 ~ p_list[[.x]]$data
) %>% bind_rows(., .id = "simulation")

a <-
  ggplot(facet_dat,
       aes(
  x = date_onset,
  y = Ri,
  col = group
)) +
  facet_grid(target ~ group,  scales = "free_y") +
  geom_point() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )



b <-
  ggplot(group_dat,
         aes(x = date_onset,
             y = Ri,
             col = group)) +
  facet_grid( ~ group, scales = "free_y") +
  geom_point() +
  geom_hline(
    data = tibble(group = LETTERS[1:4], r0 = c(3, 3, 2, 3.5)),
    aes(
      yintercept = r0,
      col = group,
      group = group
    ),
    lty = "dotted"
  ) +
  theme_bw() +
  theme(strip.text.x = element_blank(),
        legend.position = "none") +
  labs(y = "")

patchwork::wrap_plots(a,
                      b,
                      ncol = 1,
                      guides = "collect",
                      heights = c(3, 1))

# MIXING ---------------------------------------------------------------------
mix_t <- map(.x = unique(data$simulation),
             function(sim){
               sim_data <- filter(data, simulation == sim)
               map(.x = unique(sim_data$date_onset),
                   function(date){
                     mix <- sim_data %>% get_mixing(., min_t = date, max_t = date)
                     return(mix$result)
                   }) %>%
                 bind_rows(., .id = "t") %>%
                 mutate(t = as.integer(t))
             }) %>%
  bind_rows(., .id = "simulation") %>%
  mutate(across(c(source_group, group),
                ~factor(., levels = LETTERS[1:4])))

mean_freqs <- mix_t %>%
  group_by(t, source_group, group) %>%
  summarise(freq = mean(freq))

Mcol_df <- mix_t %>%
  filter(t <=20 & t >= 10) %>%
  group_by(source_group, group) %>%
  summarise(freq = mean(freq))

Truth <- generate_Mcol( n_groups = 4,
                        size = c(10000, 10000, 10000, 1000),
                        name = c("A", "B", "C", "D"),
                        delta = c(10, 2, 1, 4)) %>%
  as.data.frame(.) %>%
  rownames_to_column(var = "group") %>%
  pivot_longer(-group, names_to = "source_group", values_to = "value") %>%
  select(source_group, group, value) %>%
  arrange(source_group)


ggplot(mix_t) +
  aes(x = t,
      y = freq ,
      col = source_group) +
  facet_grid(group ~ source_group) +
  geom_point() +
  geom_vline(aes(xintercept = 20), lty = "dashed") +
  geom_line(data = mean_freqs, col = "#4C4E52")+
  geom_segment(data = Mcol_df, aes(x = 10, xend = 20, y = freq, yend = freq),
               size = 1, col = "black")+
  geom_point(data = Mcol_df, aes(median(10:20), y = freq),
             size = 2, col = "black")+
  geom_hline(data = Truth,
             aes(yintercept = value), lty = "dotted")+
  theme_bw()+
  theme(legend.position = "none")+
  gg_col +
  gg_scale



# DELTA -------------------------------------------------------------------

#early phase
Mcol <- get_mixing(data, min_t = 5, max_t = 12)$Mcol

Mcol_to_delta(n_groups = 4,
              size = c(10000, 10000, 10000, 1000),
              name = c("A", "B", "C", "D"),
              Mcol = Mcol) %>% round(., digits = 1)




# At any timepoint

timepoint = 15

Mcolt_observed <- get_mixing(data, min_t = timepoint, max_t = timepoint)$Mcol

prop_sus <- stats %>%
  filter(time  == timepoint) %>%
  group_by(group) %>%
  summarise(prop_susceptible = mean(prop_susceptible)) %>%
  pull(prop_susceptible)

Mcolt_to_delta(
  n_groups = 4,
  size = c(10000, 10000, 10000, 1000),
  name = c("A", "B", "C", "D"),
  r0 = c(3, 3, 2, 3.5),
  prop_susceptible = prop_sus,
  Mcolt_observed = Mcolt_observed
) %>%
  round(., digits = 1)


#Plot optim
timepoints <- 1:100
output <- matrix(NA, nrow = length(timepoints), ncol = 4)

for (tp in timepoints) {
  Mcolt_observed <- get_mixing(data, min_t = tp, max_t = tp)$Mcol

  prop_sus <- stats %>%
    filter(time == tp) %>%
    group_by(group) %>%
    summarise(prop_susceptible = mean(prop_susceptible)) %>%
    pull(prop_susceptible)

  delta_optim <- get_delta_optim(
    n_groups = 4,
    size = c(10000, 10000, 10000, 1000),
    name = c("A", "B", "C", "D"),
    r0 = c(3, 3, 2, 3.5),
    prop_susceptible = prop_sus,
    mixMat = Mcolt_observed
  )

  output[tp, ] <- delta_optim

}
colnames(output) <- c("A", "B", "C", "D")

p2 = output %>%
  as_tibble() %>%
  mutate(timepoint = row_number()) %>%
  filter(timepoint <=50) %>%
  pivot_longer(cols = -timepoint, names_to = "group", values_to = "delta") %>%
  ggplot(aes(x = timepoint, y = delta, col = group))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept = 10))+
  geom_hline(aes(yintercept = 4))+
  geom_hline(aes(yintercept = 2))+
  geom_hline(aes(yintercept = 1))+
  gg_col+
  theme_bw()

p2
p1/p2

