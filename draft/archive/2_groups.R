library(tictoc)
library(tidyverse)
library(o2groups)
true_delta = c(10, 2)
true_r0 = c(3,3)
out <-
  simulate_groups_furrr(
    n_simulations = 100,
    n_cores = parallel::detectCores(),
    duration = 100,
    n_groups = 2,
    size = c(10000, 10000),
    name = c("A", "B"),
    delta = true_delta,
    intro_group = c("A", "B"),
    intro_n = c(5, 5),
    r0 = true_r0 ,
    generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)$d(1:30)
  )

data = out$data
stats = out$stats

# STATS ---------------------------------------------------------------------
pal = c("A" = "magenta", "B" = "orange")
gg_col <- list(
  scale_color_manual(values = pal),
  scale_fill_manual(values = pal)
)
gg_scale <- list(scale_x_continuous(breaks = seq(0,100,5)))


plot_tree(subset(data, simulation == 1), pal = pal)
plot_stats(stats)[[1]] + gg_col + gg_scale
plot_stats(stats)[[2]] + gg_col  + gg_scale



# Ri ----------------------------------------------------------------------

# Indiv Ri
Ri <- map(.x = unique(data$simulation),
          ~ get_Ri(data[data$simulation == .x, ])) %>%
  bind_rows(., .id = "simulation")

Ri_long <- Ri %>%
  pivot_longer(cols = c("A", "B"),
               names_to = "target") %>%
  select(simulation, id, group, target, value, date_infection)

# summary Ri
Ri_sims <- Ri_long %>%
  group_by(group, target, date_infection, simulation) %>%
  summarise(Ri = mean(value))
Ri_means <- Ri_sims %>%
  group_by(group, target, date_infection) %>%
  summarise(Ri = mean(Ri))

# plot
p_facet_Ri <- ggplot(data = Ri_sims,
       aes(x = date_infection,
           y = Ri,
           col = group)) +
  facet_grid(target ~ group) +
  geom_point(alpha = 0.5) +
  geom_line(data = Ri_means,
            col = "black") +
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  gg_col +
  gg_scale



total_Ri_sims <- Ri %>%
  group_by(simulation, group, date_infection) %>%
  summarise(Ri = mean(Ri))

total_Ri_means<- Ri %>%
  group_by(group, date_infection) %>%
  summarise(Ri = mean(Ri))

p_total_Ri <- ggplot(total_Ri_sims,
       aes(x = date_infection,
           y = Ri,
           col = group,
           group = group)) +
  geom_point() +
  geom_line(data = total_Ri_means,
            col = "black") +
  geom_hline(aes(yintercept = 1), col = "#4C4E52", lty = "dotted")+
  geom_hline(aes(yintercept = true_r0[1]), col = "#4C4E52", lty = "solid")+
  facet_grid(~ group) +
  theme_bw()+
  theme( strip.text.x = element_blank(),
         legend.position = "none")+
  gg_col +
  gg_scale

patchwork::wrap_plots(p_facet_Ri,p_total_Ri, ncol = 1, guides = "collect",
                      heights = c(3,1))



# MIXING ---------------------------------------------------------------------

tic()
mix_t <- map(.x = unique(data$simulation),
             function(sim){
               sim_data <- filter(data, simulation == sim)
               map(.x = unique(sim_data$date_infection),
                   function(date){
                     mix <- sim_data %>% get_mixing(., min_t = date, max_t = date)
                     return(mix$result)
                   }) %>%
                 bind_rows(., .id = "t") %>%
                 mutate(t = as.integer(t))
             }) %>%
  bind_rows(., .id = "simulation") %>%
  mutate(across(c(source_group, group),
                ~factor(., levels = LETTERS[1:2])))
toc()

mean_freqs <- mix_t %>%
  group_by(t, source_group, group) %>%
  summarise(freq = mean(freq))


mint = 5
maxt = 15
Mcol_df <- mix_t %>%
  filter(t <= maxt & t >= mint) %>%
  group_by(source_group, group) %>%
  summarise(freq = mean(freq))

Truth <- generate_Mcol( n_groups = 2,
                        size = c(10000, 10000),
                        name = c("A", "B"),
                        delta = true_delta) %>%
  as.data.frame(.) %>%
  rownames_to_column(var = "group") %>%
  pivot_longer(-group, names_to = "source_group", values_to = "value") %>%
  select(source_group, group, value) %>%
  arrange(source_group)

p_mix <- ggplot(mix_t) +
  aes(x = t,
      y = freq ,
      col = source_group) +
  facet_grid(group ~ source_group) +
  geom_point() +
 # geom_vline(aes(xintercept = 20), lty = "dashed") +
  geom_line(data = mean_freqs, col = "#4C4E52")+
  geom_segment(data = Mcol_df, aes(x = mint, xend = maxt, y = freq, yend = freq),
               size = 1, col = "black")+
  geom_point(data = Mcol_df, aes(median(mint:maxt), y = freq),
             size = 2, col = "black")+
  geom_hline(data = Truth,
             aes(yintercept = value), lty = "dotted")+
  theme_bw()+
  theme(legend.position = "none")+
  gg_col +
  gg_scale

p_mix

# DELTA -------------------------------------------------------------------

# Early Stage

Mcol <- get_mixing(data, min_t = mint, max_t = maxt)$Mcol

Mcol_to_delta(n_groups = 2,
              size = c(10000, 10000),
              name = c("A", "B"),
              Mcol = Mcol)


# At any timepoint

timepoint = 15

Mcolt_observed <- get_mixing(data, min_t = timepoint, max_t = timepoint)$Mcol

prop_sus <- stats %>%
  filter(time  == timepoint) %>%
  group_by(group) %>%
  summarise(prop_susceptible = mean(prop_susceptible)) %>%
  pull(prop_susceptible)


get_delta_optim(
  mixMat = Mcolt_observed,
  n_groups = 2,
  size = c(10000, 10000),
  name = c("A", "B"),
  r0 = true_r0,
  prop_susceptible = prop_sus
)


#Plot optim

timepoints <- 1:100
output <- matrix(NA, nrow = length(timepoints), ncol = 2)

for (tp in timepoints) {
  Mcolt_observed <- get_mixing(data, min_t = tp, max_t = tp)$Mcol

  prop_sus <- stats %>%
    filter(time == tp) %>%
    group_by(group) %>%
    summarise(prop_susceptible = mean(prop_susceptible)) %>%
    pull(prop_susceptible)

  delta_optim <- get_delta_optim(
    mixMat = Mcolt_observed,
    n_groups = 2,
    size = c(10000, 10000),
    name = c("A", "B"),
    r0 = true_r0,
    prop_susceptible = prop_sus
  )

  output[tp, ] <- delta_optim

}
colnames(output) <- c("A", "B")
output %>%
  as_tibble() %>%
  mutate(timepoint = row_number()) %>%
  filter(timepoint <=50) %>%
  pivot_longer(cols = -timepoint, names_to = "group", values_to = "delta") %>%
  ggplot(aes(x = timepoint, y = delta, col = group))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept = true_delta[1]))+
  geom_hline(aes(yintercept = true_delta[2]))+
  theme_bw()

