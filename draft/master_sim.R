
# Libraries -----------------------------------------------------------------------------------
require(tidyverse)
require(o2groups)
require(parallel)
require(purrr)
require(furrr)
require(patchwork)
plan(multisession, workers = future::availableCores()[[1]] - 2)
pal = c("A" = "magenta", "B" = "orange", "C" = "forestgreen", "D" = "steelblue")
gg_col <- list(
  scale_color_manual(values = pal),
  scale_fill_manual(values = pal)
)
gg_scale <- list(scale_x_continuous(breaks = seq(0,100,10),
                                    limits = c(0, NA)))



# Params --------------------------------------------------------------------------------------
n_simulations = 100
n_cores = future::availableCores()[[1]] - 2
duration = 100
n_groups = 2
size = c(100, 100) #c(300, 300)
name = c("A", "B")
delta = c(10, 2)
intro_group = c("A", "B")
intro_n = c(1, 1)
r0 = c(3, 3)
generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)
incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)

set.seed(123)
sim <-
  o2groups::simulate_groups_furrr(
    n_simulations = n_simulations,
    n_cores = n_cores,
    duration = duration,
    n_groups = n_groups,
    size = size,
    name = name,
    delta = delta,
    intro_group = intro_group,
    intro_n = intro_n,
    r0 = r0 ,
    generation_time = generation_time$d(1:30),
    stack = FALSE
  )

sim <- furrr::future_map(
  .x = 1:n_simulations,
  ~ {
    sim[[.x]][["data"]]["date_onset"] <-
      sim[[.x]][["data"]]["date_infection"] + incubation_period$r(nrow(sim[[.x]][["data"]]))

    sim[[.x]][["data"]] <-
      sim[[.x]][["data"]] %>%
      simulacr::get_si() %>%
      simulacr::get_gt() %>%
      o2groups::get_Ri() %>%
      arrange(date_infection)

    return(sim[[.x]][["data"]])
  },
  .options = furrr::furrr_options(seed = TRUE),
  verbose = FALSE
)

# SUSCEPTIBLES --------------------------------------------------------------------------------
prop_sus <- lapply(sim, function(df) {
  lapply(sort(unique(df$date_onset)), function(date) {
    tibble(
      prop_sus =
        get_prop_susceptibles(
          dat = df,
          t = date,
          n_groups = n_groups,
          size = size,
          name = name
        ),
      group = name,
      date_onset = as.integer(date)
    )
  }) %>%
    bind_rows()
}) %>% bind_rows(.id = "sim")

prop_sus_avg <- prop_sus %>%
  group_by(group, date_onset) %>%
  summarise(avg = mean(prop_sus))


p_sus <- prop_sus %>%
  ggplot(aes(
    x = date_onset,
    y = prop_sus,
    col = group,
    group = interaction(group, sim)
  )) +
  geom_line() +
  geom_line(data = prop_sus_avg,
            aes(y = avg, group = group),
            col = "black") +
  gg_scale +
  gg_col +
  theme_bw()
p_sus

# Ri ----------------------------------------------------------------------
source("R/plot_Ri.R")

Ri_list <- furrr::future_map(.x = sim,
                             ~ Ri_data(.x, name))

Ri_group <- bind_rows(map(Ri_list, ~ .x$Ri_group), .id = "sim") %>%
  group_by(group, date_onset) %>%
  summarise(lower_quantile = quantile(Ri, 0.025),
            upper_quantile = quantile(Ri, 0.975),
            Ri = mean(Ri))
Ri_facet <-
  bind_rows(map(Ri_list, ~ .x$Ri_facet), .id = "sim") %>%
  group_by(group, target, date_onset) %>%
  summarise(lower_quantile = quantile(Ri, 0.025),
            upper_quantile = quantile(Ri, 0.975),
            Ri = mean(Ri))

(
  plot_Ri_facet(Ri_facet) +
    geom_ribbon(
      data = Ri_facet,
      aes(
        x = date_onset,
        y = Ri,
        ymin = lower_quantile,
        ymax = upper_quantile,
        fill = group
      ),
      col = "white",
      alpha = 0.2
    )
  +
    plot_Ri_group(Ri_group) +
    geom_ribbon(
      data = Ri_group,
      aes(
        x = date_onset,
        y = Ri,
        ymin = lower_quantile,
        ymax = upper_quantile,
        fill = group
      ),
      col = NA,
      alpha = 0.2
    )

) + plot_layout(ncol = 1)

# MIXING ------------------------------------------------------------------
source("R/plot_mix_window.R")

mix_windows <- furrr::future_map(.x = sim,
                                 ~ mix_window_data(.x, 5)) %>%
  bind_rows(.id = "sim") %>%
  group_by(window,t_start, t_end, source_group, group) %>%
  summarise(est = mean(est),
            lower_ci = mean(lower_ci),
            upper_ci  = mean(upper_ci),
            freq = mean(freq),
            n = round(mean(n)))

plot_mix_window(mix_window_data = mix_windows, 5)


# DELTA ------------------------------------------------------------------

## early phase:
mint = 0
maxt = 22


early_delta <- map(
  sim,
  ~ early_delta(
    data = .x,
    min_t = mint,
    max_t = maxt,
    size = size,
    name = name
  ) %>% as_tibble(., rownames = "group")
) %>% bind_rows(.id = "sim")

#mean estimates across the simulations
early_delta %>%
  filter(est < Inf) %>%
  group_by(group) %>%
  summarise(est = mean(est),
            low = mean(lower_ci),
            high = mean(upper_ci),
            successes = mean(successes),
            trials = mean(trials))

names(delta) <- name
#coverage
early_delta %>%
  group_by(group) %>%
  mutate(is_within_ci = delta[group] >= lower_ci &
           delta[group] <= upper_ci) %>%
  summarize(proportion_within_ci = mean(is_within_ci, na.rm = TRUE))

early_delta %>%
  filter(est < Inf) %>%
  ggplot(aes(x = as.integer(sim), y = est))+
  geom_point(aes(col = group))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci,
                  fill = group), alpha = 0.4)



# Peak Date ---------------------------------------------------------------
peak_date <- function(data) {
  apply(with(data, table(group, date_onset)), 1, function(x) {
    dates <- as.numeric(names(x))
    peak <- dates[which.max(x)]
    return(peak)
  })
}

library(incidence)
i <- incidence::incidence(dates = sim[[1]]$date_onset,
          groups = sim[[1]]$group)
plot(i)
incidence::estimate_peak(i$counts[, "B"])


i <- incidence::incidence(dates = sim[[1]]$date_onset[sim[[1]]$group == "A"])
plot(i)
incidence::estimate_peak(i)


# Find a threshold timepoint for early delta ----------------------------------------------------------------------------

best_t <- furrr::future_map(sim,
    ~ o2groups:::find_best_threshold(delta = delta,
                          data = .x,
                          n_groups,
                          size,
                          name)) %>%
  bind_rows(.id = "sim") %>%
  filter(est < Inf)

#find the prop_sus for each group that results in
# the smallest diff with the narrowest possible confidence interval across all simulations
best_t %>%
  group_by(group, sim) %>%
  filter(!is.infinite(diff)) %>%
  summarise(prop_sus = prop_sus[which.min(diff)],
            lower_ci = min(lower_ci[which.min(diff)]),
            upper_ci = max(upper_ci[which.min(diff)]),
            t = t[which.min(diff)]) %>%
  group_by(group) %>%
  summarize(prop_sus = mean(prop_sus),
            t = mean(t))


#locally weighted scatterplot smoothing
#LOESS fits a series of weighted local regressions
#to different subsets of the data to create a smooth line.
loess_fits <- best_t %>%
  group_by(group) %>%
  nest() %>%
  mutate(
    loess_fit = map(data, ~ loess(est ~ t, data = .x, span = 0.7)),
    predicted_data = map2(data, loess_fit,
                          ~ data.frame(t = .x$t, est = predict(.y)))
  ) %>%
  select(group, predicted_data) %>%
  unnest(predicted_data)

rolling_means <- best_t %>%
  group_by(group, t) %>%
  summarize(delta_avg = mean(est, na.rm = TRUE))

p_delta_t <-
  best_t %>%
  ggplot(aes(
    x = t,
    y = est,
    col = group,
    group = group
  )) +
  geom_point(alpha = 0.4) +
  geom_ribbon(
    data = loess_fits,
    aes(ymin = est - 0.01, ymax = est + 0.01),
    size = 1.1,
    color = "black"
  ) +
  geom_line(data = loess_fits,
            size = 1) +
  geom_hline(yintercept = delta, aes(col = group)) +
  geom_line(data = rolling_means ,
            aes(y = delta_avg),
            col = "black") +
  geom_hline(yintercept = delta, aes(col = group)) +
  scale_y_continuous(breaks = seq(0, 100, 5), limits = c(0, 40)) +
  theme_bw() +
  gg_col +
  gg_scale

p_delta_t
p_delta_t / p_sus
