
# OUTBREKER ---------------------------------------------------------------
library(ape)

row.names(sequences) <- data$id
dna = ape::as.DNAbin(as.alignment(sequences))
tree <- nj(dist.dna(dna, model = "N"))
tree <- root(tree, 1)
plot(tree, main = "Neighbour Joining tree")
axisPhylo()
ade4::table.paint(
  as.matrix(dist.dna(dna, model = "N"))
)

config <- outbreaker2::create_config(
  move_kappa = FALSE,# do not look for missing cases
  move_pi = FALSE,  # reporting rate
  move_mu = TRUE, # mutation rate
  prior_mu = 0.01,
  init_kappa = 1,# number of generations before the last sampled ancestor
  init_pi = 1, # 100% of reporting rate = all cases are reported
  find_import = TRUE,# imported cases,
  init_tree = "star",
  pb = FALSE)

timing <- system.time({
  out <- outbreaker2::outbreaker(
    data = list(
      ids = data$id,
      dates = data$date_onset,
      w_dens = generation_time$d(1:30),
      f_dens = incubation_period$d(1:30),
      dna = dna
    ),
    config = config
  )
})
timing["elapsed"]

# no dna:
#~3min for 187 cases #~8min for 553 cases
# with dna:
#~4min for 187 cases # for 553 cases

#bad
plot(out)
plot(out, "post", burnin = 500)
plot(out, "mu",  burnin = 500) #not good?
plot(out, type = "alpha", burnin = 500)

saveRDS(out, "draft/out187cases.RData")


out_df <- out %>%
  select(step, starts_with("alpha_")) %>%
  filter(step >= 500) %>%
  pivot_longer(cols = -step,
               names_to = "alpha",
               values_to = "source") %>%
  mutate(id = data$id[as.numeric(sub("alpha_", "", alpha))],
         source = data$id[source],
         group = data$group[as.numeric(sub("alpha_", "", alpha))],
         date_onset = data$date_onset[as.numeric(sub("alpha_", "", alpha))],
         source_group = data$group[match(source, data$id)]) %>%
  select(step, id, group, source, source_group, date_onset)

out_list <-  out_df %>%
  group_split(step) %>%
  map(~get_Ri(.x))


# RI

source("R/plot_Ri.R")

Ri_list <- furrr::future_map(.x = out_list,
                             ~ Ri_data(.x, name))

Ri_group <- bind_rows(map(Ri_list, ~ .x$Ri_group), .id = "step") %>%
  group_by(group, date_onset) %>%
  summarise(lower_quantile = quantile(Ri, 0.025),
            upper_quantile = quantile(Ri, 0.975),
            Ri = mean(Ri))
Ri_facet <-
  bind_rows(map(Ri_list, ~ .x$Ri_facet), .id = "step") %>%
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

mix_windows <- furrr::future_map(.x = out_list,
                                 ~ mix_window_data(.x, 5)) %>%
  bind_rows(.id = "step") %>%
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
maxt = 16


early_delta <- map(
  out_list,
  ~ early_delta(
    data = .x,
    min_t = mint,
    max_t = maxt,
    size = size,
    name = name
  ) %>% as_tibble(., rownames = "group")
) %>% bind_rows(.id = "step")

#mean estimates across the simulations
early_delta %>%
  filter(est < Inf) %>%
  group_by(group) %>%
  summarise(est = mean(est),
            low = mean(lower_ci),
            high = mean(upper_ci),
            successes = mean(successes),
            trials = mean(trials))


early_delta %>%
  filter(est < Inf) %>%
  ggplot(aes(x = as.integer(step), y = est))+
  geom_point(aes(col = group))+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci,
                  fill = group), alpha = 0.4)


#Plot optim
timepoints <- 1:100
output <- matrix(NA, nrow = length(timepoints), ncol = n_groups)

for (tp in timepoints) {
  Mcolt_observed <- get_mixing(out_df, min_t = tp, max_t = tp)$Mcol

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
colnames(output) <- LETTERS[1:n_groups]

output %>%
  as_tibble() %>%
  mutate(timepoint = row_number()) %>%
  filter(timepoint <=50) %>%
  pivot_longer(cols = -timepoint, names_to = "group", values_to = "delta") %>%
  filter(delta <= 20) %>%
  ggplot(aes(x = timepoint, y = delta, col = group))+
  geom_point()+
  geom_line()+
  geom_hline(data = tibble(group = LETTERS[1:n_groups],
                           delta = delta),
             aes(yintercept = delta, color = group))+
  geom_hline(yintercept = 1)+
  theme_bw()



m <- get_mixing_onset(data, min_t = 0, max_t = 15)$result %>%
  arrange(source_group)
m %>% filter(source_group == "A")
DescTools::MultinomCI(c(60,12))
binom.test(60, 72)

ci <- DescTools::MultinomCI(m$n)
as.matrix(ci)$est

Mcol_to_delta(2, name, size, Mcol_observed = )

