library(igraph)
library(tictoc)
library(tidyverse)
library(o2groups)

# Parameters --------------------------------------------------------------

duration = 100
n_groups = 2
size = c(10, 10) #c(300, 300)
name = c("A", "B")
delta = c(6, 1)
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
data = sim$data %>%
  rename(from = source,
         to = id) %>%
  select(group, from, to)

str(data)

# GRAPH ---------------------------------------------------------------------------------------
library(ggraph)
library(igraph)
library(dplyr)


data_edges = data %>% drop_na(from) %>% select(from, to)
data_vertices = data %>% select(to, group)

g <- igraph::graph_from_data_frame(d = data_edges,
                                   vertices = data_vertices,
                                   directed = TRUE)

igraph::assortativity_nominal(
  g,
  as.integer(as.factor(V(g)$group))
)

igraph::similarity(g, method = "invlogweighted")

