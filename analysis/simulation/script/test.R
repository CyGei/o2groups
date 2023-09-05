library(testthat)
library(dplyr)
library(furrr)
library(purrr)
library(skimr)
plan(multisession, workers = future::availableCores()[[1]] - 2)

read_files <- function(path) {
  files <- list.files(path = path, pattern = "*.rds", full.names = TRUE)
  furrr::future_map(files, readRDS, .options = furrr_options(seed = NULL))
}

scenarios <- read_files(here("analysis/simulation/data", "scenarios"))
results <- read_files(here("analysis/simulation/data", "results"))



# n_groups ----------------------------------------------------------------
check.n_groups <- function(scenarios, results, n_sample = NULL) {
  if (!is.null(n_sample)) {
    sampled_scenarios <- sample(length(scenarios), n_sample)
    scenarios <- scenarios[sampled_scenarios]
    results <- results[sampled_scenarios]
  }

  expect_equal(
    map_dbl(scenarios, ~ .x$n_groups),
    map_dbl(results, ~ skim_without_charts(.x) %>%
      filter(skim_variable == "name") %>%
      pull(character.n_unique)),
    info = "n_groups in scenario does not equal n_groups in results"
  )
}

test_that("Check n_groups in sampled scenarios corresponds to n_groups in results", {
  check.n_groups(scenarios, results, n_sample = 5)
})



# skim output -------------------------------------------------------------

check.skim_output <- function(scenarios, results, n_sample = NULL) {
  n_scenarios <- length(scenarios)

  if (!is.null(n_sample)) {
    sampled_indices <- sample(1:n_scenarios, n_sample)
    scenarios <- scenarios[sampled_indices]
    results <- results[sampled_indices]
  }

  for (i in 1:length(scenarios)) {
    skim <- skim_without_charts(results[[i]]) %>%
      as.data.frame() %>%
      filter(skim_variable == "name")

    expect_equal(
      skim$character.whitespace,
      0,
      info = "Whitespace count for 'name' is not equal to 0"
    )

    expect_equal(
      skim$character.empty,
      0,
      info = "Empty count for 'name' is not equal to 0"
    )

    expect_equal(
      skim$complete_rate,
      1,
      info = "Complete rate for 'name' is not equal to 1"
    )
  }
}

test_that("Check skim output for whitespace, empties, and complete rate", {
  check.skim_output(scenarios, results, n_sample = 5) # Adjust the number of samples as needed
})

rm(list = c("check.n_groups", "check.skim_output"))
