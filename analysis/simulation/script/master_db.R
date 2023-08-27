library(DBI)
library(RSQLite)
library(randomNames)

# 0. Parameters --------------------------------------------------------------------------------------------
n_scenarios <- 3
peak_coeffs <- seq(0.7 , 1, 0.1)

# 1. Folder set-up --------------------------------------------------------------------------------------------
folder_names <- list.dirs("analysis/simulation/data",
                          recursive = FALSE,
                          full.names = FALSE
)
run_name <- randomNames::randomNames(1, gender = 1, ethnicity = 6, which.names = "first")
while (run_name %in% folder_names) {
  run_name <- randomNames::randomNames(1, gender = 1, ethnicity = 6, which.names = "first")
}
output_dir <- paste0("analysis/simulation/data/", run_name)
dir.create(output_dir, recursive = TRUE)

time_start <- format(Sys.time(), "%Y-%m-%d-%H:%M")

# 2. Generate scenarios --------------------------------------------------------------------------------------------
source("analysis/simulation/script/generate_scenarios.R")
scenarios <- generate_scenarios(n_scenarios)
saveRDS(scenarios, paste0(output_dir, "/scenarios.rds"))

# 3. Run simulations -----------------------------------------------------------------------
source("analysis/simulation/script/run_simulations.R")
# simulations
simulations_time <- system.time({
  simulations <- run_simulations(scenarios)
})
saveRDS(simulations, file = paste0(output_dir, "/simulations.rds"))
saveRDS(simulations_time, file = paste0(output_dir, "/simulations_time.rds"))

# 4. Process simulations --------------------------------------------------------------------------------------------
source("analysis/simulation/script/process_simulations.R")

results_time <- system.time({
  results <- process_simulations(scenarios,simulations, peak_coeffs)
})

saveRDS(results, file = paste0(output_dir, "/results.rds"))
saveRDS(results_time, file = paste0(output_dir, "/results_time.rds"))


# 4. Model Delta Bias --------------------------------------------------------------------------------------------
#source("simulation/script/model.R")

# 5. Save the run details --------------------------------------------------------------------------------------------
time_end <- format(Sys.time(), "%Y-%m-%d-%H:%M")

# Convert to POSIXct datetime objects
time_start <- as.POSIXct(time_start, format = "%Y-%m-%d-%H:%M")
time_end <- as.POSIXct(time_end, format = "%Y-%m-%d-%H:%M")

# Calculate the time difference
totaltime <- time_end - time_start

run_details <- data.frame(
  run_name = run_name,
  time_start = as.character(time_start),
  time_end = as.character(time_end),
  totaltime = as.numeric(totaltime),
  n_scenarios = n_scenarios,
  peak_coeffs = paste(peak_coeffs, collapse = ", "),
  simulations_time = simulations_time[[3]],
  results_time = results_time[[3]]
)
saveRDS(run_details, file = paste0(output_dir, "/run_details.rds"))

