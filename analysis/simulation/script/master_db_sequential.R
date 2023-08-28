time_start <- format(Sys.time(), "%Y-%m-%d-%H:%M")
library(DBI)
library(RSQLite)
library(randomNames)
library(here)


# 0. Parameters --------------------------------------------------------------------------------------------
n_scenarios <- 3000
n_simulations <- 50
peak_coeffs <- seq(0.7 , 1, 0.1)
rseed <- sample(100000:999999, 1)


# 1. Database Set-up --------------------------------------------------------------------------------------------
output_dir <- here("analysis/simulation/data")
db_conn <-
  dbConnect(RSQLite::SQLite(), dbname = paste0(output_dir, "/database.db"))

# 2. Generate scenarios --------------------------------------------------------------------------------------------
source("analysis/simulation/script/generate_scenarios.R")
scenarios <- generate_scenarios(n_scenarios, seed = rseed)
saveRDS(scenarios, paste0(output_dir, "/scenarios.rds"))

# 3. Processing -----------------------------------------------------------------------
process_time <- system.time({
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]

    simulation <- o2groups::simulate_groups(
      duration = 100,
      n_groups = scenario$n_groups,
      size = scenario$size,
      name = scenario$name,
      delta = scenario$delta,
      intro_n = scenario$intro_n,
      r0 = scenario$r0,
      generation_time = scenario$generation_time$d(0:50),
      incubation_period = scenario$incubation_period$r(500)
    )$data
    simulation$scenario <- scenario$scenario

    observed_groups <- sort(unique(simulation$group))
    peaks <- lapply(observed_groups, function(g) {
      incid <- incidence::incidence(dates = simulation$date_onset[simulation$group == g])
      peak <- incidence::estimate_peak(incid)
      return(round(peak$estimated))
    })
    names(peaks) <- sort(observed_groups)

    # For each group, estimate delta at the group's respective peak
    delta_results <- data.frame(
      simulation = integer(), group = character(),
      est = numeric(), lower_ci = numeric(), upper_ci = numeric(),
      successes = numeric(), trials = numeric(), stringsAsFactors = FALSE
    )
    for (group in observed_groups) {
      d_group <- o2groups::early_delta(
        data = simulation, min_t = 0,
        max_t = peak_coeff * peaks[[group]], size = scenario$size,
        name = scenario$name
      )
      #keep only the row of the current group
      d_group <- d_group[group, ]
      d_group$group <- rownames(d_group)



    }

    # process the results from the simulations with different peak coefficients using base R
    # use base r
    result <- data.frame()
    for (j in seq_along(peak_coeffs)) {
      temp_result <- o2groups::process_simulation(simulation, peak_coeffs[j], scenario$size, scenario$name) %>%
        mutate(scenario = scenario$scenario, peak_coeff = peak_coeffs[j])
      result <- rbind(result, temp_result)
    }

    dbWriteTable(db_conn, "simulations", simulation, append = TRUE)
    dbWriteTable(db_conn, "results", result, append = TRUE)
  }

  rm(list = c("scenario", "simulation"))
})

dbDisconnect(db_conn)

# 4. Save the run details --------------------------------------------------------------------------------------------
time_end <- format(Sys.time(), "%Y-%m-%d-%H:%M")

# Convert to POSIXct datetime objects
time_start <- as.POSIXct(time_start, format = "%Y-%m-%d-%H:%M")
time_end <- as.POSIXct(time_end, format = "%Y-%m-%d-%H:%M")
totaltime <- time_end - time_start

run_details <- data.frame(
  rseed = rseed,
  time_start = as.character(time_start),
  time_end = as.character(time_end),
  totaltime = as.numeric(totaltime),
  n_scenarios = n_scenarios,
  peak_coeffs = paste(peak_coeffs, collapse = ", "),
  process_time = process_time[[3]]
)
saveRDS(run_details,
        file = paste0(output_dir, "/run_details_", Sys.Date(), ".rds"))



# Query ---------------------------------------------------------------------------------------

# # Example SQL query
# query <- "SELECT * FROM results"
#
# # Execute the query and retrieve the results as a data frame
# results <- dbGetQuery(db_conn, query)