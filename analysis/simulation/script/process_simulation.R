process_simulation <- function(sim, peak_coeffs, scenario) {
    peaks <- o2groups:::get_peak(sim)

    # iterate over each peak coefficient
    simulation_results <- purrr::map(peak_coeffs, function(peak_coeff) {
        # iterate over each group
        group_result <- purrr::map(names(peaks), function(group) {
            d_group <- o2groups::early_delta(
                data = sim,
                min_t = 0,
                max_t = peak_coeff * peaks[[group]],
                size = scenario$size,
                name = scenario$name
            )

            if (group %in% rownames(d_group)) {
                d_group <- d_group[group, ]
                d_group <- data.frame(
                    group = group,
                    peak_coeff = peak_coeff,
                    t(d_group)
                )
                return(d_group)
            }
            return(NULL)
        }) %>% purrr::list_rbind()
    }) %>% purrr::list_rbind()
    if (nrow(simulation_results) > 0) {
        simulation_results$scenario <- scenario$scenario
    }
    return(simulation_results)
}
